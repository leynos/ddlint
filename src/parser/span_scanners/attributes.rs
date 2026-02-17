//! Scanner for attribute spans and placement validation.
//!
//! Detects `#[…]` attribute syntax, records attribute spans, and validates
//! that attributes precede only permitted item kinds (typedef, function,
//! relation).

use chumsky::error::Simple;

use crate::parser::span_collector::SpanCollector;
use crate::{Span, SyntaxKind};

type State<'a> = SpanCollector<'a, Vec<Simple<SyntaxKind>>>;

/// Whether the given keyword may directly begin an attributed item.
///
/// Permitted targets per spec §5.1: `Typedef`, `Function`, and
/// `RelationDecl`. Relation declarations may be prefixed with role keywords
/// (`input`, `output`, `stream`, `multiset`).
///
/// Note: `extern` is **not** included here — it requires a lookahead to
/// confirm the following keyword is `type` or `function`. That check is
/// performed separately in [`is_valid_attribute_target`].
fn is_simple_attribute_target(kind: SyntaxKind) -> bool {
    matches!(
        kind,
        SyntaxKind::K_TYPEDEF
            | SyntaxKind::K_TYPE
            | SyntaxKind::K_FUNCTION
            | SyntaxKind::K_INPUT
            | SyntaxKind::K_OUTPUT
            | SyntaxKind::K_RELATION
            | SyntaxKind::K_STREAM
            | SyntaxKind::K_MULTISET
    )
}

/// Whether the keyword following `extern` is a permitted extern-prefixed
/// attribute target (`type` or `function`).
fn is_permitted_extern_continuation(kind: SyntaxKind) -> bool {
    matches!(kind, SyntaxKind::K_TYPE | SyntaxKind::K_FUNCTION)
}

/// Check whether the stream is positioned at a valid attribute target.
///
/// For most keywords a single peek suffices. For `extern` we must peek
/// past any inline whitespace to confirm the next keyword is `type` or
/// `function` — otherwise constructs like `extern transformer` would be
/// incorrectly accepted.
fn is_valid_attribute_target(st: &State<'_>) -> bool {
    let Some(&(kind, _)) = st.stream.peek() else {
        return false;
    };
    if is_simple_attribute_target(kind) {
        return true;
    }
    if kind == SyntaxKind::K_EXTERN {
        // Peek past inline whitespace to find the keyword after `extern`.
        return st
            .stream
            .peek_after_ws_inline()
            .is_some_and(|(k, _)| is_permitted_extern_continuation(*k));
    }
    false
}

/// Consume a single `#[…]` attribute, returning its span.
///
/// The stream cursor must point at `T_HASH`. Returns `None` if the hash is
/// not followed by `T_LBRACKET` (not an attribute) or if the bracket is
/// never closed (an error is emitted instead).
fn consume_attribute(st: &mut State<'_>) -> Option<Span> {
    let (_, hash_span) = st.stream.peek()?.clone();
    let start = hash_span.start;
    st.stream.advance(); // consume T_HASH

    // Skip optional inline whitespace between # and [
    st.stream.skip_ws_inline();

    // Must be followed by T_LBRACKET to form an attribute
    let next = st.stream.peek()?;
    if next.0 != SyntaxKind::T_LBRACKET {
        return None;
    }
    st.stream.advance(); // consume T_LBRACKET

    // Consume tokens until the matching T_RBRACKET, tracking bracket depth
    let mut depth: usize = 1;
    loop {
        let Some(&(kind, ref span)) = st.stream.peek() else {
            // EOF without closing bracket
            st.extra.push(Simple::custom(
                start..st.stream.src().len(),
                "unclosed attribute bracket",
            ));
            return None;
        };

        let end = span.end;
        st.stream.advance();

        match kind {
            SyntaxKind::T_LBRACKET => depth += 1,
            SyntaxKind::T_RBRACKET => {
                depth -= 1;
                if depth == 0 {
                    return Some(start..end);
                }
            }
            _ => {}
        }
    }
}

/// Collect consecutive attribute spans and validate their placement.
///
/// After gathering one or more `#[…]` spans, peeks at the next
/// non-whitespace token to determine the target item. If the target is not
/// a permitted attribute host, emits a diagnostic.
#[expect(
    clippy::expect_used,
    reason = "attr_spans is guaranteed non-empty after the early-return guard"
)]
fn handle_hash(st: &mut State<'_>, _span: Span) {
    let mut attr_spans: Vec<Span> = Vec::new();

    // Consume the first attribute
    let Some(first) = consume_attribute(st) else {
        return;
    };
    attr_spans.push(first);

    // Consume any consecutive attributes (stacked #[a] #[b] …)
    loop {
        st.stream.skip_ws_inline();
        // Also skip newline-containing whitespace between stacked attributes
        skip_trivia_across_lines(st);

        match st.stream.peek() {
            Some(&(SyntaxKind::T_HASH, _)) => {
                if let Some(attr) = consume_attribute(st) {
                    attr_spans.push(attr);
                } else {
                    break;
                }
            }
            _ => break,
        }
    }

    // Peek at the target item keyword
    skip_trivia_across_lines(st);
    st.stream.skip_ws_inline();

    let is_valid = is_valid_attribute_target(st);

    if is_valid {
        st.spans.extend(attr_spans);
    } else {
        // SAFETY: `attr_spans` is guaranteed non-empty — we early-return
        // above if the first `consume_attribute` fails.
        let first_start = attr_spans
            .first()
            .expect("attr_spans must be non-empty after successful consume")
            .start;
        let last_end = attr_spans
            .last()
            .expect("attr_spans must be non-empty after successful consume")
            .end;
        st.extra.push(Simple::custom(
            first_start..last_end,
            "attribute not permitted on this item",
        ));
    }
}

/// Advance past whitespace and comment tokens, including those containing
/// newlines.
fn skip_trivia_across_lines(st: &mut State<'_>) {
    while let Some(&(kind, _)) = st.stream.peek() {
        if matches!(kind, SyntaxKind::T_WHITESPACE | SyntaxKind::T_COMMENT) {
            st.stream.advance();
        } else {
            break;
        }
    }
}

/// Extract attribute spans and placement errors from the token stream.
///
/// Returns `(Vec<Span>, Vec<Simple<SyntaxKind>>)` containing collected
/// attribute spans and any placement validation errors.
///
/// # Examples
///
/// ```rust,ignore
/// use ddlint::test_util::tokenize;
/// use ddlint::parser::span_scanners::attributes::collect_attribute_spans;
///
/// let src = "#[cold]\ntypedef T = u32\n";
/// let tokens = tokenize(src);
/// let (spans, errors) = collect_attribute_spans(&tokens, src);
/// assert_eq!(spans.len(), 1);
/// assert!(errors.is_empty());
/// ```
pub(crate) fn collect_attribute_spans(
    tokens: &[(SyntaxKind, Span)],
    src: &str,
) -> (Vec<Span>, Vec<Simple<SyntaxKind>>) {
    let mut st = State::new(tokens, src, Vec::new());

    token_dispatch!(st, {
        SyntaxKind::T_HASH => handle_hash,
    });

    st.into_parts()
}
