//! Scanner for attribute spans and placement validation.
//!
//! Detects `#[…]` attribute syntax, records attribute spans, and validates
//! that attributes precede only permitted item kinds (typedef, function,
//! relation).

use chumsky::error::Simple;

use crate::parser::span_collector::SpanCollector;
use crate::parser::token_stream::TokenStream;
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
fn is_valid_attribute_target(stream: &TokenStream<'_>) -> bool {
    let Some(&(kind, _)) = stream.peek() else {
        return false;
    };
    if is_simple_attribute_target(kind) {
        return true;
    }
    if kind == SyntaxKind::K_EXTERN {
        // Peek past inline whitespace to find the keyword after `extern`.
        return stream
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
fn consume_attribute(
    stream: &mut TokenStream<'_>,
    diagnostics: &mut Vec<Simple<SyntaxKind>>,
) -> Option<Span> {
    let (_, hash_span) = stream.peek()?.clone();
    let start = hash_span.start;
    stream.advance(); // consume T_HASH

    // Skip optional inline whitespace between # and [
    stream.skip_ws_inline();

    // Must be followed by T_LBRACKET to form an attribute
    let next = stream.peek()?;
    if next.0 != SyntaxKind::T_LBRACKET {
        return None;
    }
    stream.advance(); // consume T_LBRACKET

    // Consume tokens until the matching T_RBRACKET, tracking bracket depth
    let mut depth: usize = 1;
    loop {
        let Some(&(kind, ref span)) = stream.peek() else {
            // EOF without closing bracket
            diagnostics.push(Simple::custom(
                start..stream.src().len(),
                "unclosed attribute bracket",
            ));
            return None;
        };

        let end = span.end;
        stream.advance();

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
fn handle_hash(
    stream: &mut TokenStream<'_>,
    spans: &mut Vec<Span>,
    diagnostics: &mut Vec<Simple<SyntaxKind>>,
) {
    let mut attr_spans: Vec<Span> = Vec::new();

    // Consume the first attribute
    let Some(first) = consume_attribute(stream, diagnostics) else {
        return;
    };
    let first_start = first.start;
    attr_spans.push(first);

    consume_stacked_attributes(stream, diagnostics, &mut attr_spans);

    // Peek at the target item keyword
    skip_trivia_across_lines(stream);
    stream.skip_ws_inline();

    if is_valid_attribute_target(stream) {
        spans.extend(attr_spans);
    } else {
        // `attr_spans` is guaranteed non-empty — the first attribute is
        // pushed above — so the fallback end is never used in practice.
        let last_end = attr_spans.last().map_or(first_start, |span| span.end);
        diagnostics.push(Simple::custom(
            first_start..last_end,
            "attribute not permitted on this item",
        ));
    }
}

/// Consume any consecutive attributes following the first one.
///
/// Stacked attributes may be separated by inline trivia or by trivia spanning
/// newlines, so both forms are skipped before each lookahead.
fn consume_stacked_attributes(
    stream: &mut TokenStream<'_>,
    diagnostics: &mut Vec<Simple<SyntaxKind>>,
    attr_spans: &mut Vec<Span>,
) {
    loop {
        stream.skip_ws_inline();
        // Also skip newline-containing whitespace between stacked attributes
        skip_trivia_across_lines(stream);

        match stream.peek() {
            Some(&(SyntaxKind::T_HASH, _)) => {
                if let Some(attr) = consume_attribute(stream, diagnostics) {
                    attr_spans.push(attr);
                } else {
                    break;
                }
            }
            _ => break,
        }
    }
}

/// Advance past whitespace and comment tokens, including those containing
/// newlines.
fn skip_trivia_across_lines(stream: &mut TokenStream<'_>) {
    while let Some(&(kind, _)) = stream.peek() {
        if matches!(kind, SyntaxKind::T_WHITESPACE | SyntaxKind::T_COMMENT) {
            stream.advance();
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

    let State {
        stream,
        spans,
        extra,
    } = &mut st;
    stream.dispatch(|stream, kind, _span| match kind {
        SyntaxKind::T_HASH => handle_hash(stream, spans, extra),
        _ => stream.advance(),
    });

    st.into_parts()
}
