//! Shared utilities used by the specialised span scanners.
//!
//! These helpers wrap common parsing patterns such as "parse and record" so
//! individual scanners can focus on their own syntax without duplicating
//! boilerplate.

use chumsky::prelude::*;

use crate::parser::lexer_helpers::inline_ws;
use crate::parser::span_collector::SpanCollector;
use crate::{Span, SyntaxKind};

/// Convenience alias for scanners that accumulate `Simple` errors.
pub(crate) type State<'a> = SpanCollector<'a, Vec<Simple<SyntaxKind>>>;

/// Parse a statement and record its span on success.
///
/// The `parser` **must** consume the entire statement and return a `Span`
/// covering it, including any trailing inline whitespace. Any parse errors
/// produced by `parser` are appended to `st.extra`. On success the span is
/// recorded and the token stream advanced to the end of the statement;
/// otherwise the current line is skipped.
///
/// # Examples
///
/// ```rust,ignore
/// use chumsky::prelude::*;
/// use crate::{parser::span_scanners::utils::parse_and_record, SyntaxKind, Span};
/// use crate::parser::span_collector::SpanCollector;
///
/// let src = "import foo\n";
/// let tokens = crate::test_util::tokenize(src);
/// let mut st = SpanCollector::new(&tokens, src, Vec::new());
/// let ident = just(SyntaxKind::T_IDENT).map_with_span(|_, sp: Span| sp);
/// let (span, errs) = parse_and_record(&mut st, 0, ident);
/// assert!(span.is_some());
/// assert!(errs.is_empty());
/// assert_eq!(st.extra, errs);
/// ```
pub(crate) fn parse_and_record<P, E>(
    st: &mut SpanCollector<'_, E>,
    start: usize,
    parser: P,
) -> (Option<Span>, Vec<Simple<SyntaxKind>>)
where
    P: Parser<SyntaxKind, Span, Error = Simple<SyntaxKind>>,
    E: Extend<Simple<SyntaxKind>>,
{
    let (res, errs) = st.parse_span(parser, start);
    if let Some(ref sp) = res {
        st.stream.skip_until(sp.end);
        st.spans.push(sp.clone());
    } else {
        st.skip_line();
    }
    st.extra.extend(errs.clone());
    (res, errs)
}

/// Collect spans for declarations that must be prefixed by `extern`.
///
/// The helper scans the token stream for `extern` declarations, recording
/// spans and parse errors via `parse_and_record`. Any non-extern occurrence of
/// `kind` is delegated to `on_non_extern` so callers can inject additional
/// diagnostics or recovery behaviour.
pub(crate) fn collect_extern_declarations_with_rule<P, FDecl, FNonExtern>(
    tokens: &[(SyntaxKind, Span)],
    src: &str,
    kind: SyntaxKind,
    decl_parser: FDecl,
    mut on_non_extern: FNonExtern,
) -> (Vec<Span>, Vec<Simple<SyntaxKind>>)
where
    P: Parser<SyntaxKind, Span, Error = Simple<SyntaxKind>>,
    FDecl: Fn() -> P,
    FNonExtern: FnMut(&mut State<'_>, Span),
{
    let mut st: State<'_> = State::new(tokens, src, Vec::new());

    let handle_extern = |st: &mut State<'_>, span: Span| {
        let is_decl = st
            .stream
            .peek_after_ws_inline()
            .is_some_and(|(k, _)| *k == kind);
        if !is_decl {
            st.skip_line();
            return;
        }

        let ws = inline_ws().repeated();
        let start = span.start;
        let parser = just(SyntaxKind::K_EXTERN)
            .padded_by(ws.clone())
            .ignore_then(decl_parser())
            .map(move |sp: Span| start..sp.end);

        parse_and_record(st, start, parser);
    };

    while let Some(&(token_kind, ref span_ref)) = st.stream.peek() {
        let span = span_ref.clone();
        if token_kind == SyntaxKind::K_EXTERN {
            handle_extern(&mut st, span);
        } else if token_kind == kind {
            on_non_extern(&mut st, span);
        } else {
            st.stream.advance();
        }
    }

    st.into_parts()
}
