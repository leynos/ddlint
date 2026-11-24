//! Shared utilities used by the specialised span scanners.
//!
//! These helpers wrap common parsing patterns such as "parse and record" so
//! individual scanners can focus on their own syntax without duplicating
//! boilerplate.

use chumsky::prelude::*;

use crate::{Span, SyntaxKind, parser::span_collector::SpanCollector};

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

/// Collect the spans of every statement introduced by one keyword.
///
/// The scanner walks the token stream once. Each token whose kind equals
/// `keyword` starts a candidate statement: `make_parser()` builds a fresh
/// parser, which runs from that token. Every other token is skipped one at a
/// time, so a keyword that does not begin a statement is still examined as the
/// start of the next token's statement.
///
/// The parser **must** consume its entire statement and return a `Span`
/// covering it, including any trailing inline whitespace. Recording,
/// advancement, and recovery are exactly
/// [`parse_and_record`]'s: a parsed span is recorded and the stream advances to
/// its end, a failed parse appends its diagnostics and skips the rest of the
/// line, and every diagnostic is appended to the returned vector. Success
/// therefore keeps non-terminal diagnostics that a validating parser emits.
///
/// The factory is called per match rather than once, because the production
/// grammars build non-`Clone` combinator values.
///
/// This helper owns mechanics only. It suits a scanner that matches a single
/// keyword, applies no guard to the token after it, accepts the whole parsed
/// span, and reports no diagnostics beyond its parser's. A scanner that needs
/// candidate discovery, span filtering, diagnostic replacement, or custom
/// recovery keeps its own orchestration.
///
/// # Examples
///
/// The parser sees the raw token stream, trivia included, so it must skip the
/// whitespace between tokens itself.
///
/// ```rust,ignore
/// use chumsky::prelude::*;
/// use crate::{
///     Span, SyntaxKind,
///     parser::span_scanners::utils::collect_with_parser,
/// };
///
/// let src = "type Alpha type Beta";
/// let tokens = crate::test_util::tokenize(src);
/// let ws = || {
///     filter(|kind: &SyntaxKind| *kind == SyntaxKind::T_WHITESPACE)
///         .ignored()
///         .repeated()
/// };
/// let parser = || {
///     just(SyntaxKind::K_TYPE)
///         .padded_by(ws())
///         .ignore_then(just(SyntaxKind::T_IDENT))
///         .padded_by(ws())
///         .map_with_span(|_, sp: Span| sp)
/// };
/// let (spans, errs) = collect_with_parser(&tokens, src, SyntaxKind::K_TYPE, parser);
/// assert_eq!(spans.len(), 2);
/// assert!(errs.is_empty());
/// ```
pub(super) fn collect_with_parser<P, F>(
    tokens: &[(SyntaxKind, Span)],
    src: &str,
    keyword: SyntaxKind,
    mut make_parser: F,
) -> (Vec<Span>, Vec<Simple<SyntaxKind>>)
where
    F: FnMut() -> P,
    P: Parser<SyntaxKind, Span, Error = Simple<SyntaxKind>>,
{
    let mut st: State<'_> = State::new(tokens, src, Vec::new());

    while let Some((kind, span)) = st.stream.peek().cloned() {
        if kind == keyword {
            parse_and_record(&mut st, span.start, make_parser());
        } else {
            st.stream.advance();
        }
    }

    st.into_parts()
}
