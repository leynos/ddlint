//! Shared utilities used by the specialised span scanners.
//!
//! These helpers wrap common parsing patterns such as "parse and record" so
//! individual scanners can focus on their own syntax without duplicating
//! boilerplate.

use chumsky::prelude::*;

use crate::parser::span_collector::{
    SpanCollector, parse_span as split_parse_span, skip_line as split_skip_line,
};
use crate::parser::token_stream::TokenStream;
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
    parse_and_record_split(&mut st.stream, &mut st.spans, &mut st.extra, start, parser)
}

/// Split-state form of [`parse_and_record`] for dispatch callbacks.
///
/// [`TokenStream::dispatch`] callbacks already hold the stream, so they pass
/// the stream, span vector and diagnostic vector as separate borrows. The
/// behaviour matches [`parse_and_record`]: a successful parse records its span
/// and skips to the span end, while a failed parse skips the current line;
/// diagnostics are appended either way.
pub(crate) fn parse_and_record_split<P, E>(
    stream: &mut TokenStream<'_>,
    spans: &mut Vec<Span>,
    extra: &mut E,
    start: usize,
    parser: P,
) -> (Option<Span>, Vec<Simple<SyntaxKind>>)
where
    P: Parser<SyntaxKind, Span, Error = Simple<SyntaxKind>>,
    E: Extend<Simple<SyntaxKind>>,
{
    let (res, errs) = split_parse_span(stream, parser, start);
    if let Some(ref sp) = res {
        stream.skip_until(sp.end);
        spans.push(sp.clone());
    } else {
        split_skip_line(stream);
    }
    extra.extend(errs.clone());
    (res, errs)
}
