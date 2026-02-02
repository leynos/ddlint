//! Shared utilities used by the specialised span scanners.
//!
//! These helpers wrap common parsing patterns such as "parse and record" so
//! individual scanners can focus on their own syntax without duplicating
//! boilerplate.

use chumsky::prelude::*;

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
