//! Scanner for `typedef` and extern type declarations.
//!
//! The functions here locate spans for type aliases and extern type
//! declarations, skipping malformed lines to keep downstream parsing aligned.

use chumsky::error::Simple;

use crate::parser::span_collector::{SpanCollector, push_line_span, skip_line};
use crate::parser::token_stream::TokenStream;
use crate::{Span, SyntaxKind};

/// Consume a `typedef` or `extern type` head and record the rest of its line.
///
/// Both keyword arms share this handler because they differ only in whether a
/// `type` keyword must follow. `extern` declarations that continue with any
/// other keyword are skipped so later declarations stay aligned.
fn handle_typedef(
    stream: &mut TokenStream<'_>,
    spans: &mut Vec<Span>,
    span: Span,
    is_extern: bool,
) {
    let start = span.start;
    stream.advance();
    if is_extern {
        stream.skip_ws_inline();
        if stream
            .peek()
            .is_none_or(|(kind, _)| *kind != SyntaxKind::K_TYPE)
        {
            skip_line(stream);
            return;
        }
        stream.advance();
    }
    push_line_span(stream, spans, start);
}

pub(crate) fn collect_typedef_spans(
    tokens: &[(SyntaxKind, Span)],
    src: &str,
) -> (Vec<Span>, Vec<Simple<SyntaxKind>>) {
    type State<'a> = SpanCollector<'a, Vec<Simple<SyntaxKind>>>;

    let mut st = State::new(tokens, src, Vec::new());

    let State {
        stream,
        spans,
        extra: _,
    } = &mut st;
    stream.dispatch(|stream, kind, span| match kind {
        SyntaxKind::K_TYPEDEF => handle_typedef(stream, spans, span, false),
        SyntaxKind::K_EXTERN => handle_typedef(stream, spans, span, true),
        _ => stream.advance(),
    });

    st.into_parts()
}
