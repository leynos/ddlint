//! Scanner for `typedef` and extern type declarations.
//!
//! The functions here locate spans for type aliases and extern type
//! declarations, skipping malformed lines to keep downstream parsing aligned.

use crate::parser::span_collector::SpanCollector;
use crate::{Span, SyntaxKind};

pub(crate) fn collect_typedef_spans(tokens: &[(SyntaxKind, Span)], src: &str) -> Vec<Span> {
    type State<'a> = SpanCollector<'a, ()>;

    fn handle_typedef(st: &mut State<'_>, span: Span) {
        let start = span.start;
        st.stream.advance();
        st.push_line_span(start);
    }

    fn handle_extern(st: &mut State<'_>, span: Span) {
        let start = span.start;
        st.stream.advance();
        st.stream.skip_ws_inline();
        if st
            .stream
            .peek()
            .is_some_and(|(kind, _)| *kind == SyntaxKind::K_TYPE)
        {
            st.stream.advance();
            st.push_line_span(start);
        } else {
            st.skip_line();
        }
    }

    let mut st = State::new(tokens, src, ());

    token_dispatch!(st, {
        SyntaxKind::K_TYPEDEF => handle_typedef,
        SyntaxKind::K_EXTERN => handle_extern,
    });

    st.spans
}
