//! Span collection for supported type declarations.
//!
//! The scanner collects `type` and `extern type` spans for downstream parsing,
//! ending each span at a top-level semicolon or newline while preserving
//! newlines inside balanced groups. It skips legacy `typedef` and `extern`
//! forms without `type`; `reserved_tokens` owns the compatibility diagnostic
//! for the legacy keyword.

use chumsky::error::Simple;

use crate::{Span, SyntaxKind};

use super::indexes_support::{TypeDepths, adjust_type_depths, is_trivia};
use super::utils::State;

/// Keep balanced groups inside a type span, stopping at a top-level terminator.
fn push_type_span(st: &mut State<'_>, start: usize) {
    let mut depths = TypeDepths::default();
    let mut end = st.stream.src().len();
    while let Some((kind, span)) = st.stream.peek().cloned() {
        let text = st.stream.src().get(span.clone()).unwrap_or_default();
        let terminates = depths.at_top_level()
            && (kind == SyntaxKind::T_SEMI || (is_trivia(kind) && text.contains('\n')));
        adjust_type_depths(kind, &mut depths);
        end = span.end;
        st.stream.advance();
        if terminates {
            break;
        }
    }
    st.spans.push(start..end);
}

/// Collect complete `type` and `extern type` declarations.
///
/// Spans end at a top-level semicolon or newline; newlines inside balanced
/// groups remain part of the declaration. The scanner skips legacy `typedef`
/// declarations and `extern` forms without `type`. The reserved-token parser
/// policy, not this span scanner, reports legacy syntax.
pub(crate) fn collect_typedef_spans(
    tokens: &[(SyntaxKind, Span)],
    src: &str,
) -> (Vec<Span>, Vec<Simple<SyntaxKind>>) {
    /// Start a span at a supported `type` declaration.
    fn handle_type(st: &mut State<'_>, span: Span) {
        let start = span.start;
        st.stream.advance();
        push_type_span(st, start);
    }

    /// Skip legacy `typedef`; its compatibility error is emitted centrally.
    fn handle_typedef(st: &mut State<'_>, _span: Span) {
        st.skip_line();
    }

    /// Collect an `extern type` span, or skip an `extern` form without `type`.
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
            push_type_span(st, start);
        } else {
            st.skip_line();
        }
    }

    let mut st = State::new(tokens, src, Vec::new());

    token_dispatch!(st, {
        SyntaxKind::K_TYPEDEF => handle_typedef,
        SyntaxKind::K_TYPE => handle_type,
        SyntaxKind::K_EXTERN => handle_extern,
    });

    st.into_parts()
}
