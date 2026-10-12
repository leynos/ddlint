//! Span collection for supported type declarations.
//!
//! The scanner collects `type` and `extern type` spans for downstream parsing,
//! ending each span at a top-level semicolon or newline while preserving
//! newlines inside balanced groups. It skips legacy `typedef` and `extern`
//! forms without `type`; `reserved_tokens` owns the compatibility diagnostic
//! for the legacy keyword.

use chumsky::prelude::*;

use crate::parser::lexer_helpers::balanced_block;
use crate::{Span, SyntaxKind};

use super::utils::State;

/// Keep balanced groups inside a type span, stopping at a top-level terminator.
/// Fall back to skipping the current line if span parsing fails.
fn push_type_span(st: &mut State<'_>, start: usize) {
    let src = st.stream.src();
    let terminator = filter(move |kind: &SyntaxKind| *kind == SyntaxKind::T_SEMI).or(filter_map(
        move |span: Span, kind| {
            if src
                .get(span.clone())
                .is_some_and(|text| text.contains('\n'))
            {
                Ok(kind)
            } else {
                Err(Simple::expected_input_found(span, Vec::new(), Some(kind)))
            }
        },
    ));
    let content = choice((
        balanced_block(SyntaxKind::T_LPAREN, SyntaxKind::T_RPAREN),
        balanced_block(SyntaxKind::T_LBRACE, SyntaxKind::T_RBRACE),
        balanced_block(SyntaxKind::T_LBRACKET, SyntaxKind::T_RBRACKET),
        terminator.not().ignored(),
    ));
    let parser = content
        .repeated()
        .then_ignore(terminator.or_not())
        .map_with_span(|_, span: Span| start..span.end);
    let input = chumsky::Stream::from_iter(
        start..src.len(),
        st.stream.tokens().iter().skip(st.stream.cursor()).cloned(),
    );
    let (span, _) = parser.parse_recovery(input);
    if let Some(span) = span {
        st.stream.skip_until(span.end);
        st.spans.push(span);
    } else {
        st.skip_line();
    }
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
