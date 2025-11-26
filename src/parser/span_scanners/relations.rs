//! Scanner for relation, input, and output declarations.
//!
//! This module groups parsing logic for relation-like constructs, recording
//! spans while accumulating any parse errors for downstream diagnostics.

use chumsky::prelude::*;

use crate::parser::{
    ast::parse_utils::{primary_key_clause, relation_columns},
    span_collector::SpanCollector,
};
use crate::{Span, SyntaxKind};

#[derive(Debug)]
struct Extras<'a> {
    src: &'a str,
    errors: Vec<Simple<SyntaxKind>>,
}

type State<'a> = SpanCollector<'a, Extras<'a>>;

fn record_relation(st: &mut State<'_>, start: usize) {
    let parser = relation_columns()
        .then(primary_key_clause(st.extra.src).or_not())
        .map_with_span(|_, sp: Span| sp);

    let (res, errs) = st.parse_span(parser, start);
    if !errs.is_empty() {
        st.extra.errors.extend(errs.clone());
    }

    if let Some(sp) = res {
        st.stream.skip_until(sp.end);
        st.stream.skip_ws_inline();
        if errs.is_empty() {
            // Defensive: detect stray or malformed `primary` clauses the parser allowed through.
            if let Some((_, span)) = st.stream.peek().cloned()
                && st.extra.src.get(span.clone()) == Some("primary")
            {
                st.extra.errors.push(Simple::custom(
                    span,
                    "unexpected or malformed primary key clause",
                ));
                st.skip_line();
            } else {
                let end = st.stream.line_end(st.stream.cursor());
                st.stream.skip_until(end);
                st.spans.push(start..end);
            }
        } else {
            st.skip_line();
        }
    } else {
        st.skip_line();
    }
}

/// Extract relation, input, and output spans from the token stream.
///
/// * `tokens` - Slice of `(SyntaxKind, Span)` pairs representing the tokenised source.
/// * `src` - Source string used for span slicing and diagnostic context.
///
/// Returns `(Vec<Span>, Vec<Simple<SyntaxKind>>)` containing collected spans and any parse errors.
pub(crate) fn collect_relation_spans(
    tokens: &[(SyntaxKind, Span)],
    src: &str,
) -> (Vec<Span>, Vec<Simple<SyntaxKind>>) {
    /// Handle an `input` relation declaration: advance, skip inline whitespace,
    /// parse the following relation or skip to line end on mismatch.
    fn handle_input(st: &mut State<'_>, span: Span) {
        let start = span.start;
        st.stream.advance();
        st.stream.skip_ws_inline();
        if st
            .stream
            .peek()
            .is_some_and(|(kind, _)| *kind == SyntaxKind::K_RELATION)
        {
            st.stream.advance();
            record_relation(st, start);
        } else {
            let end = st.stream.line_end(st.stream.cursor());
            st.stream.skip_until(end);
        }
    }

    /// Outputs share the same grammar as inputs, so reuse the input handler.
    fn handle_output(st: &mut State<'_>, span: Span) {
        handle_input(st, span);
    }

    /// Handle a bare `relation` declaration by consuming the token and recording the span.
    fn handle_relation(st: &mut State<'_>, span: Span) {
        let start = span.start;
        st.stream.advance();
        record_relation(st, start);
    }

    let mut st = State::new(
        tokens,
        src,
        Extras {
            src,
            errors: Vec::new(),
        },
    );

    token_dispatch!(st, {
        SyntaxKind::K_INPUT => handle_input,
        SyntaxKind::K_OUTPUT => handle_output,
        SyntaxKind::K_RELATION => handle_relation,
    });

    let (spans, extras) = st.into_parts();
    (spans, extras.errors)
}
