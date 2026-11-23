//! Scanner for `apply` statement spans.
//!
//! Parses top-level `apply` statements and records their spans, reporting
//! syntax errors while keeping span scanning aligned.

use chumsky::prelude::*;

use crate::parser::lexer_helpers::{ident, inline_ws};
use crate::parser::token_stream::TokenStream;
use crate::{Span, SyntaxKind};

use super::utils::{State, parse_and_record_split};

fn handle_apply(
    stream: &mut TokenStream<'_>,
    spans: &mut Vec<Span>,
    diagnostics: &mut Vec<Simple<SyntaxKind>>,
    span: Span,
) {
    let ws = inline_ws().repeated();
    let ident_p = ident();

    let ident_list = ident_p
        .clone()
        .separated_by(just(SyntaxKind::T_COMMA).padded_by(ws.clone()))
        .allow_trailing()
        .ignored();

    let paren_list = just(SyntaxKind::T_LPAREN)
        .padded_by(ws.clone())
        .ignore_then(ident_list.clone())
        .then_ignore(just(SyntaxKind::T_RPAREN).padded_by(ws.clone()))
        .ignored();

    let parser = just(SyntaxKind::K_APPLY)
        .padded_by(ws.clone())
        .ignore_then(ident_p)
        .then(paren_list.clone())
        .then_ignore(just(SyntaxKind::T_ARROW).padded_by(ws.clone()))
        .then(paren_list)
        .padded_by(ws)
        .map_with_span(|_, sp: Span| sp);

    parse_and_record_split(stream, spans, diagnostics, span.start, parser);
}

pub(crate) fn collect_apply_spans(
    tokens: &[(SyntaxKind, Span)],
    src: &str,
) -> (Vec<Span>, Vec<Simple<SyntaxKind>>) {
    let mut st: State<'_> = State::new(tokens, src, Vec::new());

    let State {
        stream,
        spans,
        extra,
    } = &mut st;
    stream.dispatch(|stream, kind, span| match kind {
        SyntaxKind::K_APPLY => handle_apply(stream, spans, extra, span),
        _ => stream.advance(),
    });

    st.into_parts()
}
