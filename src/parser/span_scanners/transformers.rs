//! Scanner for transformer declarations.
//!
//! Parses transformer definitions, delegating shared extern handling to the
//! utilities module.

use chumsky::prelude::*;

use crate::parser::lexer_helpers::{balanced_block, ident, inline_ws};
use crate::{Span, SyntaxKind};

use super::utils::{State, collect_extern_declarations_with_rule};

const NON_EXTERN_TRANSFORMER_ERROR: &str = "transformer declarations must be extern";

fn transformer_decl() -> impl Parser<SyntaxKind, Span, Error = Simple<SyntaxKind>> {
    let ws = inline_ws().repeated();
    let ident_p = ident();

    let args = balanced_block(SyntaxKind::T_LPAREN, SyntaxKind::T_RPAREN);
    let outputs = ident_p
        .clone()
        .separated_by(just(SyntaxKind::T_COMMA).padded_by(ws.clone()))
        .at_least(1)
        .ignored();

    just(SyntaxKind::K_TRANSFORMER)
        .padded_by(ws.clone())
        .ignore_then(ident_p)
        .then(args)
        .then_ignore(just(SyntaxKind::T_COLON).padded_by(ws.clone()))
        .then(outputs)
        .padded_by(ws)
        .map_with_span(|_, sp: Span| sp)
}

pub(crate) fn collect_transformer_spans(
    tokens: &[(SyntaxKind, Span)],
    src: &str,
) -> (Vec<Span>, Vec<Simple<SyntaxKind>>) {
    collect_extern_declarations_with_rule(
        tokens,
        src,
        SyntaxKind::K_TRANSFORMER,
        transformer_decl,
        handle_non_extern_transformer,
    )
}

fn handle_non_extern_transformer(st: &mut State<'_>, span: Span) {
    let start = span.start;
    let parser = transformer_decl();
    let (res, errs) = st.parse_span(parser, start);
    if let Some(sp) = res {
        st.stream.skip_until(sp.end);
        push_non_extern_transformer_error(&mut st.extra, sp, errs);
    } else {
        st.skip_line();
        push_non_extern_transformer_error(&mut st.extra, span, errs);
    }
}

fn push_non_extern_transformer_error(
    extra: &mut Vec<Simple<SyntaxKind>>,
    span: Span,
    errs: Vec<Simple<SyntaxKind>>,
) {
    extra.extend(errs);
    extra.push(Simple::custom(span, NON_EXTERN_TRANSFORMER_ERROR));
}
