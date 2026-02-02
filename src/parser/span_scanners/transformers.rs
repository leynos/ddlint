//! Scanner for transformer declarations.
//!
//! Parses transformer definitions, delegating shared extern handling to the
//! utilities module.

use chumsky::prelude::*;

use crate::parser::lexer_helpers::{balanced_block, ident, inline_ws};
use crate::{Span, SyntaxKind};

use super::utils::{State, parse_and_record};

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
    let mut st: State<'_> = State::new(tokens, src, Vec::new());

    let handle_extern = |st: &mut State<'_>, span: Span| {
        let is_decl = st
            .stream
            .peek_after_ws_inline()
            .is_some_and(|(k, _)| *k == SyntaxKind::K_TRANSFORMER);
        if !is_decl {
            st.skip_line();
            return;
        }

        let ws = inline_ws().repeated();
        let start = span.start;
        let parser = just(SyntaxKind::K_EXTERN)
            .padded_by(ws.clone())
            .ignore_then(transformer_decl())
            .map(move |sp: Span| start..sp.end);

        parse_and_record(st, start, parser);
    };

    let handle_non_extern = |st: &mut State<'_>, span: Span| {
        let start = span.start;
        let parser = transformer_decl();
        let (res, errs) = st.parse_span(parser, start);
        if let Some(sp) = res {
            st.stream.skip_until(sp.end);
            st.extra.extend(errs.clone());
            st.extra.push(Simple::custom(
                sp,
                "transformer declarations must be extern",
            ));
        } else {
            st.skip_line();
            st.extra.extend(errs.clone());
            st.extra.push(Simple::custom(
                span,
                "transformer declarations must be extern",
            ));
        }
    };

    token_dispatch!(st, {
        SyntaxKind::K_EXTERN => handle_extern,
        SyntaxKind::K_TRANSFORMER => handle_non_extern,
    });

    st.into_parts()
}
