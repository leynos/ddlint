//! Scanner for transformer declarations.
//!
//! Parses transformer definitions, delegating shared extern handling to the
//! utilities module.

use chumsky::prelude::*;

use crate::parser::lexer_helpers::{balanced_block, ident, inline_ws};
use crate::{Span, SyntaxKind};

use super::utils::collect_extern_declarations;

pub(crate) fn collect_transformer_spans(
    tokens: &[(SyntaxKind, Span)],
    src: &str,
) -> (Vec<Span>, Vec<Simple<SyntaxKind>>) {
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

    collect_extern_declarations(tokens, src, SyntaxKind::K_TRANSFORMER, transformer_decl)
}
