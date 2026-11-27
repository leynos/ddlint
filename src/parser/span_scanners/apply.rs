//! Scanner for `apply` statement spans.
//!
//! Parses top-level `apply` statements and records their spans, reporting
//! syntax errors while keeping span scanning aligned.

use chumsky::prelude::*;

use super::utils::collect_with_parser;
use crate::{
    Span,
    SyntaxKind,
    parser::lexer_helpers::{ident, inline_ws},
};

/// Grammar for one `apply` statement.
///
/// Matches the transformer name, then the input and output identifier lists
/// around `->`, and returns the statement span including trailing inline
/// whitespace. Empty and trailing-comma lists stay accepted.
fn apply_parser() -> impl Parser<SyntaxKind, Span, Error = Simple<SyntaxKind>> {
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

    just(SyntaxKind::K_APPLY)
        .padded_by(ws.clone())
        .ignore_then(ident_p)
        .then(paren_list.clone())
        .then_ignore(just(SyntaxKind::T_ARROW).padded_by(ws.clone()))
        .then(paren_list)
        .padded_by(ws)
        .map_with_span(|_, sp: Span| sp)
}

pub(crate) fn collect_apply_spans(
    tokens: &[(SyntaxKind, Span)],
    src: &str,
) -> (Vec<Span>, Vec<Simple<SyntaxKind>>) {
    collect_with_parser(tokens, src, SyntaxKind::K_APPLY, apply_parser)
}
