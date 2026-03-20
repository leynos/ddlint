//! Scanner for index declarations.
//!
//! Parses `index` statements, capturing their spans and any syntax errors.

use chumsky::prelude::*;

use crate::parser::lexer_helpers::{atom, balanced_block_nonempty, ident, inline_ws};
use crate::{Span, SyntaxKind};

use super::utils::{State, parse_and_record};

pub(crate) const MISSING_INDEX_FIELD_LIST: &str =
    "index declarations require a typed field list `(name: Type, ...)` before `on`";

pub(crate) fn collect_index_spans(
    tokens: &[(SyntaxKind, Span)],
    src: &str,
) -> (Vec<Span>, Vec<Simple<SyntaxKind>>) {
    fn index_fields() -> impl Parser<SyntaxKind, (), Error = Simple<SyntaxKind>> {
        balanced_block_nonempty(SyntaxKind::T_LPAREN, SyntaxKind::T_RPAREN)
            .or(just(SyntaxKind::K_ON)
                .try_map(|_, span| Err(Simple::custom(span, MISSING_INDEX_FIELD_LIST))))
    }

    fn delay_suffix() -> impl Parser<SyntaxKind, (), Error = Simple<SyntaxKind>> {
        let ws = inline_ws().repeated();

        just(SyntaxKind::T_MINUS)
            .padded_by(ws.clone())
            .then_ignore(just(SyntaxKind::T_LT).padded_by(ws.clone()))
            .ignore_then(just(SyntaxKind::T_NUMBER).padded_by(ws.clone()))
            .then_ignore(just(SyntaxKind::T_GT).padded_by(ws))
            .ignored()
    }

    fn index_target() -> impl Parser<SyntaxKind, (), Error = Simple<SyntaxKind>> {
        atom().then(delay_suffix().or_not()).ignored()
    }

    fn index_decl() -> impl Parser<SyntaxKind, Span, Error = Simple<SyntaxKind>> {
        let ident = ident();
        let fields = index_fields();
        let target = index_target();

        just(SyntaxKind::K_INDEX)
            .padded_by(inline_ws().repeated())
            .ignore_then(ident.clone())
            .then(fields)
            .then_ignore(just(SyntaxKind::K_ON).padded_by(inline_ws().repeated()))
            .then(target)
            .padded_by(inline_ws().repeated())
            .map_with_span(|_, sp: Span| sp)
    }

    let mut st: State<'_> = State::new(tokens, src, Vec::new());

    let handle_index = |st: &mut State<'_>, span: Span| {
        let parser = index_decl();
        parse_and_record(st, span.start, parser);
    };

    token_dispatch!(st, {
        SyntaxKind::K_INDEX => handle_index,
    });

    st.into_parts()
}
