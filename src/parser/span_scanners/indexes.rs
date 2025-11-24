//! Scanner for index declarations.
//!
//! Parses `index` statements, capturing their spans and any syntax errors.

use chumsky::prelude::*;

use crate::parser::lexer_helpers::{balanced_block_nonempty, ident, inline_ws};
use crate::{Span, SyntaxKind};

use super::utils::{parse_and_record, State};

pub(crate) fn collect_index_spans(
    tokens: &[(SyntaxKind, Span)],
    src: &str,
) -> (Vec<Span>, Vec<Simple<SyntaxKind>>) {
    fn index_columns() -> impl Parser<SyntaxKind, (), Error = Simple<SyntaxKind>> {
        balanced_block_nonempty(SyntaxKind::T_LPAREN, SyntaxKind::T_RPAREN)
    }

    fn index_decl() -> impl Parser<SyntaxKind, Span, Error = Simple<SyntaxKind>> {
        let ident = ident();

        let columns = index_columns();

        just(SyntaxKind::K_INDEX)
            .padded_by(inline_ws().repeated())
            .ignore_then(ident.clone())
            .then_ignore(just(SyntaxKind::K_ON).padded_by(inline_ws().repeated()))
            .then(ident)
            .then(columns)
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
