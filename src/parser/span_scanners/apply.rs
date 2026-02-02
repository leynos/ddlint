//! Scanner for `apply` statement spans.
//!
//! Parses top-level `apply` statements and records their spans, reporting
//! syntax errors while keeping span scanning aligned.

use chumsky::prelude::*;

use crate::parser::lexer_helpers::{ident, inline_ws};
use crate::{Span, SyntaxKind};

use super::utils::{State, parse_and_record};

pub(crate) fn collect_apply_spans(
    tokens: &[(SyntaxKind, Span)],
    src: &str,
) -> (Vec<Span>, Vec<Simple<SyntaxKind>>) {
    let mut st: State<'_> = State::new(tokens, src, Vec::new());

    let handle_apply = |st: &mut State<'_>, span: Span| {
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

        parse_and_record(st, span.start, parser);
    };

    token_dispatch!(st, {
        SyntaxKind::K_APPLY => handle_apply,
    });

    st.into_parts()
}
