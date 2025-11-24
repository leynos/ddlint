//! Scanner for function declarations.
//!
//! Handles both regular and `extern` functions, capturing spans while
//! recovering gracefully from malformed inputs.

use chumsky::prelude::*;

use crate::parser::lexer_helpers::{balanced_block, ident, inline_ws};
use crate::{Span, SyntaxKind};

use super::utils::{State, parse_and_record};

pub(crate) fn collect_function_spans(
    tokens: &[(SyntaxKind, Span)],
    src: &str,
) -> (Vec<Span>, Vec<Simple<SyntaxKind>>) {
    fn params() -> impl Parser<SyntaxKind, (), Error = Simple<SyntaxKind>> {
        balanced_block(SyntaxKind::T_LPAREN, SyntaxKind::T_RPAREN)
    }

    fn return_ty() -> impl Parser<SyntaxKind, (), Error = Simple<SyntaxKind>> {
        just(SyntaxKind::T_COLON)
            .padded_by(inline_ws().repeated())
            .ignore_then(
                filter(|kind: &SyntaxKind| {
                    !matches!(kind, SyntaxKind::T_LBRACE | SyntaxKind::T_SEMI)
                })
                .ignored()
                .padded_by(inline_ws().repeated())
                .repeated(),
            )
            .ignored()
            .or_not()
            .ignored()
    }

    fn body_block() -> impl Parser<SyntaxKind, (), Error = Simple<SyntaxKind>> {
        balanced_block(SyntaxKind::T_LBRACE, SyntaxKind::T_RBRACE)
    }

    fn body_optional() -> impl Parser<SyntaxKind, (), Error = Simple<SyntaxKind>> {
        body_block().or_not().ignored()
    }

    fn func_decl(with_body: bool) -> BoxedParser<'static, SyntaxKind, Span, Simple<SyntaxKind>> {
        let ident = ident();
        let parser = just(SyntaxKind::K_FUNCTION)
            .padded_by(inline_ws().repeated())
            .ignore_then(ident.clone())
            .then(params())
            .then(return_ty())
            .then(if with_body {
                body_block().boxed()
            } else {
                body_optional().boxed()
            })
            .padded_by(inline_ws().repeated())
            .map_with_span(|_, sp: Span| sp);
        parser.boxed()
    }

    fn handle_func(st: &mut State<'_>, span: Span, is_extern: bool) {
        if is_extern
            && !matches!(
                st.stream.peek_after_ws_inline().map(|t| t.0),
                Some(SyntaxKind::K_FUNCTION)
            )
        {
            st.skip_line();
            return;
        }

        let start = span.start;
        let parser = if is_extern {
            let s = start;
            just(SyntaxKind::K_EXTERN)
                .padded_by(inline_ws().repeated())
                .ignore_then(func_decl(false))
                .map(move |sp: Span| s..sp.end)
                .boxed()
        } else {
            func_decl(true)
        };
        parse_and_record(st, start, parser);
    }

    let mut st: State<'_> = State::new(tokens, src, Vec::new());

    let handle_extern = |st: &mut State<'_>, span: Span| {
        handle_func(st, span, true);
    };
    let handle_function = |st: &mut State<'_>, span: Span| {
        handle_func(st, span, false);
    };

    token_dispatch!(st, {
        SyntaxKind::K_EXTERN => handle_extern,
        SyntaxKind::K_FUNCTION => handle_function,
    });

    st.into_parts()
}
