//! Scanner for transformer declarations.
//!
//! Parses transformer definitions, delegating shared extern handling to the
//! utilities module.

use chumsky::prelude::*;

use crate::parser::lexer_helpers::{balanced_block, ident, inline_ws};
use crate::{Span, SyntaxKind};

use super::utils::State;

const NON_EXTERN_TRANSFORMER_ERROR: &str = "transformer declarations must be extern";
const MISSING_OUTPUT_SIGNATURE_ERROR: &str =
    "transformer declarations require ':' followed by at least one output identifier";

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

    while let Some(&(token_kind, ref span_ref)) = st.stream.peek() {
        let span = span_ref.clone();
        if token_kind == SyntaxKind::K_EXTERN {
            handle_extern_transformer(&mut st, span);
        } else if token_kind == SyntaxKind::K_TRANSFORMER {
            handle_non_extern_transformer(&mut st, span);
        } else {
            st.stream.advance();
        }
    }

    st.into_parts()
}

fn handle_extern_transformer(st: &mut State<'_>, span: Span) {
    let is_transformer_decl = st
        .stream
        .peek_after_ws_inline()
        .is_some_and(|(kind, _)| *kind == SyntaxKind::K_TRANSFORMER);
    if !is_transformer_decl {
        st.skip_line();
        return;
    }

    let ws = inline_ws().repeated();
    let start = span.start;
    let parser = just(SyntaxKind::K_EXTERN)
        .padded_by(ws.clone())
        .ignore_then(transformer_decl())
        .map(move |sp: Span| start..sp.end);
    let (res, errs) = st.parse_span(parser, start);
    if let Some(ref declaration_span) = res {
        st.stream.skip_until(declaration_span.end);
        st.spans.push(declaration_span.clone());
        st.extra.extend(errs);
        return;
    }

    let error_span = start..st.stream.line_end(st.stream.cursor());
    st.skip_line();
    if has_missing_output_signature(st, start, error_span.end) {
        st.extra
            .push(Simple::custom(error_span, MISSING_OUTPUT_SIGNATURE_ERROR));
    } else {
        st.extra.extend(errs);
    }
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

fn has_missing_output_signature(st: &State<'_>, start: usize, line_end: usize) -> bool {
    let tokens = st.stream.tokens();
    let src = st.stream.src();
    let Some(mut idx) = tokens.iter().position(|(_, span)| span.start == start) else {
        return false;
    };

    if !matches_token(tokens, idx, SyntaxKind::K_EXTERN) {
        return false;
    }
    idx += 1;
    idx = skip_inline_trivia(tokens, src, idx, line_end);

    if !matches_token(tokens, idx, SyntaxKind::K_TRANSFORMER) {
        return false;
    }
    idx += 1;
    idx = skip_inline_trivia(tokens, src, idx, line_end);

    if !matches_token(tokens, idx, SyntaxKind::T_IDENT) {
        return false;
    }
    idx += 1;
    idx = skip_inline_trivia(tokens, src, idx, line_end);

    let Some(next_idx) = skip_balanced_parens(tokens, idx, line_end) else {
        return false;
    };
    idx = skip_inline_trivia(tokens, src, next_idx, line_end);

    if !matches_token(tokens, idx, SyntaxKind::T_COLON) {
        return true;
    }
    idx += 1;
    idx = skip_inline_trivia(tokens, src, idx, line_end);

    !matches_token(tokens, idx, SyntaxKind::T_IDENT)
}

fn skip_balanced_parens(
    tokens: &[(SyntaxKind, Span)],
    start: usize,
    line_end: usize,
) -> Option<usize> {
    if !matches_token(tokens, start, SyntaxKind::T_LPAREN) {
        return None;
    }

    let mut depth = 0usize;
    for (idx, (kind, span)) in tokens.iter().enumerate().skip(start) {
        if span.start >= line_end {
            return None;
        }

        match kind {
            SyntaxKind::T_LPAREN => depth += 1,
            SyntaxKind::T_RPAREN => {
                depth = depth.checked_sub(1)?;
                if depth == 0 {
                    return Some(idx + 1);
                }
            }
            _ => {}
        }
    }

    None
}

fn skip_inline_trivia(
    tokens: &[(SyntaxKind, Span)],
    src: &str,
    mut idx: usize,
    line_end: usize,
) -> usize {
    while let Some((kind, span)) = tokens.get(idx) {
        if span.start >= line_end
            || !matches!(kind, SyntaxKind::T_WHITESPACE | SyntaxKind::T_COMMENT)
            || src
                .get(span.clone())
                .is_some_and(|text| text.contains('\n'))
        {
            break;
        }
        idx += 1;
    }
    idx
}

fn matches_token(tokens: &[(SyntaxKind, Span)], idx: usize, expected: SyntaxKind) -> bool {
    tokens.get(idx).is_some_and(|(kind, _)| *kind == expected)
}
