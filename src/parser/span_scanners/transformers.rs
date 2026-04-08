//! Scanner for transformer declarations.
//!
//! This scanner owns transformer-specific recovery, including the targeted
//! missing-output diagnostic for malformed `extern transformer` declarations.
//! It imports only [`State`] from the shared scanner utilities.

use chumsky::prelude::*;

use crate::parser::error_messages::{
    CAPITALIZED_TRANSFORMER_NAME_ERROR, MISSING_OUTPUT_SIGNATURE_ERROR,
};
use crate::parser::lexer_helpers::{balanced_block, ident, inline_ws};
use crate::{Span, SyntaxKind};

use super::utils::State;

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

/// Collect transformer declaration spans and related parse errors.
///
/// `tokens` is the tokenized source stream, and `src` is the backing source
/// text used for span slicing. The returned tuple contains the collected
/// transformer spans plus any `Simple<SyntaxKind>` diagnostics raised while
/// scanning malformed declarations.
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

/// Validates that the transformer name starting at `start` begins with a
/// lowercase letter or underscore, emitting a targeted diagnostic if not.
/// Returns `true` if the name is valid, `false` otherwise.
fn push_invalid_name_diagnostic_if_needed(st: &mut State<'_>, start: usize) -> bool {
    let Some(name_span) = extract_transformer_name_span(st, start) else {
        return true;
    };
    if let Some(name) = st.stream.src().get(name_span.clone())
        && !is_lowercase_ident(name)
    {
        st.extra.push(Simple::custom(
            name_span,
            CAPITALIZED_TRANSFORMER_NAME_ERROR,
        ));
        return false;
    }
    true
}

fn handle_extern_transformer(st: &mut State<'_>, span: Span) {
    if !is_extern_transformer_start(st) {
        st.skip_line();
        return;
    }

    let ws = inline_ws().repeated();
    let start = span.start;
    let parser = just(SyntaxKind::K_EXTERN)
        .padded_by(ws)
        .ignore_then(transformer_decl())
        .map(move |sp: Span| start..sp.end);
    let (res, errs) = st.parse_span(parser, start);
    if let Some(declaration_span) = res {
        let name_valid = push_invalid_name_diagnostic_if_needed(st, start);
        st.stream.skip_until(declaration_span.end);
        if name_valid {
            st.spans.push(declaration_span);
        }
        st.extra.extend(errs);
        return;
    }

    st.skip_line();
    push_invalid_name_diagnostic_if_needed(st, start);

    if let Some(error_span) = missing_output_signature_span_unchecked(st, start) {
        st.extra
            .push(Simple::custom(error_span, MISSING_OUTPUT_SIGNATURE_ERROR));
    }

    st.extra.extend(errs);
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

fn is_extern_transformer_start(st: &State<'_>) -> bool {
    st.stream
        .peek_after_ws_inline()
        .is_some_and(|(kind, _)| *kind == SyntaxKind::K_TRANSFORMER)
}

/// Skips past the `extern transformer <ident>` prefix, returning the
/// identifier's span and the index immediately after it (past trivia).
fn skip_extern_transformer_prefix(
    tokens: &[(SyntaxKind, Span)],
    start: usize,
) -> Option<(Span, usize)> {
    let mut idx = tokens.iter().position(|(_, span)| span.start == start)?;

    if !matches_token(tokens, idx, SyntaxKind::K_EXTERN) {
        return None;
    }
    idx += 1;
    idx = skip_trivia(tokens, idx);

    if !matches_token(tokens, idx, SyntaxKind::K_TRANSFORMER) {
        return None;
    }
    idx += 1;
    idx = skip_trivia(tokens, idx);

    if !matches_token(tokens, idx, SyntaxKind::T_IDENT) {
        return None;
    }
    let ident_span = tokens.get(idx)?.1.clone();
    idx += 1;
    idx = skip_trivia(tokens, idx);

    Some((ident_span, idx))
}

fn missing_output_signature_span_unchecked(st: &State<'_>, start: usize) -> Option<Span> {
    let tokens = st.stream.tokens();
    let src = st.stream.src();
    let (_ident_span, idx) = skip_extern_transformer_prefix(tokens, start)?;
    missing_output_signature_span_impl(src, tokens, idx, start)
}

fn missing_output_signature_span_impl(
    src: &str,
    tokens: &[(SyntaxKind, Span)],
    mut idx: usize,
    start: usize,
) -> Option<Span> {
    // The recovery scan mirrors the declaration prefix exactly:
    // `extern transformer <ident>(...)`.
    //
    // It intentionally tolerates multiline trivia because the parser accepts
    // newline-separated parameter lists. If the parameter list itself is
    // malformed, we fall back to the original parser errors instead of
    // reclassifying the failure as "missing output signature".
    let (next_idx, decl_end) = skip_balanced_parens(tokens, idx)?;
    idx = skip_trivia(tokens, next_idx);

    if !matches_token(tokens, idx, SyntaxKind::T_COLON) {
        return Some(start..declaration_error_end(src, decl_end));
    }
    let colon_end = tokens.get(idx).map_or(decl_end, |(_, span)| span.end);
    idx += 1;
    idx = skip_trivia(tokens, idx);

    if matches_token(tokens, idx, SyntaxKind::T_IDENT) {
        None
    } else {
        Some(start..declaration_error_end(src, colon_end))
    }
}

fn skip_balanced_parens(tokens: &[(SyntaxKind, Span)], start: usize) -> Option<(usize, usize)> {
    if !matches_token(tokens, start, SyntaxKind::T_LPAREN) {
        return None;
    }

    let mut depth = 0usize;
    for (idx, (kind, span)) in tokens.iter().enumerate().skip(start) {
        match kind {
            SyntaxKind::T_LPAREN => depth += 1,
            SyntaxKind::T_RPAREN => {
                depth = depth.checked_sub(1)?;
                if depth == 0 {
                    return Some((idx + 1, span.end));
                }
            }
            _ => {}
        }
    }

    None
}

fn declaration_error_end(src: &str, fallback_end: usize) -> usize {
    line_end_at(src, fallback_end)
}

fn line_end_at(src: &str, start: usize) -> usize {
    src.as_bytes()
        .iter()
        .enumerate()
        .skip(start)
        .find_map(|(idx, byte)| (*byte == b'\n').then_some(idx + 1))
        .unwrap_or(src.len())
}

fn skip_trivia(tokens: &[(SyntaxKind, Span)], mut idx: usize) -> usize {
    while let Some((kind, _span)) = tokens.get(idx) {
        if should_stop_skipping_trivia(*kind) {
            break;
        }
        idx += 1;
    }
    idx
}

fn should_stop_skipping_trivia(kind: SyntaxKind) -> bool {
    !matches!(kind, SyntaxKind::T_WHITESPACE | SyntaxKind::T_COMMENT)
}

fn is_lowercase_ident(ident: &str) -> bool {
    ident
        .chars()
        .next()
        .is_some_and(|ch| ch == '_' || ch.is_ascii_lowercase())
}

fn extract_transformer_name_span(st: &State<'_>, start: usize) -> Option<Span> {
    let tokens = st.stream.tokens();
    let (ident_span, _idx) = skip_extern_transformer_prefix(tokens, start)?;
    Some(ident_span)
}

fn matches_token(tokens: &[(SyntaxKind, Span)], idx: usize, expected: SyntaxKind) -> bool {
    tokens.get(idx).is_some_and(|(kind, _)| *kind == expected)
}
