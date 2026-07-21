//! Diagnostics for tokens reserved without parser semantics.
//!
//! The active syntax specification section 9.1 keeps these token kinds in the
//! lexer for precise spans, but rejects unsupported uses in the parser with a
//! deterministic message and fix hint.

use chumsky::error::Simple;

use crate::{Span, SyntaxKind};

pub(crate) const RESERVED_TYPEDEF_ERROR: &str =
    "`typedef` is a legacy DDlog keyword; use `type` instead";
pub(crate) const RESERVED_SPACESHIP_ERROR: &str =
    "`<=>` was reserved upstream but has no semantics in DDlog; remove it";
pub(crate) const RESERVED_BARE_HASH_ERROR: &str =
    "`#` is reserved; only `#[...]` attribute syntax is accepted";
pub(crate) const RESERVED_BIGINT_ERROR: &str =
    "`bigint` is a legacy type name; use a sized integer such as `i64` or `u64`";
pub(crate) const RESERVED_BIT_ERROR: &str =
    "`bit` is a legacy type name; use an unsigned sized integer such as `u32`";
pub(crate) const RESERVED_DOUBLE_ERROR: &str = "`double` is a legacy type name; use `f64`";
pub(crate) const RESERVED_FLOAT_ERROR: &str = "`float` is a legacy type name; use `f32`";
pub(crate) const RESERVED_SIGNED_ERROR: &str =
    "`signed` is a legacy type name; use a signed sized integer such as `i32`";

pub(crate) fn rejection_for(kind: SyntaxKind) -> Option<&'static str> {
    match kind {
        SyntaxKind::K_TYPEDEF => Some(RESERVED_TYPEDEF_ERROR),
        SyntaxKind::T_SPACESHIP => Some(RESERVED_SPACESHIP_ERROR),
        SyntaxKind::K_BIGINT => Some(RESERVED_BIGINT_ERROR),
        SyntaxKind::K_BIT => Some(RESERVED_BIT_ERROR),
        SyntaxKind::K_DOUBLE => Some(RESERVED_DOUBLE_ERROR),
        SyntaxKind::K_FLOAT => Some(RESERVED_FLOAT_ERROR),
        SyntaxKind::K_SIGNED => Some(RESERVED_SIGNED_ERROR),
        _ => None,
    }
}

pub(crate) fn reserved_token_error(span: Span, message: &'static str) -> Simple<SyntaxKind> {
    Simple::custom(span, message)
}

pub(crate) fn collect_reserved_token_errors(
    tokens: &[(SyntaxKind, Span)],
    src: &str,
    expression_spans: &[Span],
) -> Vec<Simple<SyntaxKind>> {
    tokens
        .iter()
        .enumerate()
        .filter_map(|(idx, (kind, span))| {
            reserved_message_for_token(tokens, src, expression_spans, idx, *kind, span)
                .map(|message| reserved_token_error(span.clone(), message))
        })
        .collect()
}

fn reserved_message_for_token(
    tokens: &[(SyntaxKind, Span)],
    src: &str,
    expression_spans: &[Span],
    idx: usize,
    kind: SyntaxKind,
    span: &Span,
) -> Option<&'static str> {
    if kind == SyntaxKind::T_HASH {
        return is_bare_hash(tokens, src, idx).then_some(RESERVED_BARE_HASH_ERROR);
    }

    let message = rejection_for(kind)?;
    if kind == SyntaxKind::T_SPACESHIP && span_is_in_expression(span, expression_spans) {
        return None;
    }
    Some(message)
}

fn is_bare_hash(tokens: &[(SyntaxKind, Span)], src: &str, idx: usize) -> bool {
    next_inline_non_trivia(tokens, src, idx).is_none_or(|(kind, _)| kind != SyntaxKind::T_LBRACKET)
}

fn next_inline_non_trivia(
    tokens: &[(SyntaxKind, Span)],
    src: &str,
    idx: usize,
) -> Option<(SyntaxKind, Span)> {
    let mut cursor = idx + 1;
    while let Some((kind, span)) = tokens.get(cursor) {
        if matches!(kind, SyntaxKind::T_WHITESPACE | SyntaxKind::T_COMMENT)
            && src
                .get(span.clone())
                .is_some_and(|text| !text.contains('\n'))
        {
            cursor += 1;
            continue;
        }
        return Some((*kind, span.clone()));
    }
    None
}

fn span_is_in_expression(span: &Span, expression_spans: &[Span]) -> bool {
    expression_spans
        .iter()
        .any(|expr| expr.start <= span.start && span.end <= expr.end)
}
