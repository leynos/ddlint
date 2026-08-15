//! Diagnostics for tokens reserved without parser semantics.
//!
//! The active syntax specification section 9.1 keeps these token kinds in the
//! lexer for precise spans, but rejects unsupported uses in the parser with a
//! deterministic message and fix hint.

use std::collections::HashSet;

use chumsky::error::{Simple, SimpleReason};

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
    parse_errors: &[Simple<SyntaxKind>],
) -> Vec<Simple<SyntaxKind>> {
    let emitted_reserved_errors = emitted_reserved_errors(parse_errors);
    tokens
        .iter()
        .enumerate()
        .filter_map(|(idx, (kind, span))| {
            let message = reserved_message_for_token(tokens, idx, *kind)?;
            (!emitted_reserved_errors.contains(&(span.clone(), message)))
                .then(|| reserved_token_error(span.clone(), message))
        })
        .collect()
}

fn emitted_reserved_errors(errors: &[Simple<SyntaxKind>]) -> HashSet<(Span, &'static str)> {
    errors
        .iter()
        .filter_map(|error| match error.reason() {
            SimpleReason::Custom(message) => {
                reserved_message(message).map(|message| (error.span().clone(), message))
            }
            _ => None,
        })
        .collect()
}

fn reserved_message(message: &str) -> Option<&'static str> {
    [
        RESERVED_TYPEDEF_ERROR,
        RESERVED_SPACESHIP_ERROR,
        RESERVED_BARE_HASH_ERROR,
        RESERVED_BIGINT_ERROR,
        RESERVED_BIT_ERROR,
        RESERVED_DOUBLE_ERROR,
        RESERVED_FLOAT_ERROR,
        RESERVED_SIGNED_ERROR,
    ]
    .into_iter()
    .find(|candidate| *candidate == message)
}

fn reserved_message_for_token(
    tokens: &[(SyntaxKind, Span)],
    idx: usize,
    kind: SyntaxKind,
) -> Option<&'static str> {
    if kind == SyntaxKind::T_HASH {
        return is_bare_hash(tokens, idx).then_some(RESERVED_BARE_HASH_ERROR);
    }

    rejection_for(kind)
}

fn is_bare_hash(tokens: &[(SyntaxKind, Span)], idx: usize) -> bool {
    !matches!(tokens.get(idx + 1), Some((SyntaxKind::T_LBRACKET, _)))
}
