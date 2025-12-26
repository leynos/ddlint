//! Helpers for constructing expression nodes and asserting parser errors in
//! tests.
//!
//! These functions reduce boilerplate when asserting over [`Expr`] nodes and
//! verifying that parsing failures surface precise spans and messages.

mod assertions;
mod expressions;
mod literals;

pub use assertions::{
    assert_delimiter_error, assert_no_parse_errors, assert_parse_error,
    assert_unclosed_delimiter_error,
};
pub use expressions::{
    bit_slice, break_expr, call, call_expr, closure, continue_expr, field, field_access, for_loop,
    if_expr, map_entry, map_lit, match_arm, match_expr, method_call, pat, return_expr, struct_expr,
    tuple, tuple_index, var, vec_lit,
};
pub use literals::{
    lit_bool, lit_interned_raw_interpolated_str, lit_interned_raw_str, lit_interned_str,
    lit_interpolated_str, lit_num, lit_raw_interpolated_str, lit_raw_str, lit_str,
};

use crate::{Span, SyntaxKind, tokenize_with_trivia};

/// Tokenize `src` into `(SyntaxKind, Span)` pairs using
/// [`tokenize_with_trivia`].
///
/// # Examples
///
/// ```rust,no_run
/// # #[cfg(feature = "test-support")]
/// # {
/// use ddlint::test_util::tokenize;
/// assert!(tokenize("input relation R(x: u32);").len() > 0);
/// # }
/// ```
#[must_use]
pub fn tokenize(src: &str) -> Vec<(SyntaxKind, Span)> {
    tokenize_with_trivia(src)
}

/// Typed wrapper for variable and function names.
#[derive(Debug, Clone)]
pub struct Name(pub(crate) String);

impl From<&str> for Name {
    fn from(s: &str) -> Self {
        Self(s.to_string())
    }
}

impl From<String> for Name {
    fn from(s: String) -> Self {
        Self(s)
    }
}

/// Common error message patterns for parser assertions.
#[derive(Debug, Clone)]
pub enum ErrorPattern {
    Custom(String),
}

impl From<&str> for ErrorPattern {
    fn from(s: &str) -> Self {
        Self::Custom(s.to_string())
    }
}

/// Replace internal token names with human-readable forms.
pub(crate) fn normalise_tokens(s: &str) -> String {
    // Build replacements from SyntaxKind debug names to human-friendly labels.
    // This avoids a hand-maintained token map drifting from the parser.
    use SyntaxKind as K;
    let keys = [
        (K::T_LPAREN, "left paren"),
        (K::T_RPAREN, "right paren"),
        (K::T_LBRACKET, "left bracket"),
        (K::T_RBRACKET, "right bracket"),
        (K::T_LBRACE, "left brace"),
        (K::T_RBRACE, "right brace"),
        (K::T_COMMA, "comma"),
        (K::T_SEMI, "semicolon"),
        (K::T_PIPE, "pipe"),
        (K::T_DOT, "dot"),
        (K::T_COLON, "colon"),
        (K::T_IDENT, "identifier"),
        (K::T_NUMBER, "number"),
    ];
    let mut out = s.to_string();
    for (k, human) in keys {
        let raw = format!("{k:?}");
        out = out.replace(&raw, human);
    }
    out
}
