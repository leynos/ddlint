//! Helpers for constructing literal expression nodes in tests.

use crate::parser::ast::{Expr, Literal, StringKind, StringLiteral};
use crate::parser::expression::parse_numeric_literal;

/// Newtype wrapper for numeric literal text.
///
/// Provides type safety for numeric literal source text passed to [`lit_num`].
#[derive(Debug, Clone)]
pub struct NumericText(String);

impl From<&str> for NumericText {
    fn from(s: &str) -> Self {
        Self(s.to_string())
    }
}

impl AsRef<str> for NumericText {
    fn as_ref(&self) -> &str {
        &self.0
    }
}

/// Newtype wrapper for string literal body content.
///
/// Provides type safety for string body content passed to string literal
/// constructors.
#[derive(Debug, Clone)]
pub struct StringBody(String);

impl From<&str> for StringBody {
    fn from(s: &str) -> Self {
        Self(s.to_string())
    }
}

impl AsRef<str> for StringBody {
    fn as_ref(&self) -> &str {
        &self.0
    }
}

/// Construct a numeric [`Expr::Literal`].
#[must_use]
pub fn lit_num(n: impl Into<NumericText>) -> Expr {
    let text: NumericText = n.into();
    let src = text.as_ref();
    let literal = parse_numeric_literal(src)
        .unwrap_or_else(|err| panic!("failed to parse numeric literal '{src}': {}", err.message()));
    Expr::Literal(Literal::Number(literal))
}

/// Construct a string [`Expr::Literal`].
#[must_use]
pub fn lit_str(s: impl Into<StringBody>) -> Expr {
    let body: StringBody = s.into();
    string_literal(
        body.as_ref(),
        StringKind::Standard {
            interpolated: false,
        },
        false,
    )
}

/// Construct an interpolated string [`Expr::Literal`].
#[must_use]
pub fn lit_interpolated_str(s: impl Into<StringBody>) -> Expr {
    let body: StringBody = s.into();
    string_literal(
        body.as_ref(),
        StringKind::Standard { interpolated: true },
        false,
    )
}

/// Construct a raw string [`Expr::Literal`].
#[must_use]
pub fn lit_raw_str(s: impl Into<StringBody>) -> Expr {
    let body: StringBody = s.into();
    string_literal(
        body.as_ref(),
        StringKind::Raw {
            interpolated: false,
        },
        false,
    )
}

/// Construct a raw interpolated string [`Expr::Literal`].
#[must_use]
pub fn lit_raw_interpolated_str(s: impl Into<StringBody>) -> Expr {
    let body: StringBody = s.into();
    string_literal(body.as_ref(), StringKind::Raw { interpolated: true }, false)
}

/// Construct an interned standard string [`Expr::Literal`].
#[must_use]
pub fn lit_interned_str(s: impl Into<StringBody>) -> Expr {
    let body: StringBody = s.into();
    string_literal(
        body.as_ref(),
        StringKind::Standard {
            interpolated: false,
        },
        true,
    )
}

/// Construct an interned raw string [`Expr::Literal`].
#[must_use]
pub fn lit_interned_raw_str(s: impl Into<StringBody>) -> Expr {
    let body: StringBody = s.into();
    string_literal(
        body.as_ref(),
        StringKind::Raw {
            interpolated: false,
        },
        true,
    )
}

/// Construct an interned interpolated raw string [`Expr::Literal`].
#[must_use]
pub fn lit_interned_raw_interpolated_str(s: impl Into<StringBody>) -> Expr {
    let body: StringBody = s.into();
    string_literal(body.as_ref(), StringKind::Raw { interpolated: true }, true)
}

fn string_literal(body: &str, kind: StringKind, interned: bool) -> Expr {
    Expr::Literal(Literal::String(StringLiteral {
        body: body.to_string(),
        kind,
        interned,
    }))
}

/// Construct a boolean [`Expr::Literal`].
#[must_use]
pub fn lit_bool(b: bool) -> Expr {
    Expr::Literal(Literal::Bool(b))
}
