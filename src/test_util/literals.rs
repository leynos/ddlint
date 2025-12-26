//! Helpers for constructing literal expression nodes in tests.

use crate::parser::ast::{Expr, Literal, StringKind, StringLiteral};
use crate::parser::expression::parse_numeric_literal;

/// Construct a numeric [`Expr::Literal`].
#[must_use]
pub fn lit_num(n: &str) -> Expr {
    let literal = parse_numeric_literal(n)
        .unwrap_or_else(|err| panic!("failed to parse numeric literal '{n}': {}", err.message()));
    Expr::Literal(Literal::Number(literal))
}

/// Construct a string [`Expr::Literal`].
#[must_use]
pub fn lit_str(s: &str) -> Expr {
    string_literal(
        s,
        StringKind::Standard {
            interpolated: false,
        },
        false,
    )
}

/// Construct an interpolated string [`Expr::Literal`].
#[must_use]
pub fn lit_interpolated_str(s: &str) -> Expr {
    string_literal(s, StringKind::Standard { interpolated: true }, false)
}

/// Construct a raw string [`Expr::Literal`].
#[must_use]
pub fn lit_raw_str(s: &str) -> Expr {
    string_literal(
        s,
        StringKind::Raw {
            interpolated: false,
        },
        false,
    )
}

/// Construct a raw interpolated string [`Expr::Literal`].
#[must_use]
pub fn lit_raw_interpolated_str(s: &str) -> Expr {
    string_literal(s, StringKind::Raw { interpolated: true }, false)
}

/// Construct an interned standard string [`Expr::Literal`].
#[must_use]
pub fn lit_interned_str(s: &str) -> Expr {
    string_literal(
        s,
        StringKind::Standard {
            interpolated: false,
        },
        true,
    )
}

/// Construct an interned raw string [`Expr::Literal`].
#[must_use]
pub fn lit_interned_raw_str(s: &str) -> Expr {
    string_literal(
        s,
        StringKind::Raw {
            interpolated: false,
        },
        true,
    )
}

/// Construct an interned interpolated raw string [`Expr::Literal`].
#[must_use]
pub fn lit_interned_raw_interpolated_str(s: &str) -> Expr {
    string_literal(s, StringKind::Raw { interpolated: true }, true)
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
