//! Helpers for constructing literal expressions in tests.
//!
//! These functions reduce boilerplate when asserting over [`Expr`] nodes.

use crate::parser::ast::{Expr, Literal};

/// Construct a numeric [`Expr::Literal`].
#[must_use]
pub fn lit_num(n: &str) -> Expr {
    Expr::Literal(Literal::Number(n.into()))
}

/// Construct a string [`Expr::Literal`].
#[must_use]
pub fn lit_str(s: &str) -> Expr {
    Expr::Literal(Literal::String(s.into()))
}

/// Construct a boolean [`Expr::Literal`].
#[must_use]
pub fn lit_bool(b: bool) -> Expr {
    Expr::Literal(Literal::Bool(b))
}
