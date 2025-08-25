//! Helpers for integration tests.
//!
//! Lightweight stand-ins for the crate's internal test helpers so integration
//! tests can assert over expressions and parser errors without enabling the
//! optional `test-util` feature.

#![allow(dead_code, reason = "not every test uses all helpers")]

use chumsky::error::{Simple, SimpleReason};
use ddlint::{
    SyntaxKind,
    parser::ast::{Expr, Literal},
};
use std::ops::Range;

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

/// Construct a variable [`Expr::Variable`].
#[must_use]
pub fn var(name: &str) -> Expr {
    Expr::Variable(name.into())
}

/// Construct a function call [`Expr::Call`].
#[must_use]
pub fn call(name: &str, args: Vec<Expr>) -> Expr {
    Expr::Call {
        name: name.into(),
        args,
    }
}

#[expect(clippy::expect_used, reason = "helpers panic with clear message")]
fn assert_error_contains(errors: &[Simple<SyntaxKind>], pattern: &str, span: Range<usize>) {
    let error = errors.first().expect("error missing");
    let rendered = format!("{error:?}");
    assert!(
        rendered.contains(pattern),
        "expected error to contain pattern '{pattern}', got '{rendered}'",
    );
    assert_eq!(error.span(), span);
}

/// Assert that the parser produced the expected error message and span.
#[track_caller]
pub fn assert_parse_error(errors: &[Simple<SyntaxKind>], pattern: &str, start: usize, end: usize) {
    assert_error_contains(errors, pattern, start..end);
}

#[expect(clippy::expect_used, reason = "helpers panic with clear message")]
fn assert_delim_reason(errors: &[Simple<SyntaxKind>]) {
    let reason = errors.first().expect("error missing").reason();
    assert!(
        matches!(reason, SimpleReason::Unexpected | SimpleReason::Custom(_)),
        "expected delimiter error, got {reason:?}",
    );
}

/// Assert that a parser error indicates a delimiter mismatch.
#[track_caller]
pub fn assert_delimiter_error(
    errors: &[Simple<SyntaxKind>],
    pattern: &str,
    start: usize,
    end: usize,
) {
    assert_error_contains(errors, pattern, start..end);
    assert_delim_reason(errors);
}

/// Assert that a parser error indicates an unclosed delimiter.
#[track_caller]
pub fn assert_unclosed_delimiter_error(
    errors: &[Simple<SyntaxKind>],
    pattern: &str,
    start: usize,
    end: usize,
) {
    assert_error_contains(errors, pattern, start..end);
    assert_delim_reason(errors);
}
