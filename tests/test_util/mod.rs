//! Helpers for integration tests.
//!
//! Lightweight stand-ins for the crate's internal test helpers so integration
//! tests can assert over expressions and parser errors without any feature
//! flags.

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
const _: fn(&str) -> Expr = lit_num;

/// Construct a string [`Expr::Literal`].
#[must_use]
pub fn lit_str(s: &str) -> Expr {
    Expr::Literal(Literal::String(s.into()))
}
const _: fn(&str) -> Expr = lit_str;

/// Construct a boolean [`Expr::Literal`].
#[must_use]
pub fn lit_bool(b: bool) -> Expr {
    Expr::Literal(Literal::Bool(b))
}
const _: fn(bool) -> Expr = lit_bool;

/// Construct a variable [`Expr::Variable`].
#[must_use]
pub fn var(name: &str) -> Expr {
    Expr::Variable(name.into())
}
const _: fn(&str) -> Expr = var;

/// Construct a function call [`Expr::Call`].
#[must_use]
pub fn call(name: &str, args: Vec<Expr>) -> Expr {
    Expr::Call {
        name: name.into(),
        args,
    }
}
const _: fn(&str, Vec<Expr>) -> Expr = call;

/// Assert that a parser produced no errors.
///
/// # Examples
///
/// ```ignore
/// use test_util::assert_no_parse_errors;
/// let errors: Vec<chumsky::error::Simple<ddlint::SyntaxKind>> = Vec::new();
/// assert_no_parse_errors(&errors);
/// ```
///
/// # Panics
/// Panics if `errors` is not empty.
#[track_caller]
pub fn assert_no_parse_errors<E: std::fmt::Debug>(errors: &[E]) {
    assert!(errors.is_empty(), "Parse errors: {errors:?}");
}
const _: fn(&[()]) = assert_no_parse_errors::<()>;

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
const _: fn(&[Simple<SyntaxKind>], &str, Range<usize>) = assert_error_contains;

/// Assert that the parser produced the expected error message and span.
#[track_caller]
pub fn assert_parse_error(errors: &[Simple<SyntaxKind>], pattern: &str, start: usize, end: usize) {
    assert_error_contains(errors, pattern, start..end);
}
const _: fn(&[Simple<SyntaxKind>], &str, usize, usize) = assert_parse_error;

#[expect(clippy::expect_used, reason = "helpers panic with clear message")]
fn assert_delim_reason(errors: &[Simple<SyntaxKind>]) {
    let reason = errors.first().expect("error missing").reason();
    assert!(
        matches!(reason, SimpleReason::Unexpected | SimpleReason::Custom(_)),
        "expected delimiter error, got {reason:?}",
    );
}
const _: fn(&[Simple<SyntaxKind>]) = assert_delim_reason;

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
const _: fn(&[Simple<SyntaxKind>], &str, usize, usize) = assert_delimiter_error;

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
const _: fn(&[Simple<SyntaxKind>], &str, usize, usize) = assert_unclosed_delimiter_error;
