//! Helpers for constructing expression nodes and asserting parser errors in
//! tests.
//!
//! These functions reduce boilerplate when asserting over [`Expr`] nodes and
//! verifying that parsing failures surface precise spans and messages.

use crate::{
    SyntaxKind,
    parser::ast::{Expr, Literal},
};
use chumsky::error::Simple;

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

/// Assert that the parser produced exactly one error with the expected message
/// and span.
///
/// # Examples
///
/// ```
/// use chumsky::error::Simple;
/// use ddlint::{test_util::assert_parse_error, SyntaxKind};
///
/// let err = Simple::custom(0..1, "oops");
/// assert_parse_error(&[err], "oops", 0, 1);
/// ```
///
/// # Panics
/// Panics if `errors` is empty or the message or span do not match.
#[track_caller]
#[expect(clippy::expect_used, reason = "test helpers use expect for clarity")]
pub fn assert_parse_error(
    errors: &[Simple<SyntaxKind>],
    expected_msg: &str,
    start: usize,
    end: usize,
) {
    assert_eq!(errors.len(), 1, "expected one error, got {errors:?}");
    let error = errors.first().expect("error missing");
    let rendered = format!("{error:?}");
    assert!(
        rendered.contains(expected_msg),
        "expected error to contain '{expected_msg}', got '{rendered}'"
    );
    assert_eq!(error.span(), start..end);
}

/// Assert that a parser error indicates a delimiter mismatch.
///
/// This is a thin wrapper over [`assert_parse_error`] that also checks the error
/// reports a found token.
///
/// # Panics
/// Panics if `errors` is empty or the error kind does not indicate a mismatch.
#[track_caller]
#[expect(clippy::expect_used, reason = "test helpers use expect for clarity")]
pub fn assert_delimiter_error(
    errors: &[Simple<SyntaxKind>],
    expected_msg: &str,
    start: usize,
    end: usize,
) {
    use chumsky::error::SimpleReason;

    assert_parse_error(errors, expected_msg, start, end);
    let error = errors.first().expect("error missing");
    assert!(
        matches!(
            error.reason(),
            SimpleReason::Unexpected | SimpleReason::Custom(_)
        ),
        "expected delimiter mismatch, got {:?}",
        error.reason()
    );
}

/// Assert that a parser error indicates an unclosed delimiter.
///
/// This verifies the error span points to the opening delimiter and that no
/// closing token was found.
///
/// # Panics
/// Panics if `errors` is empty or the error kind does not indicate an unclosed
/// delimiter.
#[track_caller]
#[expect(clippy::expect_used, reason = "test helpers use expect for clarity")]
pub fn assert_unclosed_delimiter_error(
    errors: &[Simple<SyntaxKind>],
    expected_msg: &str,
    start: usize,
    end: usize,
) {
    use chumsky::error::SimpleReason;

    assert_parse_error(errors, expected_msg, start, end);
    let error = errors.first().expect("error missing");
    assert!(
        matches!(
            error.reason(),
            SimpleReason::Unexpected | SimpleReason::Custom(_)
        ),
        "expected unclosed delimiter, got {:?}",
        error.reason()
    );
}
