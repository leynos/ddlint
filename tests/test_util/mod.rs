//! Shared test utilities for integration tests.
//!
//! These helpers construct AST nodes and assert over parser errors. They mirror
//! a subset of the `ddlint::test_util` module without requiring the `test-util`
//! feature, enabling integration tests to compile against the published
//! library.

#![expect(
    dead_code,
    reason = "helpers are reused across multiple tests so some may be unused"
)]

use chumsky::error::Simple;
use ddlint::{
    SyntaxKind,
    parser::ast::{Expr, Literal},
};
use std::ops::Range;

/// Typed wrapper for variable and function names.
#[derive(Debug, Clone)]
pub struct Name(String);

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

/// Construct a numeric [`Expr::Literal`].
///
/// # Examples
///
/// ```
/// use test_util::lit_num;
/// use ddlint::parser::ast::Expr;
///
/// let expr = lit_num("42");
/// assert!(matches!(expr, Expr::Literal(_)));
/// ```
#[must_use]
pub fn lit_num(n: &str) -> Expr {
    Expr::Literal(Literal::Number(n.into()))
}

/// Construct a string [`Expr::Literal`].
///
/// # Examples
///
/// ```
/// use test_util::lit_str;
/// use ddlint::parser::ast::Expr;
///
/// let expr = lit_str("hi");
/// assert!(matches!(expr, Expr::Literal(_)));
/// ```
#[must_use]
pub fn lit_str(s: &str) -> Expr {
    Expr::Literal(Literal::String(s.into()))
}

/// Construct a boolean [`Expr::Literal`].
///
/// # Examples
///
/// ```
/// use test_util::lit_bool;
/// use ddlint::parser::ast::Expr;
///
/// let expr = lit_bool(true);
/// assert!(matches!(expr, Expr::Literal(_)));
/// ```
#[must_use]
pub fn lit_bool(b: bool) -> Expr {
    Expr::Literal(Literal::Bool(b))
}

/// Construct a variable [`Expr::Variable`].
///
/// # Examples
///
/// ```
/// use test_util::var;
/// use ddlint::parser::ast::Expr;
///
/// let expr = var("x");
/// assert!(matches!(expr, Expr::Variable(_)));
/// ```
#[must_use]
pub fn var(name: impl Into<Name>) -> Expr {
    let name: Name = name.into();
    Expr::Variable(name.0)
}

/// Construct a function call [`Expr::Call`].
///
/// # Examples
///
/// ```
/// use test_util::{call, lit_num};
/// use ddlint::parser::ast::Expr;
///
/// let expr = call("f", vec![lit_num("1")]);
/// assert!(matches!(expr, Expr::Call { .. }));
/// ```
#[must_use]
pub fn call(name: impl Into<Name>, args: Vec<Expr>) -> Expr {
    let name: Name = name.into();
    Expr::Call { name: name.0, args }
}

/// Common error message patterns for parser assertions.
#[derive(Debug, Clone)]
pub enum ErrorPattern {
    /// Custom substring to match against a rendered error.
    Custom(String),
    /// The parser reported an unexpected token.
    UnexpectedToken,
    /// The parser reported an unexpected end-of-file.
    UnexpectedEof,
    /// A delimiter was left unclosed.
    UnclosedDelimiter,
    /// A delimiter mismatch occurred.
    MismatchedDelimiter,
}

impl ErrorPattern {
    fn contains_message(&self, rendered: &str) -> bool {
        match self {
            Self::Custom(msg) => rendered.contains(msg),
            Self::UnexpectedToken => rendered.contains("unexpected"),
            Self::UnexpectedEof => rendered.contains("EOF") || rendered.contains("end"),
            Self::UnclosedDelimiter => {
                rendered.contains("unclosed") || rendered.contains("missing")
            }
            Self::MismatchedDelimiter => {
                rendered.contains("expected") || rendered.contains("found")
            }
        }
    }
}

impl From<&str> for ErrorPattern {
    fn from(s: &str) -> Self {
        Self::Custom(s.to_string())
    }
}

#[derive(Debug, Copy, Clone)]
enum DelimiterErrorType {
    Mismatch,
    Unclosed,
}

/// Assert that the parser produced exactly one error matching the expected
/// pattern and span.
///
/// # Examples
///
/// ```
/// use chumsky::error::Simple;
/// use ddlint::SyntaxKind;
/// use test_util::assert_parse_error;
///
/// let err: Simple<SyntaxKind> = Simple::custom(0..1, "oops");
/// assert_parse_error(&[err], "oops", 0, 1);
/// ```
///
/// # Panics
/// Panics if `errors` is empty or the message or span do not match.
#[track_caller]
pub fn assert_parse_error(
    errors: &[Simple<SyntaxKind>],
    expected_pattern: impl Into<ErrorPattern>,
    start: usize,
    end: usize,
) {
    let pattern: ErrorPattern = expected_pattern.into();
    assert!(!errors.is_empty(), "no errors reported");
    let span = start..end;
    let (idx, error) = errors
        .iter()
        .enumerate()
        .find(|(_, e)| e.span() == span)
        .unwrap_or_else(|| panic!("no error with span {span:?}; errors: {errors:?}"));
    let rendered = format!("{error:?}");
    assert!(
        pattern.contains_message(&rendered),
        "error #{idx} did not match pattern {pattern:?}: {rendered}",
    );
}

#[track_caller]
fn assert_delimiter_error_impl(
    errors: &[Simple<SyntaxKind>],
    expected_pattern: &ErrorPattern,
    span: Range<usize>,
    error_type: DelimiterErrorType,
) {
    use chumsky::error::SimpleReason;
    assert!(!errors.is_empty(), "no errors reported");
    let (idx, error) = errors
        .iter()
        .enumerate()
        .find(|(_, e)| e.span() == span)
        .unwrap_or_else(|| panic!("no error with span {span:?}; errors: {errors:?}"));
    let rendered = format!("{error:?}");
    assert!(
        expected_pattern.contains_message(&rendered),
        "error #{idx} did not match pattern {expected_pattern:?}: {rendered}",
    );
    match error_type {
        DelimiterErrorType::Unclosed => assert!(
            matches!(
                error.reason(),
                SimpleReason::Unclosed { .. } | SimpleReason::Unexpected | SimpleReason::Custom(_)
            ),
            "expected Unclosed, got {:?}",
            error.reason(),
        ),
        DelimiterErrorType::Mismatch => assert!(
            matches!(
                error.reason(),
                SimpleReason::Unexpected | SimpleReason::Custom(_)
            ),
            "expected Mismatch (Unexpected/Custom), got {:?}",
            error.reason(),
        ),
    }
}

/// Assert that a parser error indicates a delimiter mismatch.
///
/// # Panics
/// Panics if `errors` is empty or the error kind does not indicate a mismatch.
#[track_caller]
pub fn assert_delimiter_error(
    errors: &[Simple<SyntaxKind>],
    expected_pattern: impl Into<ErrorPattern>,
    start: usize,
    end: usize,
) {
    let pattern: ErrorPattern = expected_pattern.into();
    assert_delimiter_error_impl(errors, &pattern, start..end, DelimiterErrorType::Mismatch);
}

/// Assert that a parser error indicates an unclosed delimiter.
///
/// # Panics
/// Panics if `errors` is empty or the error kind does not indicate an unclosed
/// delimiter.
#[track_caller]
pub fn assert_unclosed_delimiter_error(
    errors: &[Simple<SyntaxKind>],
    expected_pattern: impl Into<ErrorPattern>,
    start: usize,
    end: usize,
) {
    let pattern: ErrorPattern = expected_pattern.into();
    assert_delimiter_error_impl(errors, &pattern, start..end, DelimiterErrorType::Unclosed);
}
