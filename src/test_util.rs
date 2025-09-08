//! Helpers for constructing expression nodes and asserting parser errors in
//! tests.
//!
//! These functions reduce boilerplate when asserting over [`Expr`] nodes and
//! verifying that parsing failures surface precise spans and messages.

use crate::{
    Span, SyntaxKind,
    parser::ast::{Expr, Literal},
    tokenize_with_trivia,
};
use chumsky::error::{Simple, SimpleReason};
use std::ops::Range;

/// Tokenise `src` into `(SyntaxKind, Span)` pairs using
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

/// Common error message patterns for parser assertions.
#[derive(Debug, Clone)]
pub enum ErrorPattern {
    Custom(String),
}

/// Map parser token identifiers to readable names.
const TOKEN_MAP: &[(&str, &str)] = &[
    ("T_LPAREN", "left paren"),
    ("T_RPAREN", "right paren"),
    ("T_LBRACKET", "left bracket"),
    ("T_RBRACKET", "right bracket"),
    ("T_LBRACE", "left brace"),
    ("T_RBRACE", "right brace"),
    ("T_COMMA", "comma"),
    ("T_SEMI", "semicolon"),
    ("T_PIPE", "pipe"),
    ("T_DOT", "dot"),
    ("T_COLON", "colon"),
    ("T_IDENT", "identifier"),
    ("T_NUMBER", "number"),
];

/// Replace internal token names with human-readable forms.
fn normalise_tokens(s: &str) -> String {
    TOKEN_MAP
        .iter()
        .fold(s.to_string(), |acc, (raw, human)| acc.replace(raw, human))
}

impl ErrorPattern {
    fn contains_message(&self, rendered: &str) -> bool {
        let rendered = normalise_tokens(rendered);
        match self {
            Self::Custom(msg) => {
                let msg = normalise_tokens(msg);
                rendered.contains(&msg)
            }
        }
    }
}

impl From<&str> for ErrorPattern {
    fn from(s: &str) -> Self {
        Self::Custom(s.to_string())
    }
}

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
///
/// Accepts any type convertible into [`Name`].
#[must_use]
pub fn var(name: impl Into<Name>) -> Expr {
    let name: Name = name.into();
    Expr::Variable(name.0)
}

/// Construct a function call [`Expr::Call`].
///
/// Accepts any type convertible into [`Name`] for the function name.
#[must_use]
pub fn call(name: impl Into<Name>, args: Vec<Expr>) -> Expr {
    let name: Name = name.into();
    Expr::Call {
        callee: Box::new(Expr::Variable(name.0)),
        args,
    }
}

/// Construct a call expression [`Expr::Call`].
#[must_use]
pub fn call_expr(callee: Expr, args: Vec<Expr>) -> Expr {
    Expr::Call {
        callee: Box::new(callee),
        args,
    }
}

pub fn method_call(recv: Expr, name: impl Into<Name>, args: Vec<Expr>) -> Expr {
    let name: Name = name.into();
    Expr::MethodCall {
        recv: Box::new(recv),
        name: name.0,
        args,
    }
}

/// Construct a field access [`Expr::FieldAccess`].
#[must_use]
pub fn field_access(expr: Expr, field: impl Into<Name>) -> Expr {
    let field: Name = field.into();
    Expr::FieldAccess {
        expr: Box::new(expr),
        field: field.0,
    }
}

/// Construct a tuple index [`Expr::TupleIndex`].
#[must_use]
pub fn tuple_index(expr: Expr, index: &str) -> Expr {
    Expr::TupleIndex {
        expr: Box::new(expr),
        index: index.to_string(),
    }
}

/// Construct a bit slice [`Expr::BitSlice`].
#[must_use]
pub fn bit_slice(expr: Expr, hi: Expr, lo: Expr) -> Expr {
    Expr::BitSlice {
        expr: Box::new(expr),
        hi: Box::new(hi),
        lo: Box::new(lo),
    }
}

/// Construct a call expression [`Expr::Call`].
#[must_use]
pub fn struct_expr(name: impl Into<Name>, fields: Vec<(String, Expr)>) -> Expr {
    let name: Name = name.into();
    Expr::Struct {
        name: name.0,
        fields,
    }
}

/// Convenience to build a struct field tuple.
#[must_use]
pub fn field(name: impl Into<Name>, expr: Expr) -> (String, Expr) {
    let name: Name = name.into();
    (name.0, expr)
}

/// Construct a tuple literal [`Expr::Tuple`].
#[must_use]
pub fn tuple(items: Vec<Expr>) -> Expr {
    Expr::Tuple(items)
}

/// Construct a closure literal [`Expr::Closure`].
///
/// Accepts any iterable of parameter names.
#[must_use]
pub fn closure<P, S>(params: P, body: Expr) -> Expr
where
    P: IntoIterator<Item = S>,
    S: AsRef<str>,
{
    Expr::Closure {
        params: params.into_iter().map(|p| p.as_ref().to_string()).collect(),
        body: Box::new(body),
    }
}

/// Assert that a parser produced no errors.
///
/// # Examples
///
/// ```
/// use ddlint::test_util::assert_no_parse_errors;
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

/// Assert that the parser produced exactly one error matching
/// `expected_pattern` and span.
///
/// # Examples
///
/// ```
/// use chumsky::error::Simple;
/// use ddlint::{test_util::assert_parse_error, SyntaxKind};
///
/// let err: Simple<SyntaxKind> = Simple::custom(0..1, "oops");
/// assert_parse_error(&[err], "oops", 0, 1);
/// ```
///
/// # Panics
/// Panics if `errors` is empty or the message or span do not match.
#[track_caller]
#[expect(clippy::expect_used, reason = "test helpers use expect for clarity")]
pub fn assert_parse_error(
    errors: &[Simple<SyntaxKind>],
    expected_pattern: impl Into<ErrorPattern>,
    start: usize,
    end: usize,
) {
    let pattern: ErrorPattern = expected_pattern.into();
    assert_eq!(errors.len(), 1, "expected one error, got {errors:?}");
    let error = errors.first().expect("error missing");
    let rendered = format!("{error:?}");
    assert!(
        pattern.contains_message(&rendered),
        "expected error to contain pattern '{pattern:?}', got '{rendered}'",
    );
    assert_eq!(error.span(), start..end);
}

#[track_caller]
#[expect(clippy::expect_used, reason = "test helpers use expect for clarity")]
fn assert_delimiter_error_impl<'a>(
    errors: &'a [Simple<SyntaxKind>],
    expected_pattern: &ErrorPattern,
    span: Range<usize>,
) -> &'a Simple<SyntaxKind> {
    let error = errors.first().expect("error missing");
    let rendered = format!("{error:?}");
    assert!(
        expected_pattern.contains_message(&rendered),
        "expected error to contain pattern '{expected_pattern:?}', got '{rendered}'",
    );
    assert_eq!(error.span(), span);
    error
}

/// Kinds of delimiter-related parser errors.
#[derive(Debug, Clone, Copy)]
enum DelimiterErrorKind {
    /// The encountered closing delimiter did not match the expected opener.
    Mismatch,
    /// A delimiter was opened but never closed.
    Unclosed,
}

impl DelimiterErrorKind {
    fn reason_check(self, reason: &SimpleReason<SyntaxKind, Range<usize>>) -> bool {
        match self {
            Self::Mismatch => matches!(reason, SimpleReason::Unexpected),
            Self::Unclosed => matches!(
                reason,
                SimpleReason::Unclosed { .. } | SimpleReason::Custom(_)
            ),
        }
    }

    const fn description(self) -> &'static str {
        match self {
            Self::Mismatch => "delimiter mismatch",
            Self::Unclosed => "unclosed delimiter",
        }
    }
}

/// Assert that a delimiter error matches `expected_pattern` and has the
/// specified [`DelimiterErrorKind`].
#[track_caller]
fn assert_delimiter_error_of_kind(
    errors: &[Simple<SyntaxKind>],
    expected_pattern: impl Into<ErrorPattern>,
    span: Range<usize>,
    kind: DelimiterErrorKind,
) {
    let pattern: ErrorPattern = expected_pattern.into();
    let error = assert_delimiter_error_impl(errors, &pattern, span);
    assert!(
        kind.reason_check(error.reason()),
        "expected {}, got {:?}",
        kind.description(),
        error.reason()
    );
}

/// Assert that a parser error indicates a delimiter mismatch.
///
/// # Panics
/// Panics if `errors` is empty or the error does not match `expected_pattern`.
#[track_caller]
pub fn assert_delimiter_error(
    errors: &[Simple<SyntaxKind>],
    expected_pattern: impl Into<ErrorPattern>,
    start: usize,
    end: usize,
) {
    assert_delimiter_error_of_kind(
        errors,
        expected_pattern,
        start..end,
        DelimiterErrorKind::Mismatch,
    );
}

/// Assert that a parser error indicates an unclosed delimiter.
///
/// This verifies the error span points to the opening delimiter and that no
/// closing token was found while matching the expected pattern.
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
    assert_delimiter_error_of_kind(
        errors,
        expected_pattern,
        start..end,
        DelimiterErrorKind::Unclosed,
    );
}
