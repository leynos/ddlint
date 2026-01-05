//! Assertion helpers for verifying parser errors in tests.

use super::{ErrorPattern, normalise_tokens};
use crate::SyntaxKind;
use chumsky::error::{Simple, SimpleReason};
use std::ops::Range;

/// Assert that `f` panics and return its panic message.
///
/// # Examples
///
/// ```rust,no_run
/// # #[cfg(feature = "test-support")]
/// # {
/// use ddlint::test_util::assert_panic_with_message;
/// let message = assert_panic_with_message(|| panic!("boom"));
/// assert!(message.contains("boom"));
/// # }
/// ```
///
/// # Panics
/// Panics if `f` does not panic.
#[track_caller]
pub fn assert_panic_with_message<F>(f: F) -> String
where
    F: FnOnce() + std::panic::UnwindSafe,
{
    let result = std::panic::catch_unwind(f);
    let Err(err) = result else {
        panic!("expected panic");
    };
    err.downcast_ref::<String>()
        .cloned()
        .or_else(|| err.downcast_ref::<&str>().map(|s| (*s).to_string()))
        .unwrap_or_default()
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
/// Panics if `errors` is empty. It also panics when the message fails to
/// match. The same applies if the span differs.
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
    let rendered_normalised = normalise_tokens(&rendered);
    let pattern_normalised = match &pattern {
        ErrorPattern::Custom(msg) => normalise_tokens(msg),
    };
    assert!(
        rendered_normalised.contains(&pattern_normalised),
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
    let rendered_normalised = normalise_tokens(&rendered);
    let pattern_normalised = match expected_pattern {
        ErrorPattern::Custom(msg) => normalise_tokens(msg),
    };
    assert!(
        rendered_normalised.contains(&pattern_normalised),
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
            Self::Mismatch => match reason {
                SimpleReason::Unexpected => true,
                SimpleReason::Custom(msg) if msg.starts_with("unexpected token") => true,
                _ => false,
            },
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

/// Generate a delimiter assertion wrapper function.
macro_rules! delimiter_error_assertion {
    ($name:ident, $kind:expr, $doc:expr) => {
        #[doc = $doc]
        #[track_caller]
        pub fn $name(
            errors: &[Simple<SyntaxKind>],
            expected_pattern: impl Into<ErrorPattern>,
            start: usize,
            end: usize,
        ) {
            assert_delimiter_error_of_kind(errors, expected_pattern, start..end, $kind);
        }
    };
}

delimiter_error_assertion!(
    assert_delimiter_error,
    DelimiterErrorKind::Mismatch,
    "Assert that a parser error indicates a delimiter mismatch."
);

delimiter_error_assertion!(
    assert_unclosed_delimiter_error,
    DelimiterErrorKind::Unclosed,
    "Assert that a parser error indicates an unclosed delimiter.\n\n\
     This verifies the error span points to the opening delimiter, and that no\n\
     closing token was found whilst matching the expected pattern."
);
