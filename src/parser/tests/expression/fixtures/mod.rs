//! Shared fixture definitions and accessors for expression parser tests.
//!
//! This module defines the case types used by the valid and error fixture
//! groups, then exposes the grouped lists consumed by the parent expression
//! parser test modules.

mod errors;
mod valid;

use crate::parser::ast::Expr;

pub(super) struct ExpressionCase {
    /// Source snippet under test.
    ///
    /// This should be a complete expression string the parser attempts to
    /// consume for the corresponding `expected` AST.
    pub(super) src: &'static str,
    /// Expected AST produced from parsing `src`.
    ///
    /// Tests compare parser output against this value for structural
    /// correctness, not just string rendering.
    pub(super) expected: Expr,
}

pub(super) struct CountedErrorCase {
    /// Source snippet expected to fail parsing.
    ///
    /// This case is validated by asserting the parser emits at least
    /// `min_errs` errors.
    pub(super) src: &'static str,
    /// Minimum number of errors that must be reported for this failure case.
    pub(super) min_errs: usize,
}

pub(super) struct SpannedErrorCase {
    /// Source snippet expected to fail parsing with a specific span.
    ///
    /// Tests verify both error message and span boundaries against the expected
    /// `start` and `end`.
    pub(super) src: &'static str,
    /// Exact message fragment expected from the parser error.
    pub(super) msg: &'static str,
    /// Inclusive start offset of the expected error span.
    pub(super) start: usize,
    /// Exclusive end offset of the expected error span.
    pub(super) end: usize,
}

pub(super) struct PostfixErrorCase {
    /// Source snippet exercising postfix parsing errors.
    ///
    /// Includes a dedicated `unclosed` flag for cases that expect an unmatched
    /// trailing delimiter.
    pub(super) src: &'static str,
    /// Exact message fragment expected from the parser error.
    pub(super) msg: &'static str,
    /// Inclusive start offset of the expected error span.
    pub(super) start: usize,
    /// Exclusive end offset of the expected error span.
    pub(super) end: usize,
    /// Indicates that the syntax was expected to reach an unclosed delimiter.
    pub(super) unclosed: bool,
}

/// Returns the combined valid expression fixtures consumed by parser tests.
///
/// The returned cases are sourced from `valid::expression_cases()` and include
/// all syntax categories currently covered by valid-expression fixtures.
pub(super) fn expression_cases() -> Vec<ExpressionCase> {
    valid::expression_cases()
}

/// Returns invalid `for`-loop source fixtures used by parser error tests.
///
/// The returned array is forwarded from `valid::invalid_for_loop_sources()` and
/// contains source strings intentionally designed to trigger parse failures.
pub(super) fn invalid_for_loop_sources() -> [&'static str; 4] {
    valid::invalid_for_loop_sources()
}

/// Returns literal expression fixtures for parser happy-path validation.
///
/// The returned cases are delegated from `valid::literal_cases()`.
pub(super) fn literal_cases() -> Vec<ExpressionCase> {
    valid::literal_cases()
}

/// Returns counted parse error fixtures for general expression error coverage.
///
/// The returned cases are delegated from `errors::error_cases()` and encode the
/// minimum number of diagnostics expected per input.
pub(super) fn error_cases() -> Vec<CountedErrorCase> {
    errors::error_cases()
}

/// Returns span-specific pattern-matching error fixtures.
///
/// The returned cases come from `errors::match_pattern_error_cases()` and
/// assert exact message and span on parse failures.
pub(super) fn match_pattern_error_cases() -> Vec<SpannedErrorCase> {
    errors::match_pattern_error_cases()
}

/// Returns span-specific postfix parse error fixtures.
///
/// The returned cases come from `errors::postfix_error_cases()` and include an
/// `unclosed` marker used for unterminated postfix delimiters.
pub(super) fn postfix_error_cases() -> Vec<PostfixErrorCase> {
    errors::postfix_error_cases()
}
