//! Shared fixture data for expression parser tests.

mod errors;
mod valid;

use crate::parser::ast::Expr;

pub(super) struct ExpressionCase {
    pub(super) src: &'static str,
    pub(super) expected: Expr,
}

pub(super) struct CountedErrorCase {
    pub(super) src: &'static str,
    pub(super) min_errs: usize,
}

pub(super) struct SpannedErrorCase {
    pub(super) src: &'static str,
    pub(super) msg: &'static str,
    pub(super) start: usize,
    pub(super) end: usize,
}

pub(super) struct PostfixErrorCase {
    pub(super) src: &'static str,
    pub(super) msg: &'static str,
    pub(super) start: usize,
    pub(super) end: usize,
    pub(super) unclosed: bool,
}

pub(super) fn expression_cases() -> Vec<ExpressionCase> {
    valid::expression_cases()
}

pub(super) fn invalid_for_loop_sources() -> [&'static str; 4] {
    valid::invalid_for_loop_sources()
}

pub(super) fn literal_cases() -> Vec<ExpressionCase> {
    valid::literal_cases()
}

pub(super) fn error_cases() -> Vec<CountedErrorCase> {
    errors::error_cases()
}

pub(super) fn match_pattern_error_cases() -> Vec<SpannedErrorCase> {
    errors::match_pattern_error_cases()
}

pub(super) fn postfix_error_cases() -> Vec<PostfixErrorCase> {
    errors::postfix_error_cases()
}
