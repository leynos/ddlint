//! Aggregated valid-expression fixtures for parser tests.
//!
//! Each child module covers one syntactic area, and this module combines those
//! groups into the ordered case lists exercised by the expression parser suite.

mod basic;
mod collections;
mod control_flow;
mod postfix;
mod structured;

use crate::parser::tests::expression::fixtures::ExpressionCase;

/// Returns all valid expression test cases by combining fixture groups.
///
/// The returned vector is composed of the basic, collection, struct/closure,
/// postfix, and control-flow fixtures in the ordering used by the parser test
/// suite.
pub(super) fn expression_cases() -> Vec<ExpressionCase> {
    let mut cases = basic::basic_expression_cases();
    cases.extend(collections::collection_expression_cases());
    cases.extend(structured::struct_and_closure_cases());
    cases.extend(postfix::postfix_expression_cases());
    cases.extend(control_flow::control_flow_expression_cases());
    cases
}

/// Returns invalid `for`-loop source fixtures.
///
/// The returned fixed-size array contains malformed source strings used by
/// expression parse error tests.
pub(super) fn invalid_for_loop_sources() -> [&'static str; 4] {
    basic::invalid_for_loop_sources()
}

/// Returns literal expression fixtures for parser tests.
///
/// These cases are used where tests only need literal forms (strings, booleans,
/// and numbers) and is delegated from the basic fixture module.
pub(super) fn literal_cases() -> Vec<ExpressionCase> {
    basic::literal_cases()
}
