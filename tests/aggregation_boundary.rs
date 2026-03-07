//! Behavioural tests for the aggregation extraction boundary.
//!
//! These tests verify that `parse()` does not surface rule-body aggregation
//! misuse through `Parsed::errors()` or `Parsed::semantic_rules()`, and that
//! callers must request `Rule::body_terms()` to trigger aggregation
//! classification and validation.

use ddlint::parse;
use ddlint::parser::ast::Rule;
use ddlint::test_util::{
    assert_custom_parse_error_contains, assert_first_rule_without_parse_stage_aggregation_errors,
};

fn assert_body_terms_error_contains(rule: &Rule, expected_message: &str) {
    let errors = match rule.body_terms() {
        Ok(terms) => panic!("expected body_terms error, got {terms:?}"),
        Err(errs) => errs,
    };
    assert_custom_parse_error_contains(&errors, expected_message);
}

#[test]
fn parse_defers_duplicate_group_by_error_until_body_terms() {
    let parsed = parse("X(x) :- group_by(sum(x), k), group_by(count(x), k).");
    let rule = assert_first_rule_without_parse_stage_aggregation_errors(&parsed);
    assert_body_terms_error_contains(
        &rule,
        "at most one aggregation (group_by or Aggregate) is permitted per rule body",
    );
}

#[test]
fn parse_defers_legacy_aggregate_arity_error_until_body_terms() {
    let parsed = parse(
        "Totals(user, total) :- \
         Orders(user, amt), \
         Aggregate((user), sum(amt), extra_arg), \
         total = __group.",
    );
    let rule = assert_first_rule_without_parse_stage_aggregation_errors(&parsed);
    assert_body_terms_error_contains(&rule, "Aggregate expects exactly two arguments");
}
