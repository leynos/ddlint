//! Behavioural tests for the aggregation extraction boundary.
//!
//! These tests verify that `parse()` does not surface rule-body aggregation
//! misuse through `Parsed::errors()` or `Parsed::semantic_rules()`, and that
//! callers must request `Rule::body_terms()` to trigger aggregation
//! classification and validation.

use ddlint::parse;
use ddlint::parser::ast::Rule;

/// Parse source and extract the first rule while asserting the parser defers
/// aggregation validation to rule-body semantic helpers.
#[expect(clippy::expect_used, reason = "test helper expects a single rule")]
fn parse_rule_without_parse_stage_aggregation_errors(src: &str) -> Rule {
    let parsed = parse(src);
    assert!(
        parsed.errors().is_empty(),
        "parse() should not surface aggregation misuse: {:?}",
        parsed.errors()
    );
    assert!(
        parsed.semantic_rules().is_empty(),
        "rule-body aggregations must not produce parse-time semantic rules"
    );
    parsed
        .root()
        .rules()
        .first()
        .cloned()
        .expect("rule missing")
}

fn assert_body_terms_error_contains(rule: &Rule, expected_message: &str) {
    let errors = match rule.body_terms() {
        Ok(terms) => panic!("expected body_terms error, got {terms:?}"),
        Err(errs) => errs,
    };
    let has_expected_error = errors
        .iter()
        .any(|error| format!("{error:?}").contains(expected_message));
    assert!(
        has_expected_error,
        "expected body_terms() error containing `{expected_message}`, got {errors:?}"
    );
}

#[test]
fn parse_defers_duplicate_group_by_error_until_body_terms() {
    let rule = parse_rule_without_parse_stage_aggregation_errors(
        "X(x) :- group_by(sum(x), k), group_by(count(x), k).",
    );
    assert_body_terms_error_contains(
        &rule,
        "at most one aggregation (group_by or Aggregate) is permitted per rule body",
    );
}

#[test]
fn parse_defers_legacy_aggregate_arity_error_until_body_terms() {
    let rule = parse_rule_without_parse_stage_aggregation_errors(
        "Totals(user, total) :- \
         Orders(user, amt), \
         Aggregate((user), sum(amt), extra_arg), \
         total = __group.",
    );
    assert_body_terms_error_contains(&rule, "Aggregate expects exactly two arguments");
}
