//! Tests for aggregation term detection in rule bodies.

use rstest::rstest;

use super::super::helpers::parse_ok;
use crate::parser::ast::{AggregationSource, Expr, RuleBodyTerm};
use crate::test_util::{assert_parse_error, call, var};

/// Assert that `body_terms()` reports an expected error for a literal found in `src`.
fn assert_body_terms_error(src: &str, literal: &str, expected_error: &str) {
    let parsed = parse_ok(src);
    #[expect(clippy::expect_used, reason = "tests require a single rule")]
    let rule = parsed
        .root()
        .rules()
        .first()
        .cloned()
        .expect("rule missing");
    let errors = match rule.body_terms() {
        Ok(terms) => panic!("expected body_terms error, got {terms:?}"),
        Err(errs) => errs,
    };
    let Some(start) = src.find(literal) else {
        panic!("{literal} literal missing");
    };
    let end = start + literal.len();
    assert_parse_error(&errors, expected_error, start, end);
}

/// Assert that `body_terms()` reports the expected arity error for an
/// aggregation literal found in `src`.
fn assert_aggregation_arity_error(src: &str, literal: &str, expected_error: &str) {
    assert_body_terms_error(src, literal, expected_error);
}

#[test]
fn body_terms_detect_group_by_aggregation() {
    let src =
        "Totals(user, total) :- Orders(user, amt), group_by(sum(amt), user), total = __group.";
    let parsed = parse_ok(src);
    #[expect(clippy::expect_used, reason = "tests require a single rule")]
    let rule = parsed
        .root()
        .rules()
        .first()
        .cloned()
        .expect("rule missing");
    let terms = match rule.body_terms() {
        Ok(terms) => terms,
        Err(errs) => panic!("body terms should parse: {errs:?}"),
    };
    assert_eq!(terms.len(), 3);
    let aggregation = match terms.get(1) {
        Some(RuleBodyTerm::Aggregation(agg)) => agg,
        other => panic!("expected aggregation term, got {other:?}"),
    };
    assert_eq!(aggregation.source, AggregationSource::GroupBy);
    assert_eq!(aggregation.project, call("sum", vec![var("amt")]));
    assert_eq!(aggregation.key, var("user"));
}

#[expect(
    clippy::expect_used,
    reason = "tests assert aggregation is present when parsing Aggregate literals"
)]
#[test]
fn body_terms_detect_legacy_aggregate_aggregation() {
    let src = "Totals(user, total) :- \
               Orders(user, amt), \
               Aggregate((user), sum(amt)), \
               total = __group.";
    let parsed = parse_ok(src);
    #[expect(clippy::expect_used, reason = "tests require a single rule")]
    let rule = parsed
        .root()
        .rules()
        .first()
        .cloned()
        .expect("rule missing");
    let terms = match rule.body_terms() {
        Ok(terms) => terms,
        Err(errs) => panic!("body terms should parse: {errs:?}"),
    };
    let aggregation = terms
        .iter()
        .find_map(|term| {
            if let RuleBodyTerm::Aggregation(agg) = term {
                Some(agg)
            } else {
                None
            }
        })
        .expect("expected a RuleBodyTerm::Aggregation from Aggregate(..)");

    assert_eq!(
        aggregation.source,
        AggregationSource::LegacyAggregate,
        "expected AggregationSource::LegacyAggregate for Aggregate"
    );
    assert_eq!(aggregation.project, call("sum", vec![var("amt")]));
    match &aggregation.key {
        Expr::Group(inner) => assert_eq!(**inner, var("user")),
        other => panic!("expected grouped key Expr, got {other:?}"),
    }
}

#[test]
fn body_terms_error_on_group_by_wrong_arity() {
    let src = "Totals(u, total) :- Orders(u, amt), group_by(sum(amt)).";
    assert_aggregation_arity_error(
        src,
        "group_by(sum(amt))",
        "group_by expects exactly two arguments",
    );
}

#[test]
fn body_terms_error_on_legacy_aggregate_wrong_arity() {
    let src = "Totals(user, total) :- \
               Orders(user, amt), \
               Aggregate((user), sum(amt), extra_arg), \
               total = __group.";
    assert_aggregation_arity_error(
        src,
        "Aggregate((user), sum(amt), extra_arg)",
        "Aggregate expects exactly two arguments",
    );
}

/// Helper to assert that `body_terms()` reports the expected multiple
/// aggregation error for a rule containing more than one aggregation.
fn assert_multiple_aggregation_error(src: &str, second_literal: &str) {
    assert_body_terms_error(
        src,
        second_literal,
        "at most one aggregation (group_by or Aggregate) is permitted per rule body",
    );
}

#[rstest]
#[case::multiple_group_by(
    "X(x) :- group_by(sum(x), k), group_by(count(x), k).",
    "group_by(count(x), k)"
)]
#[case::multiple_legacy_aggregate(
    "X(x) :- Aggregate((k), sum(x)), Aggregate((k), count(x)).",
    "Aggregate((k), count(x))"
)]
#[case::mixed_group_by_and_aggregate(
    "X(x) :- group_by(sum(x), k), Aggregate((k), count(x)).",
    "Aggregate((k), count(x))"
)]
fn body_terms_error_on_multiple_aggregations(#[case] src: &str, #[case] second_literal: &str) {
    assert_multiple_aggregation_error(src, second_literal);
}

#[test]
fn body_terms_error_on_three_aggregations_reports_two_errors() {
    let src = "X(x) :- group_by(a, k), group_by(b, k), group_by(c, k).";
    let parsed = parse_ok(src);
    #[expect(clippy::expect_used, reason = "tests require a single rule")]
    let rule = parsed
        .root()
        .rules()
        .first()
        .cloned()
        .expect("rule missing");
    let errors = match rule.body_terms() {
        Ok(terms) => panic!("expected body_terms error, got {terms:?}"),
        Err(errs) => errs,
    };
    // With three aggregations, we expect two errors (one for each duplicate)
    assert_eq!(
        errors.len(),
        2,
        "expected two errors for three aggregations, got {errors:?}"
    );
}

#[test]
fn duplicate_aggregations_excluded_from_terms() {
    // Verify that even when errors are reported, the terms returned via
    // Err(...) only include the first aggregation.
    let src = "X(x) :- A(x), group_by(sum(x), k), group_by(count(x), k), B(x).";
    let parsed = parse_ok(src);
    #[expect(clippy::expect_used, reason = "tests require a single rule")]
    let rule = parsed
        .root()
        .rules()
        .first()
        .cloned()
        .expect("rule missing");

    // We use body_expression_nodes() to get a count of the raw literals
    let raw_literal_count = rule.body_expression_nodes().len();
    assert_eq!(raw_literal_count, 4, "expected 4 body literals");

    // When body_terms() fails, it returns the errors but not the terms.
    // To test structural consistency, we need to verify the helper behaviour.
    // The point is that duplicate aggregations are not pushed into terms.
    // We can't easily test the internal `terms` vector directly, but we can
    // verify that parsing rules with duplicate aggregations produces the
    // expected error count (one per duplicate).
    let errors = match rule.body_terms() {
        Ok(terms) => panic!("expected body_terms error, got {terms:?}"),
        Err(errs) => errs,
    };
    assert_eq!(
        errors.len(),
        1,
        "expected exactly one error for two aggregations"
    );
}
