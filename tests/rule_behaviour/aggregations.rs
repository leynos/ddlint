//! Rule body classification and aggregation diagnostics.

use ddlint::parser::ast::{AggregationSource, RuleBodyTerm};

use super::{assert_body_terms_error_contains, assert_body_terms_ok, parse_and_extract_rule};

#[test]
fn parses_rules_with_flatmap_and_group_by_terms() {
    let src = "Totals(user, total) :- Orders(user, amt), var rows = FlatMap(fetch_rows(amt)), group_by(sum(amt), user), total = __group.";
    let rule = parse_and_extract_rule(src).expect("source should parse and contain a rule");
    let terms = assert_body_terms_ok(&rule);
    assert_eq!(terms.len(), 4);
    assert!(matches!(terms.get(1), Some(RuleBodyTerm::Assignment(_))));
    assert!(matches!(
        terms.get(2),
        Some(RuleBodyTerm::Aggregation(agg)) if agg.source == AggregationSource::GroupBy
    ));
    assert!(matches!(terms.get(3), Some(RuleBodyTerm::Assignment(_))));
}

#[test]
fn rule_body_terms_non_aggregation_classification() {
    let src = "NonAgg(x) :- some_predicate(1, 2), var x = FlatMap(some_function(3)), another_predicate(x, 4).";
    let rule = parse_and_extract_rule(src).expect("source should parse and contain a rule");
    let terms = assert_body_terms_ok(&rule);
    assert_eq!(terms.len(), 3);
    assert!(matches!(terms.first(), Some(RuleBodyTerm::Expression(_))));
    assert!(matches!(terms.get(1), Some(RuleBodyTerm::Assignment(_))));
    assert!(matches!(terms.get(2), Some(RuleBodyTerm::Expression(_))));
}

#[test]
fn rejects_multiple_group_by_in_rule_body() {
    let src = "X(x) :- group_by(sum(x), k), group_by(count(x), k).";
    let rule = parse_and_extract_rule(src).expect("source should parse and contain a rule");
    assert_body_terms_error_contains(
        &rule,
        "at most one aggregation (group_by or Aggregate) is permitted per rule body",
    );
}

#[expect(
    clippy::indexing_slicing,
    reason = "test expects specific literals and span structures"
)]
#[test]
fn multiple_aggregation_error_spans_point_to_correct_locations() {
    // End-to-end test verifying the error span points to the second aggregation.
    let src = "X(x) :- group_by(sum(x), k), group_by(count(x), k).";
    let rule = parse_and_extract_rule(src).expect("source should parse and contain a rule");

    let errors = rule.body_terms().expect_err("expected errors");
    assert_eq!(errors.len(), 1, "expected exactly one error");

    // Compute expected span for second aggregation
    let second_agg = "group_by(count(x), k)";
    let second_start = src.find(second_agg).expect("second agg missing");
    let second_end = second_start + second_agg.len();

    let error = &errors[0];
    assert_eq!(
        error.span(),
        second_start..second_end,
        "error span should point to second aggregation"
    );
}

#[test]
fn rejects_mixed_aggregation_types_in_rule_body() {
    // Test that mixing group_by and Aggregate also triggers the error.
    let src = "X(x) :- Aggregate((k), sum(x)), group_by(count(x), k).";
    let rule = parse_and_extract_rule(src).expect("source should parse and contain a rule");
    assert_body_terms_error_contains(
        &rule,
        "at most one aggregation (group_by or Aggregate) is permitted per rule body",
    );
}

#[test]
fn three_aggregations_produce_two_errors() {
    // When three aggregations appear, we should get two errors (one for each duplicate).
    let src = "X(x) :- group_by(a, k), group_by(b, k), group_by(c, k).";
    let rule = parse_and_extract_rule(src).expect("source should parse and contain a rule");

    let errors = rule.body_terms().expect_err("expected errors");
    assert_eq!(
        errors.len(),
        2,
        "expected two errors for three aggregations"
    );
}

#[test]
fn parses_legacy_aggregate_and_normalizes_arguments() {
    let src =
        "Totals(user, total) :- Orders(user, amt), Aggregate((user), sum(amt)), total = __group.";
    let rule = parse_and_extract_rule(src).expect("source should parse and contain a rule");
    let terms = assert_body_terms_ok(&rule);
    // Find the aggregation term.
    let aggregation = terms.iter().find_map(|term| {
        if let RuleBodyTerm::Aggregation(agg) = term {
            Some(agg)
        } else {
            None
        }
    });
    let agg = aggregation.expect("expected aggregation term");
    // Verify legacy Aggregate is recognized.
    assert_eq!(agg.source, AggregationSource::LegacyAggregate);
    // Verify argument normalization: project is sum(amt), key is (user).
    // Note: (user) parses as a grouped expression.
    assert_eq!(agg.project.to_sexpr(), "(call sum amt)");
    assert_eq!(agg.key.to_sexpr(), "(group user)");
}
