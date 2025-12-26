//! Behavioural tests covering rule parsing with aggregations and `FlatMap` constructs.

use ddlint::parse;
use ddlint::parser::ast::{AggregationSource, Rule, RuleBodyTerm};

/// Parse source and extract the first rule, asserting no CST errors occur.
#[expect(clippy::expect_used, reason = "test helper expects valid source")]
fn parse_and_extract_rule(src: &str) -> Rule {
    let parsed = parse(src);
    assert!(
        parsed.errors().is_empty(),
        "unexpected parse errors: {:?}",
        parsed.errors()
    );
    parsed
        .root()
        .rules()
        .first()
        .cloned()
        .expect("rule missing")
}

/// Call `body_terms()` expecting `Ok`, returning terms on success or panicking.
fn assert_body_terms_ok(rule: &Rule) -> Vec<RuleBodyTerm> {
    match rule.body_terms() {
        Ok(terms) => terms,
        Err(errs) => panic!("body terms should parse: {errs:?}"),
    }
}

/// Call `body_terms()` expecting `Err`, asserting the error list contains `expected_msg`.
#[expect(clippy::expect_used, reason = "test helper asserts error presence")]
fn assert_body_terms_error_contains(rule: &Rule, expected_msg: &str) {
    let result = rule.body_terms();
    assert!(result.is_err(), "expected body_terms error");
    let errors = result.expect_err("expected errors");
    let has_error = errors
        .iter()
        .any(|e| format!("{e:?}").contains(expected_msg));
    assert!(
        has_error,
        "expected error containing '{expected_msg}', got: {errors:?}"
    );
}

#[test]
fn parses_rules_with_flatmap_and_group_by_terms() {
    let src = "Totals(user, total) :- Orders(user, amt), var rows = FlatMap(fetch_rows(amt)), group_by(sum(amt), user), total = __group.";
    let rule = parse_and_extract_rule(src);
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
    let rule = parse_and_extract_rule(src);
    let terms = assert_body_terms_ok(&rule);
    assert_eq!(terms.len(), 3);
    assert!(matches!(terms.first(), Some(RuleBodyTerm::Expression(_))));
    assert!(matches!(terms.get(1), Some(RuleBodyTerm::Assignment(_))));
    assert!(matches!(terms.get(2), Some(RuleBodyTerm::Expression(_))));
}

#[test]
fn parses_multi_head_rules_with_adornments_and_ref_new_lowering() {
    let src = "A(a)@site(x) -<10>, B'(b), &RefOrder{ id: id, amt: amt } :- Src(a, b, id, amt).";
    let parsed = parse(src);
    assert!(
        parsed.errors().is_empty(),
        "unexpected parse errors: {:?}",
        parsed.errors()
    );
    #[expect(clippy::expect_used, reason = "test expects a single rule")]
    let rule = parsed
        .root()
        .rules()
        .first()
        .cloned()
        .expect("rule missing");

    let heads = match rule.heads() {
        Ok(heads) => heads,
        Err(errs) => panic!("heads should parse: {errs:?}"),
    };
    assert_eq!(heads.len(), 3);

    let Some(first) = heads.first() else {
        panic!("missing first head");
    };
    assert_eq!(
        first.atom.to_sexpr(),
        "(delay 10 (call A a))",
        "first head should include delay"
    );
    let Some(location) = first.location.as_ref() else {
        panic!("location missing");
    };
    assert_eq!(location.to_sexpr(), "(call site x)");

    let Some(second) = heads.get(1) else {
        panic!("missing second head");
    };
    assert_eq!(second.atom.to_sexpr(), "(diff (call B b))");

    let Some(third) = heads.get(2) else {
        panic!("missing third head");
    };
    assert_eq!(
        third.atom.to_sexpr(),
        "(call ref_new (struct RefOrder (id id) (amt amt)))"
    );
}

#[test]
fn rejects_multiple_group_by_in_rule_body() {
    let src = "X(x) :- group_by(sum(x), k), group_by(count(x), k).";
    let rule = parse_and_extract_rule(src);
    assert_body_terms_error_contains(
        &rule,
        "at most one aggregation (group_by or Aggregate) is permitted per rule body",
    );
}

#[expect(
    clippy::expect_used,
    clippy::indexing_slicing,
    reason = "test expects specific literals and span structures"
)]
#[test]
fn multiple_aggregation_error_spans_point_to_correct_locations() {
    // End-to-end test verifying the error span points to the second aggregation.
    let src = "X(x) :- group_by(sum(x), k), group_by(count(x), k).";
    let rule = parse_and_extract_rule(src);

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
    let rule = parse_and_extract_rule(src);
    assert_body_terms_error_contains(
        &rule,
        "at most one aggregation (group_by or Aggregate) is permitted per rule body",
    );
}

#[expect(clippy::expect_used, reason = "test expects specific error structures")]
#[test]
fn three_aggregations_produce_two_errors() {
    // When three aggregations appear, we should get two errors (one for each duplicate).
    let src = "X(x) :- group_by(a, k), group_by(b, k), group_by(c, k).";
    let rule = parse_and_extract_rule(src);

    let errors = rule.body_terms().expect_err("expected errors");
    assert_eq!(
        errors.len(),
        2,
        "expected two errors for three aggregations"
    );
}

#[expect(clippy::expect_used, reason = "test asserts aggregation presence")]
#[test]
fn parses_legacy_aggregate_and_normalizes_arguments() {
    let src =
        "Totals(user, total) :- Orders(user, amt), Aggregate((user), sum(amt)), total = __group.";
    let rule = parse_and_extract_rule(src);
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
