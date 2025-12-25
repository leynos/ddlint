//! Behavioural tests covering rule parsing with aggregations and `FlatMap` constructs.

use ddlint::parse;
use ddlint::parser::ast::{AggregationSource, RuleBodyTerm};

#[test]
fn parses_rules_with_flatmap_and_group_by_terms() {
    let src = "Totals(user, total) :- Orders(user, amt), var rows = FlatMap(fetch_rows(amt)), group_by(sum(amt), user), total = __group.";
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
    let terms = match rule.body_terms() {
        Ok(terms) => terms,
        Err(errs) => panic!("body terms should parse: {errs:?}"),
    };
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
    let source = "NonAgg(x) :- some_predicate(1, 2), var x = FlatMap(some_function(3)), another_predicate(x, 4).";

    let parsed = parse(source);
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
    let terms = match rule.body_terms() {
        Ok(terms) => terms,
        Err(errs) => panic!("body terms should parse: {errs:?}"),
    };

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
    let parsed = parse(src);
    assert!(
        parsed.errors().is_empty(),
        "CST parse should succeed: {:?}",
        parsed.errors()
    );
    #[expect(clippy::expect_used, reason = "test expects a single rule")]
    let rule = parsed
        .root()
        .rules()
        .first()
        .cloned()
        .expect("rule missing");
    let result = rule.body_terms();
    assert!(result.is_err(), "expected multiple aggregation error");
    #[expect(clippy::expect_used, reason = "test asserts error presence")]
    let errors = result.expect_err("expected errors");
    let has_error = errors.iter().any(|e| {
        format!("{e:?}")
            .contains("at most one aggregation (group_by or Aggregate) is permitted per rule")
    });
    assert!(
        has_error,
        "expected 'at most one aggregation' error, got: {errors:?}"
    );
}

#[test]
fn parses_legacy_aggregate_and_normalizes_arguments() {
    let src =
        "Totals(user, total) :- Orders(user, amt), Aggregate((user), sum(amt)), total = __group.";
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
    let terms = match rule.body_terms() {
        Ok(terms) => terms,
        Err(errs) => panic!("body terms should parse: {errs:?}"),
    };
    // Find the aggregation term.
    let aggregation = terms.iter().find_map(|term| {
        if let RuleBodyTerm::Aggregation(agg) = term {
            Some(agg)
        } else {
            None
        }
    });
    #[expect(clippy::expect_used, reason = "test asserts aggregation presence")]
    let agg = aggregation.expect("expected aggregation term");
    // Verify legacy Aggregate is recognized.
    assert_eq!(agg.source, AggregationSource::LegacyAggregate);
    // Verify argument normalization: project is sum(amt), key is (user).
    // Note: (user) parses as a grouped expression.
    assert_eq!(agg.project.to_sexpr(), "(call sum amt)");
    assert_eq!(agg.key.to_sexpr(), "(group user)");
}
