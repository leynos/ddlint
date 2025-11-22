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
