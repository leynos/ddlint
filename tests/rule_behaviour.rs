//! Behavioural tests covering rule parsing with aggregations and `FlatMap` constructs.

use ddlint::parse;
use ddlint::parser::ast::{AggregationSource, RuleBodyTerm};

#[test]
fn parses_rules_with_flatmap_and_group_by_terms() {
    let src =
        "Totals(user, total) :- Orders(user, amt), group_by(sum(amt), user), total = __group.";
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
    assert_eq!(terms.len(), 3);
    assert!(matches!(
        terms.get(1),
        Some(RuleBodyTerm::Aggregation(agg)) if agg.source == AggregationSource::GroupBy
    ));
    assert!(matches!(terms.get(2), Some(RuleBodyTerm::Assignment(_))));
}
