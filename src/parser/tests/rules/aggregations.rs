//! Tests for aggregation term detection in rule bodies.

use super::super::helpers::parse_ok;
use crate::parser::ast::{AggregationSource, Expr, RuleBodyTerm};
use crate::test_util::{assert_parse_error, call, var};

/// Assert that `body_terms()` reports the expected arity error for an
/// aggregation literal found in `src`.
fn assert_aggregation_arity_error(src: &str, literal: &str, expected_error: &str) {
    let parsed = parse_ok(src);
    #[expect(clippy::expect_used, reason = "tests require a single rule")]
    let rule = parsed
        .root()
        .rules()
        .first()
        .cloned()
        .expect("rule missing");
    let errors = match rule.body_terms() {
        Ok(terms) => panic!("expected aggregation arity error, got {terms:?}"),
        Err(errs) => errs,
    };
    let Some(start) = src.find(literal) else {
        panic!("{literal} literal missing");
    };
    let end = start + literal.len();
    assert_parse_error(&errors, expected_error, start, end);
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
