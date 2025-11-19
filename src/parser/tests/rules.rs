//! Rule parsing tests.
//!
//! Ensures rule heads, bodies, and error cases are handled.

use super::helpers::{parse_err, parse_ok, pretty_print};
use crate::parser::ast::{AggregationSource, AstNode, RuleBodyTerm};
use crate::test_util::{ErrorPattern, assert_parse_error, call, var};
use rstest::{fixture, rstest};

#[fixture]
fn simple_rule() -> &'static str {
    "ActiveUser(user_id) :- User(user_id, _, true)."
}

#[fixture]
fn multi_literal_rule() -> &'static str {
    "UserLogin(username, session_id) :- User(user_id, username, _), UserSession(user_id, session_id, _)."
}

#[fixture]
fn fact_rule() -> &'static str {
    "SystemAlert(\"System is now online.\")."
}

#[fixture]
fn for_loop_rule() -> &'static str {
    "ItemsProcessed(count) :- for (entry in Items(entry)) Process(entry)."
}

#[fixture]
fn for_loop_with_if_iterable() -> &'static str {
    "ItemsProcessed(count) :- for (entry in if cond(entry) { Items(entry) } else { Others(entry) }) Process(entry)."
}

#[fixture]
fn for_loop_with_guard_rule() -> &'static str {
    "FilteredItems(entry) :- for (entry in Items(entry) if true) Process(entry)."
}

#[fixture]
fn nested_for_loop_rule() -> &'static str {
    "PairProcessed(a, b) :- for (a in A(a)) for (b in B(b)) ProcessPair(a, b)."
}

#[rstest]
#[case::simple_rule(simple_rule(), false)]
#[case::multi_literal_rule(multi_literal_rule(), false)]
#[case::fact_rule(fact_rule(), false)]
#[case::for_loop_rule(for_loop_rule(), false)]
#[case::for_loop_if_iterable(for_loop_with_if_iterable(), false)]
#[case::for_loop_with_guard(for_loop_with_guard_rule(), false)]
#[case::nested_for_loop(nested_for_loop_rule(), false)]
fn rule_parsing_tests(#[case] rule_input: &str, #[case] should_have_errors: bool) {
    let parsed = if should_have_errors {
        parse_err(rule_input)
    } else {
        parse_ok(rule_input)
    };
    let rules = parsed.root().rules();
    assert_eq!(rules.len(), 1);
    #[expect(clippy::expect_used, reason = "tests require a single rule")]
    let rule = rules.first().expect("rule missing");
    assert_eq!(
        pretty_print(rule.syntax()),
        rule_input,
        "round trip mismatch"
    );
}

#[test]
fn body_terms_capture_flatmap_assignments() {
    let src = "Flat(ip) :- Source(addrs), var ip = FlatMap(extract_ips(addrs)).";
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
    assert_eq!(terms.len(), 2);
    let assignment = match terms.get(1) {
        Some(RuleBodyTerm::Assignment(assign)) => assign,
        other => panic!("expected assignment term, got {other:?}"),
    };
    assert_eq!(assignment.pattern, "var ip");
    assert_eq!(
        assignment.value,
        call("FlatMap", vec![call("extract_ips", vec![var("addrs")])])
    );
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

#[test]
fn body_terms_error_on_group_by_wrong_arity() {
    let src = "Totals(u, total) :- Orders(u, amt), group_by(sum(amt)).";
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
    let literal = "group_by(sum(amt))";
    let Some(start) = src.find(literal) else {
        panic!("group_by literal missing");
    };
    let end = start + literal.len();
    assert_parse_error(
        &errors,
        "group_by expects exactly two arguments",
        start,
        end,
    );
}

#[rstest]
#[case(
    ":- User(user_id, username, _).",
    ErrorPattern::from("Unexpected"),
    0,
    2
)]
#[case(
    "UserLogin(username, session_id) :- .",
    ErrorPattern::from("Unexpected"),
    35,
    36
)]
#[case(
    "UserLogin(username, session_id) User(user_id, username, _).",
    ErrorPattern::from("Unexpected"),
    32,
    36
)]
#[case(
    "UserLogin(username, session_id) :- User(user_id, username, _)",
    ErrorPattern::from("Unexpected"),
    0,
    61
)]
#[case("This is not a rule!", ErrorPattern::from("Unexpected"), 5, 7)]
fn invalid_rule_cases(
    #[case] input: &str,
    #[case] pattern: ErrorPattern,
    #[case] start: usize,
    #[case] end: usize,
) {
    let parsed = parse_err(input);
    let errors = parsed.errors();
    #[expect(clippy::expect_used, reason = "tests expect at least one parse error")]
    let first = errors.first().expect("expected parse error");
    assert_parse_error(std::slice::from_ref(first), pattern, start, end);
}
