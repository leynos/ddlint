//! Round-trip tests for rule parsing.

use super::super::helpers::{parse_err, parse_ok, pretty_print};
use crate::parser::ast::AstNode;
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
