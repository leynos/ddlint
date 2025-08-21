//! Rule parsing tests.
//!
//! Ensures rule heads, bodies, and error cases are handled.

use super::common::pretty_print;
use crate::parse;
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

#[rstest]
#[case::simple_rule(simple_rule(), true)]
#[case::multi_literal_rule(multi_literal_rule(), true)]
#[case::fact_rule(fact_rule(), false)]
fn rule_parsing_tests(#[case] rule_input: &str, #[case] should_have_errors: bool) {
    let parsed = parse(rule_input);
    if should_have_errors {
        assert!(
            !parsed.errors().is_empty(),
            "rule parsing expected to emit errors until implementation is complete",
        );
    } else {
        assert!(parsed.errors().is_empty());
    }
    let rules = parsed.root().rules();
    assert_eq!(rules.len(), 1);
    let Some(rule) = rules.first() else {
        panic!("rule missing");
    };
    assert_eq!(pretty_print(rule.syntax()), rule_input);
}

#[test]
fn invalid_rule_missing_head() {
    let input = ":- User(user_id, username, _).";
    let parsed = parse(input);
    assert!(
        !parsed.errors().is_empty(),
        "Expected errors for missing head",
    );
}

#[test]
fn invalid_rule_missing_body() {
    let input = "UserLogin(username, session_id) :- .";
    let parsed = parse(input);
    assert!(
        !parsed.errors().is_empty(),
        "Expected errors for missing body",
    );
}

#[test]
fn invalid_rule_no_colon_dash() {
    let input = "UserLogin(username, session_id) User(user_id, username, _).";
    let parsed = parse(input);
    assert!(
        !parsed.errors().is_empty(),
        "Expected errors for missing ':-'",
    );
}

#[test]
fn invalid_rule_missing_period() {
    let input = "UserLogin(username, session_id) :- User(user_id, username, _)";
    let parsed = parse(input);
    assert!(
        !parsed.errors().is_empty(),
        "Expected errors for missing period at end",
    );
}

#[test]
fn invalid_rule_garbage() {
    let input = "This is not a rule!";
    let parsed = parse(input);
    assert!(
        !parsed.errors().is_empty(),
        "Expected errors for completely invalid input",
    );
}
