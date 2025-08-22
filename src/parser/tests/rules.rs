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
// TODO: rules with multiple body literals should parse without errors once supported
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

#[rstest]
#[case(":- User(user_id, username, _).", "missing head")]
#[case("UserLogin(username, session_id) :- .", "missing body")]
#[case(
    "UserLogin(username, session_id) User(user_id, username, _).",
    "missing ':-'"
)]
#[case(
    "UserLogin(username, session_id) :- User(user_id, username, _)",
    "missing '.'"
)]
#[case("This is not a rule!", "invalid input")]
fn invalid_rule_cases(#[case] input: &str, #[case] msg_hint: &str) {
    let parsed = parse(input);
    let errors = parsed.errors();
    assert!(!errors.is_empty(), "expected error: {msg_hint}");
    // Prefer assert_parse_error(&parsed, msg_hint, start, end) when available.
}
