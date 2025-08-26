//! Rule parsing tests.
//!
//! Ensures rule heads, bodies, and error cases are handled.

use super::helpers::{parse_err, parse_ok, pretty_print};
use crate::parser::ast::AstNode;
use crate::test_util::{ErrorPattern, assert_parse_error};
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
