//! Tests covering invalid rule parsing scenarios.

use super::super::helpers::parse_err;
use crate::test_util::{ErrorPattern, assert_parse_error};
use rstest::rstest;

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
