//! Fallible setup shared by rule-behaviour integration tests.

use ddlint::parse;
use ddlint::parser::ast::Rule;
use rstest::fixture;

/// Parse source and return its first rule, reporting syntax and lookup failures.
///
/// For example, `parse_and_extract_rule("R(x) :- Source(x).")` yields the first
/// rule, while `parse_and_extract_rule("")` returns `Err("rule missing")`.
pub(super) fn parse_and_extract_rule(src: &str) -> Result<Rule, String> {
    let parsed = parse(src);
    if !parsed.errors().is_empty() {
        return Err(format!("unexpected parse errors: {:?}", parsed.errors()));
    }
    parsed
        .root()
        .rules()
        .first()
        .cloned()
        .ok_or_else(|| String::from("rule missing"))
}

#[test]
fn rule_setup_reports_parse_errors() {
    use std::collections::BTreeSet;

    let src = "UserLogin(username, session_id) :- .";
    let error = parse_and_extract_rule(src).expect_err("invalid source should fail setup");
    // Chumsky stores expected tokens in a HashSet, so its Debug order is unstable.
    let (details, expected_and_tail) = error
        .split_once(", expected: {")
        .expect("parse error should include its expected-token set");
    let (expected_tokens, tail) = expected_and_tail
        .split_once("}, found: ")
        .expect("parse error should include the found token and label");
    assert_eq!(
        details,
        concat!(
            "unexpected parse errors: [Simple { ",
            "span: 35..36, reason: Unexpected"
        )
    );
    assert_eq!(tail, "Some(T_DOT), label: None }]");
    let actual_tokens: BTreeSet<_> = expected_tokens.split(", ").collect();
    let expected_tokens = BTreeSet::from([
        "Some(T_IDENT)",
        "Some(T_LBRACKET)",
        "Some(K_SKIP)",
        "Some(T_LPAREN)",
        "Some(K_IF)",
        "Some(T_LBRACE)",
        "Some(K_FOR)",
        "Some(T_AMP)",
    ]);
    assert_eq!(actual_tokens, expected_tokens);
}

#[test]
fn rule_setup_reports_missing_rule() {
    let error = parse_and_extract_rule("").expect_err("empty source should not contain a rule");
    assert_eq!(error, "rule missing");
}

/// Shared source for mixed-term classification and flattened-order checks.
const MIXED_RULE_SOURCE: &str = "R(x, y) :- Pre(x), for (y in Items(y)) Process(y), Post(x, y).";

/// Provide a shared rule for classification and flattened-order tests.
///
/// For example, `#[from(mixed_rule)]` supplies a body with a pre-loop
/// expression, a for-loop term, and a post-loop expression; flattening yields
/// `Pre`, `Items`, `Process`, then `Post`.
#[fixture]
pub(super) fn mixed_rule() -> Result<Rule, String> {
    parse_and_extract_rule(MIXED_RULE_SOURCE)
}
