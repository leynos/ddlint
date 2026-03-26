//! Behavioural tests for index declaration grammar alignment.
//!
//! Verifies that the public `ddlint::parse` entrypoint accepts the canonical
//! spec-form index declaration and rejects the legacy shorthand.

use ddlint::parse;
use ddlint::test_util::{assert_custom_parse_error_contains, assert_no_parse_errors};
use rstest::rstest;

#[test]
fn canonical_index_declaration_parses() -> Result<(), Box<dyn std::error::Error>> {
    let parsed = parse("index OrdersByUser(user: UserId, ts: Timestamp) on Orders[user, ts]");
    assert_no_parse_errors(parsed.errors());

    let indexes = parsed.root().indexes();
    assert_eq!(indexes.len(), 1, "expected one parsed index");
    let index = indexes.first().ok_or("index missing")?;
    assert_eq!(index.name().as_deref(), Some("OrdersByUser"));
    assert_eq!(
        index.fields(),
        Ok(vec![
            ("user".to_string(), "UserId".to_string()),
            ("ts".to_string(), "Timestamp".to_string()),
        ])
    );
    assert_eq!(index.on_target().as_deref(), Some("Orders[user,ts]"));
    Ok(())
}

#[rstest]
#[case(
    "index OrdersByUser on Orders(user, ts)",
    "index declarations require a typed field list `(name: Type, ...)` before `on`"
)]
#[case(
    "index OrdersByUser() on Orders[user]",
    "index declarations require one or more typed fields in the form `name: Type`"
)]
#[case(
    "index OrdersByUser(user) on Orders[user]",
    "index declarations require one or more typed fields in the form `name: Type`"
)]
fn invalid_index_declarations_are_rejected(
    #[case] input: &str,
    #[case] expected_error: &str,
) {
    let parsed = parse(input);
    assert_custom_parse_error_contains(parsed.errors(), expected_error);
    assert!(parsed.root().indexes().is_empty());
}
