//! Behavioural tests for index declaration grammar alignment.
//!
//! Verifies that the public `ddlint::parse` entrypoint accepts the canonical
//! spec-form index declaration and rejects the legacy shorthand.

use ddlint::parse;
use ddlint::test_util::{assert_custom_parse_error_contains, assert_no_parse_errors};

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

#[test]
fn legacy_index_shorthand_is_rejected() {
    let parsed = parse("index OrdersByUser on Orders(user, ts)");
    assert_custom_parse_error_contains(
        parsed.errors(),
        "index declarations require a typed field list `(name: Type, ...)` before `on`",
    );
    assert!(parsed.root().indexes().is_empty());
}

#[test]
fn empty_index_field_list_is_rejected() {
    let parsed = parse("index OrdersByUser() on Orders[user]");
    assert_custom_parse_error_contains(
        parsed.errors(),
        "index declarations require one or more typed fields in the form `name: Type`",
    );
    assert!(parsed.root().indexes().is_empty());
}

#[test]
fn malformed_index_field_list_is_rejected() {
    let parsed = parse("index OrdersByUser(user) on Orders[user]");
    assert_custom_parse_error_contains(
        parsed.errors(),
        "index declarations require one or more typed fields in the form `name: Type`",
    );
    assert!(parsed.root().indexes().is_empty());
}
