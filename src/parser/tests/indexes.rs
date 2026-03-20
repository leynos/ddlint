//! Index declaration parsing tests.
//!
//! These tests cover single and multi-column indexes and error cases.

use super::helpers::{normalize_whitespace, parse_index, pretty_print};
use crate::test_util::{
    assert_custom_parse_error_contains, assert_no_parse_errors, assert_parse_error,
};
use rstest::{fixture, rstest};

#[fixture]
fn index_single_column() -> &'static str {
    "index Idx_User_username(username: string) on User[username]"
}

#[fixture]
fn index_multi_column() -> &'static str {
    "index Idx_Session_user_time(user_id: u32, start_time: u64) on UserSession[user_id, start_time]"
}

#[fixture]
fn index_invalid_missing_on() -> &'static str {
    "index Idx_Invalid(username: string) User[username]"
}

#[fixture]
fn index_bracket_target() -> &'static str {
    "index Idx_lower_username(username: string) on User[lower(username)]"
}

#[fixture]
fn index_legacy_shorthand() -> &'static str {
    "index Idx_Legacy on User(username)"
}

#[fixture]
fn index_whitespace_variations() -> &'static str {
    "  index  Idx_User_ws (\n    username: string\n  ) \t on\n  User [\n    username  ]  "
}

#[rstest]
#[case(
    index_single_column(),
    "Idx_User_username",
    vec![("username".to_string(), "string".to_string())],
    "User[username]"
)]
#[case(
    index_multi_column(),
    "Idx_Session_user_time",
    vec![
        ("user_id".to_string(), "u32".to_string()),
        ("start_time".to_string(), "u64".to_string())
    ],
    "UserSession[user_id,start_time]"
)]
#[case(
    index_bracket_target(),
    "Idx_lower_username",
    vec![("username".to_string(), "string".to_string())],
    "User[lower(username)]"
)]
fn parses_indexes(
    #[case] src: &str,
    #[case] name: &str,
    #[case] fields: Vec<(String, String)>,
    #[case] on_target: &str,
) {
    let idx = parse_index(src);
    assert_eq!(idx.name().as_deref(), Some(name));
    assert_eq!(idx.fields(), fields);
    assert_eq!(idx.on_target().as_deref(), Some(on_target));
}

#[rstest]
#[expect(clippy::expect_used, reason = "fixture invariant")]
fn index_missing_on_is_error(index_invalid_missing_on: &str) {
    let parsed = crate::parse(index_invalid_missing_on);
    let start = index_invalid_missing_on
        .find("User")
        .expect("fixture invariant");
    let end = start + "User".len();
    assert_parse_error(parsed.errors(), "K_ON", start, end);
    assert!(parsed.root().indexes().is_empty());
}

#[rstest]
fn legacy_index_shorthand_is_error(index_legacy_shorthand: &str) {
    let parsed = crate::parse(index_legacy_shorthand);
    assert_custom_parse_error_contains(
        parsed.errors(),
        "index declarations require a typed field list `(name: Type, ...)` before `on`",
    );
    assert!(parsed.root().indexes().is_empty());
}

#[rstest]
#[case("index Idx_User_ws(username: string) on User[username]")]
#[case(" index  Idx_User_ws ( username: string )  on  User[ username ] ")]
#[case(index_whitespace_variations())]
fn index_declaration_whitespace_variations(#[case] src: &str) {
    let parsed = crate::parse(src);
    assert_no_parse_errors(parsed.errors());
    let indexes = parsed.root().indexes();
    assert_eq!(indexes.len(), 1);
    let printed = pretty_print(parsed.root().syntax());
    assert_eq!(normalize_whitespace(&printed), normalize_whitespace(src));
    let idx = parse_index(src);
    assert_eq!(idx.name().as_deref(), Some("Idx_User_ws"));
    assert_eq!(
        idx.fields(),
        vec![(String::from("username"), String::from("string"))]
    );
    assert_eq!(idx.on_target().as_deref(), Some("User[username]"));
}
