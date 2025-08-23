//! Index declaration parsing tests.
//!
//! These tests cover single and multi-column indexes and error cases.

use super::common::{normalise_whitespace, parse_index, pretty_print};
use crate::test_util::{assert_parse_error, assert_unclosed_delimiter_error};
use rstest::{fixture, rstest};

#[fixture]
fn index_single_column() -> &'static str {
    "index Idx_User_username on User(username)"
}

#[fixture]
fn index_multi_column() -> &'static str {
    "index Idx_Session_user_time on UserSession(user_id, start_time)"
}

#[fixture]
fn index_invalid_missing_on() -> &'static str {
    "index Idx_Invalid User(username)"
}

#[fixture]
fn index_nested_function() -> &'static str {
    "index Idx_lower_username on User(lower(username))"
}

#[fixture]
fn index_unbalanced_parentheses() -> &'static str {
    "index Idx_Unbalanced on User(lower(username)"
}

#[fixture]
fn index_whitespace_variations() -> &'static str {
    "  index  Idx_User_ws \t on\n  User (\n    username  )  "
}

#[rstest]
#[case(index_single_column(), "Idx_User_username", "User", vec!["username".into()])]
#[case(index_multi_column(), "Idx_Session_user_time", "UserSession", vec!["user_id".into(), "start_time".into()])]
#[case(index_nested_function(), "Idx_lower_username", "User", vec!["lower(username)".into()])]
fn parses_indexes(
    #[case] src: &str,
    #[case] name: &str,
    #[case] relation: &str,
    #[case] columns: Vec<String>,
) {
    let idx = parse_index(src);
    assert_eq!(idx.name(), Some(name.into()));
    assert_eq!(idx.relation(), Some(relation.into()));
    assert_eq!(idx.columns(), columns);
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
fn index_unbalanced_parentheses_is_error(index_unbalanced_parentheses: &str) {
    let parsed = crate::parse(index_unbalanced_parentheses);
    let len = index_unbalanced_parentheses.len();
    assert_unclosed_delimiter_error(parsed.errors(), "T_RPAREN", 0, len);
    assert!(parsed.root().indexes().is_empty());
}

#[rstest]
#[case("index Idx_User_ws on User(username)")]
#[case(" index  Idx_User_ws  on  User( username ) ")]
#[case(index_whitespace_variations())]
fn index_declaration_whitespace_variations(#[case] src: &str) {
    let parsed = crate::parse(src);
    assert!(parsed.errors().is_empty());
    let indexes = parsed.root().indexes();
    assert_eq!(indexes.len(), 1);
    let printed = pretty_print(parsed.root().syntax());
    assert_eq!(normalise_whitespace(&printed), normalise_whitespace(src));
    let idx = parse_index(src);
    assert_eq!(idx.name(), Some("Idx_User_ws".into()));
    assert_eq!(idx.relation(), Some("User".into()));
    assert_eq!(idx.columns(), vec![String::from("username")]);
}
