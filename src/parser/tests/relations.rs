//! Relation declaration parsing tests.
//!
//! Validates parsing for input, output, and internal relations.

use super::common::{parse_relation, pretty_print};
use crate::parser::ast::AstNode;
use crate::test_util::assert_unclosed_delimiter_error;
use rstest::{fixture, rstest};

#[fixture]
fn input_relation_pk() -> &'static str {
    "input relation User(user_id: u32, username: string) primary key (user_id)"
}

#[fixture]
fn output_relation_no_pk() -> &'static str {
    "output relation Alert(message: string, timestamp: u64)"
}

#[fixture]
fn internal_relation_compound_pk() -> &'static str {
    "relation UserSession(user_id: u32, session_id: string, start_time: u64) primary key (user_id, session_id)"
}

#[fixture]
fn multiline_relation() -> &'static str {
    "input relation Log(\n    id: u32,\n    message: string\n) primary key (id)\n"
}

#[fixture]
fn relation_unbalanced_parentheses() -> &'static str {
    "relation Foo(x: u32"
}

#[rstest]
#[case::input(input_relation_pk(), true, false, "User", vec![("user_id", "u32"), ("username", "string")], Some(vec!["user_id"]))]
#[case::output(output_relation_no_pk(), false, true, "Alert", vec![("message", "string"), ("timestamp", "u64")], None)]
#[case::internal(
    internal_relation_compound_pk(),
    false,
    false,
    "UserSession",
    vec![("user_id", "u32"), ("session_id", "string"), ("start_time", "u64")],
    Some(vec!["user_id", "session_id"]),
)]
fn parses_relations(
    #[case] src: &str,
    #[case] is_input: bool,
    #[case] is_output: bool,
    #[case] name: &str,
    #[case] columns: Vec<(&str, &str)>,
    #[case] primary_key: Option<Vec<&str>>,
) {
    let rel = parse_relation(src);
    assert_eq!(rel.is_input(), is_input);
    assert_eq!(rel.is_output(), is_output);
    assert_eq!(rel.name().as_deref(), Some(name));
    let cols: Vec<(String, String)> = columns
        .into_iter()
        .map(|(n, t)| (n.into(), t.into()))
        .collect();
    assert_eq!(rel.columns(), cols);
    let pk: Option<Vec<String>> = primary_key.map(|v| v.into_iter().map(String::from).collect());
    assert_eq!(rel.primary_key(), pk);
}

#[rstest]
fn multiline_relation_parsed(multiline_relation: &str) {
    let rel = parse_relation(multiline_relation);
    assert!(rel.is_input());
    assert_eq!(rel.name().as_deref(), Some("Log"));
    assert_eq!(
        rel.columns(),
        vec![
            ("id".into(), "u32".into()),
            ("message".into(), "string".into())
        ]
    );
    assert_eq!(rel.primary_key(), Some(vec!["id".into()]));
    assert_eq!(pretty_print(rel.syntax()), multiline_relation);
}

#[rstest]
fn relation_unbalanced_parentheses_is_error(relation_unbalanced_parentheses: &str) {
    let parsed = crate::parse(relation_unbalanced_parentheses);
    let len = relation_unbalanced_parentheses.len();
    let start = relation_unbalanced_parentheses
        .find('(')
        .unwrap_or_else(|| panic!("fixture invariant"));
    assert_unclosed_delimiter_error(parsed.errors(), "T_RPAREN", start, len);
    assert!(parsed.root().relations().is_empty());
}
