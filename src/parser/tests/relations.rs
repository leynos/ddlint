//! Relation declaration parsing tests.
//!
//! Validates parsing for input, output, and internal relations.

use super::helpers::{parse_relation, pretty_print};
use crate::parser::ast::AstNode;
use crate::test_util::{
    assert_custom_parse_error_contains, assert_no_parse_errors, assert_parse_error,
};
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
fn internal_relation_no_pk() -> &'static str {
    "relation UserSession(user_id: u32, session_id: string, start_time: u64)"
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
    internal_relation_no_pk(),
    false,
    false,
    "UserSession",
    vec![("user_id", "u32"), ("session_id", "string"), ("start_time", "u64")],
    None,
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
    // Currently the span scanner reports unclosed delimiter errors using the
    // span of the parsed substream (the statement start) rather than the
    // opening delimiter position. This preserves branch behaviour while still
    // asserting the unclosed ')' is detected.
    let len = relation_unbalanced_parentheses.len();
    assert_parse_error(parsed.errors(), "unclosed ')'", len, len);
    assert!(parsed.root().relations().is_empty());
}

#[rstest]
#[case::bare_record("R()")]
#[case::bare_bracket("R[u32]")]
#[case::input_record_pk("input R(id: u32) primary key (id)")]
#[case::output_record("output R(id: u32)")]
#[case::input_kind_record_pk("input relation R(id: u32) primary key (id)")]
#[case::output_kind_record("output relation R(id: u32)")]
#[case::kind_bracket("relation R[u32]")]
#[case::input_stream_record("input stream R(id: u32)")]
#[case::output_stream_bracket("output stream R[u32]")]
#[case::bare_stream_record("stream R(id: u32)")]
#[case::input_multiset_record_pk("input multiset R(id: u32) primary key (id)")]
#[case::bare_multiset_record("multiset R(id: u32)")]
#[case::output_multiset_bracket("output multiset R[u32]")]
#[case::input_ref_record("input & R(id: u32)")]
#[case::output_relation_ref_bracket("output relation & R[u32]")]
#[case::commented_preamble("input /* role */\nstream /* kind */\nR(id: u32)")]
fn parses_relation_form_matrix(#[case] src: &str) {
    let parsed = crate::parse(src);
    assert_no_parse_errors(parsed.errors());
    assert_eq!(parsed.root().relations().len(), 1, "source: {src}");
}

#[rstest]
#[case::kind_before_role("relation input R(id: u32)\noutput R(id: u32)", "D-REL-001", 1)]
#[case::two_roles("input output R(id: u32)\noutput R(id: u32)", "D-REL-002", 1)]
#[case::two_kinds("stream multiset R(id: u32)\noutput R(id: u32)", "D-REL-003", 1)]
#[case::bracket_primary_key("input R[u32] primary key (id)\noutput R(id: u32)", "D-REL-004", 2)]
#[case::empty_bracket("input R[]\noutput R(id: u32)", "D-REL-005", 1)]
#[case::non_input_primary_key(
    "output R(id: u32) primary key (id)\ninput R(id: u32)",
    "D-REL-006",
    2
)]
#[case::malformed_primary_key("input R(id: u32) primary value\noutput R(id: u32)", "D-REL-007", 1)]
#[case::bracket_wrapped_primary_key(
    "input R(id: u32) [primary key (id) id]\noutput R(id: u32)",
    "D-REL-008",
    2
)]
fn rejects_invalid_relation_forms(
    #[case] src: &str,
    #[case] expected_error: &str,
    #[case] expected_relations: usize,
) {
    let parsed = crate::parse(src);
    assert_custom_parse_error_contains(parsed.errors(), expected_error);
    assert_eq!(parsed.root().relations().len(), expected_relations);
}

#[test]
fn bare_rule_and_fact_are_not_relation_declarations() {
    let parsed = crate::parse("R(x) :- S(x).\nFact(1).");
    assert_no_parse_errors(parsed.errors());
    assert!(parsed.root().relations().is_empty());
    assert_eq!(parsed.root().rules().len(), 2);
}
