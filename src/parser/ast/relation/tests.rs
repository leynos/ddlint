//! Tests for relation AST accessors.

use super::*;
use crate::{parse, test_util::span_text};
use rstest::rstest;

#[test]
fn relation_name() {
    let parsed = parse("input relation R(x: u32)");
    #[expect(clippy::expect_used, reason = "Using expect for clearer test failures")]
    let rel = parsed
        .root()
        .relations()
        .first()
        .cloned()
        .expect("relation missing");
    assert_eq!(rel.name().as_deref(), Some("R"));
    assert!(rel.is_input());
    assert_eq!(rel.role(), RelationRole::Input);
    assert!(rel.role_keyword_present());
    assert_eq!(rel.kind(), RelationKind::Relation);
    assert!(rel.kind_keyword_present());
}

#[test]
fn relation_name_span_points_to_declaration_identifier() {
    let source = "input relation Source(Source: u32)";
    let parsed = parse(source);
    crate::test_util::assert_no_parse_errors(parsed.errors());
    #[expect(clippy::expect_used, reason = "Using expect for clearer test failures")]
    let rel = parsed
        .root()
        .relations()
        .first()
        .cloned()
        .expect("relation missing");

    let span = rel
        .name_span()
        .unwrap_or_else(|| panic!("missing relation name_span in `{source}`"));

    assert_eq!(span_text(source, &span), "Source");
    assert_eq!(span.start, "input relation ".len());
}

#[test]
fn relation_preamble_defaults_for_bare_relation() {
    let parsed = parse("R(x: u32)");
    crate::test_util::assert_no_parse_errors(parsed.errors());
    #[expect(clippy::expect_used, reason = "Using expect for clearer test failures")]
    let rel = parsed
        .root()
        .relations()
        .first()
        .cloned()
        .expect("relation missing");

    assert_eq!(rel.name().as_deref(), Some("R"));
    assert_eq!(rel.role(), RelationRole::Internal);
    assert!(!rel.role_keyword_present());
    assert_eq!(rel.kind(), RelationKind::Relation);
    assert!(!rel.kind_keyword_present());
    assert!(!rel.is_ref());
}

#[rstest]
#[case::paren_before_name("(x: u32)")]
#[case::bracket_before_name("[u32]")]
fn malformed_preamble_delimiters_do_not_leak_body_names(#[case] src: &str) {
    // A body delimiter before any relation name is not a relation
    // candidate, so no relation declaration surfaces a body identifier as
    // its name. This guards the delimiter cut-off in `inspect`.
    let parsed = parse(src);
    for rel in parsed.root().relations() {
        assert_eq!(
            rel.name(),
            None,
            "body identifier leaked as relation name for source: {src}",
        );
    }
}

#[test]
fn relation_kind_and_ref_are_exposed() {
    let parsed = parse("output stream & Events(event: Event)");
    crate::test_util::assert_no_parse_errors(parsed.errors());
    #[expect(clippy::expect_used, reason = "Using expect for clearer test failures")]
    let rel = parsed
        .root()
        .relations()
        .first()
        .cloned()
        .expect("relation missing");

    assert_eq!(rel.role(), RelationRole::Output);
    assert_eq!(rel.kind(), RelationKind::Stream);
    assert!(rel.kind_keyword_present());
    assert!(rel.is_ref());
}

#[test]
fn bracket_body_exposes_element_type() {
    let parsed = parse("input multiset Items[Map<string, Vec<u32>>]");
    crate::test_util::assert_no_parse_errors(parsed.errors());
    #[expect(clippy::expect_used, reason = "Using expect for clearer test failures")]
    let rel = parsed
        .root()
        .relations()
        .first()
        .cloned()
        .expect("relation missing");

    assert_eq!(rel.columns(), Ok(Vec::<(String, String)>::new()));
    assert_eq!(
        rel.element_type(),
        Ok(Some("Map<string, Vec<u32>>".to_string()))
    );
    assert_eq!(
        rel.body(),
        Ok(RelationBody::ElementType("Map<string, Vec<u32>>".into()))
    );
}

#[test]
fn record_relation_body_accessors_are_ok() {
    let parsed = parse("input relation R(x: u32, y: string)");
    crate::test_util::assert_no_parse_errors(parsed.errors());
    #[expect(clippy::expect_used, reason = "Using expect for clearer test failures")]
    let rel = parsed
        .root()
        .relations()
        .first()
        .cloned()
        .expect("relation missing");

    assert_eq!(
        rel.body(),
        Ok(RelationBody::Fields(vec![
            ("x".into(), "u32".into()),
            ("y".into(), "string".into()),
        ]))
    );
    assert_eq!(rel.element_type(), Ok(None));
    assert_eq!(
        rel.columns(),
        Ok(vec![
            ("x".into(), "u32".into()),
            ("y".into(), "string".into())
        ])
    );
    assert_eq!(rel.primary_key(), Ok(None));
}

#[rstest]
#[case::single("input relation R(x: u32) primary key (x)", vec!["x".to_string()])]
#[case::compound(
    "input relation R(x: u32, y: string) primary key (x, y)",
    vec!["x".to_string(), "y".to_string()]
)]
fn input_record_primary_key_is_ok(#[case] src: &str, #[case] expected: Vec<String>) {
    let parsed = parse(src);
    crate::test_util::assert_no_parse_errors(parsed.errors());
    #[expect(clippy::expect_used, reason = "Using expect for clearer test failures")]
    let rel = parsed
        .root()
        .relations()
        .first()
        .cloned()
        .expect("relation missing");

    assert_eq!(rel.primary_key(), Ok(Some(expected)));
}
