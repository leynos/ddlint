//! Behavioural tests for relation declaration grammar alignment.
//!
//! Verifies that the public `ddlint::parse` entrypoint accepts the canonical
//! role/kind/body/ref forms and rejects invalid primary-key combinations.

use ddlint::ast::{Relation, RelationBody, RelationKind, RelationRole};
use ddlint::parse;
use ddlint::test_util::{assert_custom_parse_error_contains, assert_no_parse_errors};
use rstest::{fixture, rstest};

#[fixture]
fn parsed_relation(#[default("")] source: &str) -> Relation {
    let parsed = parse(source);
    assert_no_parse_errors(parsed.errors());

    let mut relations = parsed.root().relations();
    assert_eq!(relations.len(), 1, "expected one parsed relation");
    relations.remove(0)
}

#[rstest]
fn canonical_record_relation_preserves_role_kind_and_primary_key(
    #[with("input stream Orders(id: OrderId) primary key (id)")] parsed_relation: Relation,
) {
    let relation = parsed_relation;
    assert_eq!(relation.name().as_deref(), Some("Orders"));
    assert_eq!(relation.role(), RelationRole::Input);
    assert!(relation.role_keyword_present());
    assert_eq!(relation.kind(), RelationKind::Stream);
    assert!(relation.kind_keyword_present());
    assert_eq!(
        relation.body(),
        Ok(RelationBody::Fields(vec![(
            "id".to_string(),
            "OrderId".to_string()
        )]))
    );
    assert_eq!(relation.primary_key(), Ok(Some(vec!["id".to_string()])));
}

#[rstest]
fn canonical_bracket_relation_preserves_role_kind_and_ref(
    #[with("#[hot]\noutput relation & Events[Event]")] parsed_relation: Relation,
) {
    let relation = parsed_relation;
    assert_eq!(relation.name().as_deref(), Some("Events"));
    assert_eq!(relation.role(), RelationRole::Output);
    assert_eq!(relation.kind(), RelationKind::Relation);
    assert!(relation.kind_keyword_present());
    assert!(relation.is_ref());
    assert_eq!(
        relation.body(),
        Ok(RelationBody::ElementType("Event".to_string()))
    );
}

#[rstest]
fn multiline_relation_preamble_parses(
    #[with(concat!(
        "input\n",
        "multiset\n",
        "Metrics(\n",
        "id: u32,\n",
        ")"
    ))]
    parsed_relation: Relation,
) {
    let relation = parsed_relation;
    assert_eq!(relation.name().as_deref(), Some("Metrics"));
    assert_eq!(relation.role(), RelationRole::Input);
    assert_eq!(relation.kind(), RelationKind::Multiset);
}

#[test]
fn bracket_relation_rejects_primary_key() {
    let parsed = parse("input R[Row] primary key (id)");
    assert_custom_parse_error_contains(
        parsed.errors(),
        "D-REL-004: bracket-form relations cannot declare a primary key clause",
    );
    assert_eq!(parsed.root().relations().len(), 1);
}

#[test]
fn bracket_wrapped_primary_key_is_rejected() {
    let parsed = parse("input R(id: u32) [primary key (id) id]");
    assert_custom_parse_error_contains(
        parsed.errors(),
        "D-REL-008: bracket-wrapped primary key clauses are not supported",
    );
    assert_eq!(parsed.root().relations().len(), 1);
}
