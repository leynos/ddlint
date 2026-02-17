//! Tests for attribute parsing and placement validation.
//!
//! These tests exercise the full `parse()` pipeline to verify that
//! attributes are recognized, wrapped in `N_ATTRIBUTE` CST nodes, and
//! that misplaced attributes emit diagnostics.

use crate::parse;
use crate::test_util::assert_no_parse_errors;
use rstest::rstest;

#[rstest]
#[case("#[cold]\ntypedef T = u32")]
#[case("#[inline]\nfunction f() {}")]
#[case("#[hot]\ninput relation R(x: u32)")]
#[case("#[cold]\noutput relation R(x: u32)")]
#[case("#[cold]\nrelation R(x: u32)")]
#[case("#[hot]\nstream relation R(x: u32)")]
#[case("#[hot]\nmultiset relation R(x: u32)")]
#[case("#[cold]\nextern function f()")]
#[case("#[cold]\nextern type Handle")]
fn attribute_on_permitted_item_no_error(#[case] src: &str) {
    let parsed = parse(src);
    assert_no_parse_errors(parsed.errors());
}

#[rstest]
#[case("#[cold]\nindex I on R(x)", "attribute")]
#[case("#[cold]\napply T(R) -> (S)", "attribute")]
#[case("#[cold]\nimport foo", "attribute")]
#[case("#[cold]\nextern transformer t(x: A): B", "attribute")]
#[case("#[cold]\nextern typedef T = u32", "attribute")]
fn attribute_on_forbidden_item_emits_error(#[case] src: &str, #[case] expected_msg: &str) {
    let parsed = parse(src);
    assert!(!parsed.errors().is_empty(), "expected errors for: {src}");
    let has_match = parsed
        .errors()
        .iter()
        .any(|e| format!("{e:?}").contains(expected_msg));
    assert!(
        has_match,
        "expected error containing '{expected_msg}' in: {:?}",
        parsed.errors()
    );
}

#[test]
fn stacked_attributes_on_typedef_no_error() {
    let src = "#[a]\n#[b]\ntypedef T = u32";
    let parsed = parse(src);
    assert_no_parse_errors(parsed.errors());
}

#[test]
fn attribute_on_rule_emits_error() {
    let src = "#[cold]\nR(x) :- S(x).";
    let parsed = parse(src);
    let has_attr_error = parsed
        .errors()
        .iter()
        .any(|e| format!("{e:?}").contains("attribute"));
    assert!(
        has_attr_error,
        "expected attribute placement error in: {:?}",
        parsed.errors()
    );
}
