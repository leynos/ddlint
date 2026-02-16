//! Tests for the attribute span scanner.

use super::super::collect_attribute_spans;
use crate::test_util::tokenize;
use rstest::rstest;

#[rstest]
#[case("#[inline]\nfunction f() {}\n", None)]
#[case("#[hot]\ninput relation R(x: u32)\n", None)]
#[case("#[cold]\nextern function f()\n", None)]
#[case("#[cold]\ntypedef T = u32\n", Some("#[cold]"))]
fn collect_attribute_spans_valid_on_permitted_item(
    #[case] src: &str,
    #[case] expected_text: Option<&str>,
) {
    let tokens = tokenize(src);
    let (spans, errors) = collect_attribute_spans(&tokens, src);
    assert_eq!(spans.len(), 1);
    assert!(errors.is_empty(), "unexpected errors: {errors:?}");
    if let Some(expected) = expected_text {
        let text = spans.first().and_then(|sp| src.get(sp.clone()));
        assert_eq!(text, Some(expected), "span text mismatch");
    }
}

#[rstest]
#[case("#[cold]\napply T(R) -> (S)\n")]
#[case("#[cold]\nimport foo\n")]
#[case("#[cold]\nextern transformer t(x: A): B\n")]
#[case("#[cold]\nextern typedef T = u32\n")]
#[case("#[cold]\nindex I on R(x)\n")]
fn collect_attribute_spans_rejected_on_forbidden_item(#[case] src: &str) {
    let tokens = tokenize(src);
    let (spans, errors) = collect_attribute_spans(&tokens, src);
    assert!(
        spans.is_empty(),
        "invalid attributes should not appear in spans"
    );
    assert_eq!(errors.len(), 1);
    let has_attribute_error = errors
        .first()
        .is_some_and(|e| format!("{e:?}").contains("attribute"));
    assert!(has_attribute_error, "unexpected errors: {errors:?}");
}

#[test]
fn collect_attribute_spans_stacked() {
    let src = "#[a]\n#[b]\ntypedef T = u32\n";
    let tokens = tokenize(src);
    let (spans, errors) = collect_attribute_spans(&tokens, src);
    assert_eq!(spans.len(), 2);
    assert!(errors.is_empty(), "unexpected errors: {errors:?}");
}

#[test]
fn collect_attribute_spans_unclosed_bracket() {
    let src = "#[cold";
    let tokens = tokenize(src);
    let (spans, errors) = collect_attribute_spans(&tokens, src);
    assert!(
        spans.is_empty(),
        "unclosed attribute should produce no spans"
    );
    assert_eq!(errors.len(), 1);
    let has_unclosed_error = errors
        .first()
        .is_some_and(|e| format!("{e:?}").contains("unclosed"));
    assert!(has_unclosed_error, "unexpected errors: {errors:?}");
}

#[test]
fn collect_attribute_spans_hash_without_bracket() {
    let src = "# typedef T = u32\n";
    let tokens = tokenize(src);
    let (spans, errors) = collect_attribute_spans(&tokens, src);
    assert!(spans.is_empty());
    assert!(errors.is_empty());
}
