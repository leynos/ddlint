//! Tests for the attribute span scanner.

use super::super::collect_attribute_spans;
use crate::test_util::tokenize;
use rstest::rstest;

#[expect(
    clippy::expect_used,
    reason = "tests unwrap spans to provide crisp failure messages"
)]
#[test]
fn collect_attribute_spans_valid_on_typedef() {
    let src = "#[cold]\ntypedef T = u32\n";
    let tokens = tokenize(src);
    let (spans, errors) = collect_attribute_spans(&tokens, src);
    assert_eq!(spans.len(), 1);
    assert!(errors.is_empty(), "unexpected errors: {errors:?}");
    let text = spans
        .first()
        .and_then(|sp| src.get(sp.clone()))
        .expect("first span text missing");
    assert_eq!(text, "#[cold]");
}

#[rstest]
#[case("#[inline]\nfunction f() {}\n")]
#[case("#[hot]\ninput relation R(x: u32)\n")]
#[case("#[cold]\nextern function f()\n")]
fn collect_attribute_spans_valid_on_permitted_item(#[case] src: &str) {
    let tokens = tokenize(src);
    let (spans, errors) = collect_attribute_spans(&tokens, src);
    assert_eq!(spans.len(), 1);
    assert!(errors.is_empty(), "unexpected errors: {errors:?}");
}

#[expect(
    clippy::expect_used,
    reason = "tests unwrap errors to provide crisp failure messages"
)]
#[test]
fn collect_attribute_spans_rejected_on_index() {
    let src = "#[cold]\nindex I on R(x)\n";
    let tokens = tokenize(src);
    let (spans, errors) = collect_attribute_spans(&tokens, src);
    assert_eq!(spans.len(), 1);
    assert_eq!(errors.len(), 1);
    let first_error = errors.first().expect("expected at least one error");
    assert!(
        format!("{first_error:?}").contains("attribute"),
        "unexpected error: {first_error:?}",
    );
}

#[rstest]
#[case("#[cold]\napply T(R) -> (S)\n")]
#[case("#[cold]\nimport foo\n")]
#[case("#[cold]\nextern transformer t(x: A): B\n")]
fn collect_attribute_spans_rejected_on_forbidden_item(#[case] src: &str) {
    let tokens = tokenize(src);
    let (spans, errors) = collect_attribute_spans(&tokens, src);
    assert_eq!(spans.len(), 1);
    assert_eq!(errors.len(), 1);
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
fn collect_attribute_spans_hash_without_bracket() {
    let src = "# typedef T = u32\n";
    let tokens = tokenize(src);
    let (spans, errors) = collect_attribute_spans(&tokens, src);
    assert!(spans.is_empty());
    assert!(errors.is_empty());
}
