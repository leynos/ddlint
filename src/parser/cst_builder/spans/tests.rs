//! Tests for span validation in CST building helpers.

use super::*;
use rstest::rstest;

#[test]
fn validate_spans_sorted_err_on_overlap() {
    let spans = vec![0..5, 4..8];
    assert!(validate_spans_sorted(&spans).is_err());
}

#[test]
fn validate_spans_sorted_err_on_unsorted() {
    let spans = vec![5..10, 0..2];
    assert!(validate_spans_sorted(&spans).is_err());
}

#[test]
fn validate_spans_sorted_ok_on_empty() {
    let spans: Vec<Span> = Vec::new();
    assert!(validate_spans_sorted(&spans).is_ok());
}

#[test]
#[expect(
    clippy::single_range_in_vec_init,
    reason = "clarity preferred over iterator for a single span literal"
)]
fn validate_spans_sorted_ok_on_single() {
    let spans: Vec<Span> = vec![0..3];
    assert!(validate_spans_sorted(&spans).is_ok());
}

#[test]
fn validate_spans_sorted_ok_on_sorted() {
    let spans = vec![0..2, 3..5, 5..8];
    assert!(validate_spans_sorted(&spans).is_ok());
}

#[expect(
    clippy::expect_used,
    reason = "test assertions prefer expect for validation failures"
)]
#[test]
fn validate_span_lists_sorted_reports_list_name() {
    let imports = vec![2..4, 1..2];
    let err = validate_span_lists_sorted(&[("imports", &imports)])
        .expect_err("expected validation error");
    let issue = err.issues().first().expect("expected at least one issue");
    assert_eq!(issue.list(), "imports");
}

#[expect(
    clippy::expect_used,
    reason = "test assertions prefer expect for valid spans"
)]
#[test]
fn builder_succeeds_on_sorted_spans() {
    let spans = vec![0..2, 2..4];
    let parsed = ParsedSpans::builder()
        .imports(spans.clone())
        .build()
        .expect("expected valid spans");
    assert_eq!(parsed.imports(), spans.as_slice());
}

#[rstest]
#[case::unsorted(vec![1..2, 0..1])]
#[case::overlap(vec![0..3, 2..4])]
fn builder_errs_on_invalid_spans(#[case] spans: Vec<Span>) {
    let Err(err) = ParsedSpans::builder().imports(spans).build() else {
        panic!("expected validation error");
    };
    assert!(err.to_string().contains("imports not sorted"));
}

#[expect(
    clippy::expect_used,
    reason = "test assertions prefer expect for invalid spans"
)]
#[test]
fn builder_reports_all_errors() {
    let imports = vec![1..2, 0..1];
    let typedefs = vec![4..5, 3..4];
    let err = ParsedSpans::builder()
        .imports(imports)
        .typedefs(typedefs)
        .build()
        .expect_err("expected validation error");
    let text = err.to_string();
    assert!(text.contains("imports not sorted"));
    assert!(text.contains("typedefs not sorted"));
}
