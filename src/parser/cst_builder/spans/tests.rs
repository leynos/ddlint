//! Tests for span validation in CST building helpers.

use super::*;
use crate::test_util::assert_panic_with_message;
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
fn validate_spans_sorted_ok_on_single() {
    let spans: Vec<Span> = std::iter::once(0..3).collect();
    assert!(validate_spans_sorted(&spans).is_ok());
}

#[test]
fn validate_spans_sorted_ok_on_sorted() {
    let spans = vec![0..2, 3..5, 5..8];
    assert!(validate_spans_sorted(&spans).is_ok());
}

#[test]
fn validate_span_lists_sorted_reports_list_name() {
    let imports = vec![2..4, 1..2];
    let Err(err) = validate_span_lists_sorted(&[("imports", &imports)]) else {
        panic!("expected validation error");
    };
    let Some(issue) = err.issues().first() else {
        panic!("expected at least one issue");
    };
    assert_eq!(issue.list(), "imports");
}

#[test]
fn builder_succeeds_on_sorted_spans() {
    let spans = vec![0..2, 2..4];
    let parsed = ParsedSpans::builder().imports(spans.clone()).build();
    assert_eq!(parsed.imports(), spans.as_slice());
}

#[rstest]
#[case::unsorted(vec![1..2, 0..1])]
#[case::overlap(vec![0..3, 2..4])]
fn builder_panics_on_invalid_spans(#[case] spans: Vec<Span>) {
    let text = assert_panic_with_message(|| {
        let parsed = ParsedSpans::builder().imports(spans).build();
        drop(parsed);
    });
    assert!(text.contains("imports not sorted"));
}

#[test]
fn builder_reports_all_errors() {
    let imports = vec![1..2, 0..1];
    let typedefs = vec![4..5, 3..4];
    let text = assert_panic_with_message(|| {
        let parsed = ParsedSpans::builder()
            .imports(imports)
            .typedefs(typedefs)
            .build();
        drop(parsed);
    });
    assert!(text.contains("imports not sorted"));
    assert!(text.contains("typedefs not sorted"));
}

#[test]
fn try_build_errs_on_unsorted_spans() {
    let unsorted = vec![1..2, 0..1];
    let result = ParsedSpans::builder().imports(unsorted).try_build();
    let Err(err) = result else {
        panic!("expected validation error");
    };
    assert_eq!(err.issues().len(), 1);
    let Some(issue) = err.issues().first() else {
        panic!("expected at least one issue");
    };
    assert_eq!(issue.list(), "imports");
}

#[test]
fn try_build_ok_on_sorted_spans() {
    let spans = vec![0..1, 2..3];
    let parsed = match ParsedSpans::builder().imports(spans.clone()).try_build() {
        Ok(parsed) => parsed,
        Err(err) => panic!("expected valid spans, got: {err}"),
    };
    assert_eq!(parsed.imports(), spans.as_slice());
}
