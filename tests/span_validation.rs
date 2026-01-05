//! Behavioural coverage for span validation in public APIs.

use ddlint::{parse, parser::ParsedSpans};

#[test]
fn parse_builds_with_valid_spans() {
    let parsed = parse("import foo::bar;");
    assert!(parsed.errors().is_empty());
    assert_eq!(parsed.root().imports().len(), 1);
}

#[expect(
    clippy::expect_used,
    reason = "test assertions prefer expect to confirm error paths"
)]
#[test]
fn builder_errs_on_invalid_spans_in_public_api() {
    let err = ParsedSpans::builder()
        .imports(vec![2..4, 1..2])
        .build()
        .expect_err("expected validation error");
    assert!(err.to_string().contains("imports not sorted"));
}

#[expect(
    clippy::expect_used,
    reason = "test assertions prefer expect to confirm error paths"
)]
#[test]
fn builder_try_build_exposes_validation_error() {
    let err = ParsedSpans::builder()
        .imports(vec![2..4, 1..2])
        .try_build()
        .expect_err("expected validation error");
    assert!(err.to_string().contains("imports not sorted"));
    let issues = err.issues();
    let issue = issues.first().expect("expected at least one issue");
    assert_eq!(issue.list(), "imports");
}
