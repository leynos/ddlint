//! Behavioural coverage for span validation in public APIs.

use ddlint::{parse, parser::ParsedSpans, test_util::assert_panic_with_message};

#[test]
fn parse_builds_with_valid_spans() {
    let parsed = parse("import foo::bar;");
    assert!(parsed.errors().is_empty());
    assert_eq!(parsed.root().imports().len(), 1);
}

#[test]
fn builder_panics_on_invalid_spans_in_public_api() {
    let text = assert_panic_with_message(|| {
        let _ = ParsedSpans::builder().imports(vec![2..4, 1..2]).build();
    });
    assert!(text.contains("imports not sorted"));
}

#[test]
fn builder_try_build_exposes_validation_error() {
    let err = ParsedSpans::builder().imports(vec![2..4, 1..2]).try_build();
    let Err(err) = err else {
        panic!("expected validation error");
    };
    assert!(err.to_string().contains("imports not sorted"));
}
