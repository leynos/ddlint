//! Tests for top-level `for` rejection diagnostic.
//!
//! Validates that the scanner emits a clear diagnostic when `for` appears at a
//! top-level line-start position, while rule-body `for` remains accepted.

use super::super::helpers::{parse_err, parse_ok};
use crate::test_util::{ErrorPattern, assert_parse_error};
use rstest::rstest;

const FOR_KEYWORD_LEN: usize = "for".len();

#[rstest]
fn top_level_for_emits_diagnostic() {
    let src = "for (x in Items(x)) Process(x).";
    let parsed = parse_err(src);
    let errors = parsed.errors();
    #[expect(clippy::expect_used, reason = "test expects at least one parse error")]
    let first = errors.first().expect("expected diagnostic");
    assert_parse_error(
        std::slice::from_ref(first),
        ErrorPattern::from("top-level `for` is not supported"),
        0,
        FOR_KEYWORD_LEN,
    );
}

#[rstest]
fn top_level_for_does_not_produce_rule() {
    let src = "for (x in Items(x)) Process(x).";
    let parsed = parse_err(src);
    assert!(
        parsed.root().rules().is_empty(),
        "top-level `for` must not produce a rule span"
    );
}

#[rstest]
fn top_level_for_after_dot_separator() {
    let src = "A(x) :- B(x). for (y in C(y)) D(y).";
    let parsed = crate::parse(src);
    let errors = parsed.errors();
    let has_top_level_for_error = errors.iter().any(|e| {
        let rendered = format!("{e:?}");
        rendered.contains("top-level") && rendered.contains("for")
    });
    assert!(
        has_top_level_for_error,
        "expected top-level `for` diagnostic after dot separator, got: {errors:?}"
    );
}

#[rstest]
fn rule_body_for_still_accepted() {
    let src = "R(x) :- for (item in Items(item)) Process(item).";
    let _parsed = parse_ok(src);
}
