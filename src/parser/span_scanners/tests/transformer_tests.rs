//! Tests for the transformer span scanner.
//!
//! These exercise `collect_transformer_spans` directly to cover boundary
//! conditions in the private helper functions: balanced-paren skipping,
//! identifier validation, error-span computation, and recovery.

use super::super::collect_transformer_spans;
use crate::parser::error_messages::{
    CAPITALIZED_TRANSFORMER_NAME_ERROR, MISSING_OUTPUT_SIGNATURE_ERROR,
};
use crate::test_util::{
    assert_custom_parse_error_contains, assert_no_custom_parse_error_contains,
    assert_no_parse_errors, tokenize,
};
use rstest::rstest;

// ── Valid declarations ──────────────────────────────────────────────

#[test]
fn empty_source_produces_no_spans_or_errors() {
    let tokens = tokenize("");
    let (spans, errors) = collect_transformer_spans(&tokens, "");
    assert!(spans.is_empty());
    assert_no_parse_errors(&errors);
}

#[test]
fn source_without_transformer_produces_no_spans() {
    let src = "input relation R(x: u32)\n";
    let tokens = tokenize(src);
    let (spans, errors) = collect_transformer_spans(&tokens, src);
    assert!(spans.is_empty());
    assert_no_parse_errors(&errors);
}

#[test]
fn underscore_prefixed_name_is_accepted() {
    let src = "extern transformer _private(a: T): Out\n";
    let tokens = tokenize(src);
    let (spans, errors) = collect_transformer_spans(&tokens, src);
    assert_eq!(spans.len(), 1);
    assert_no_parse_errors(&errors);
}

#[test]
fn missing_final_newline_still_produces_span() {
    let src = "extern transformer t(a: T): Out";
    let tokens = tokenize(src);
    let (spans, errors) = collect_transformer_spans(&tokens, src);
    assert_eq!(spans.len(), 1);
    assert_no_parse_errors(&errors);
}

#[rstest]
#[case(
    "extern transformer t(a: (u32, u32)): Out",
    1,
    "nested parens in parameter type"
)]
#[case(
    "extern transformer t(a: Map<K, V>): Out",
    1,
    "angle-bracket generic in parameter type"
)]
fn nested_constructs_inside_params(
    #[case] src: &str,
    #[case] expected_count: usize,
    #[case] description: &str,
) {
    let tokens = tokenize(src);
    let (spans, errors) = collect_transformer_spans(&tokens, src);
    assert_eq!(spans.len(), expected_count, "{description}");
    assert_no_parse_errors(&errors);
}

#[test]
fn multiple_transformers_in_one_source() {
    let src = concat!(
        "extern transformer a(x: T): Out1\n",
        "extern transformer b(y: U): Out2\n",
    );
    let tokens = tokenize(src);
    let (spans, errors) = collect_transformer_spans(&tokens, src);
    assert_eq!(spans.len(), 2);
    assert_no_parse_errors(&errors);
}

// ── Invalid names ───────────────────────────────────────────────────

#[test]
fn capitalized_name_emits_diagnostic_and_no_span() {
    let src = "extern transformer Foo(a: T): Out\n";
    let tokens = tokenize(src);
    let (spans, errors) = collect_transformer_spans(&tokens, src);
    assert!(spans.is_empty());
    assert_custom_parse_error_contains(&errors, CAPITALIZED_TRANSFORMER_NAME_ERROR);
}

// ── Missing output signature ────────────────────────────────────────

#[rstest]
#[case("extern transformer t(a: T)\n", "missing colon and outputs")]
#[case(
    "extern transformer t(a: T):\n",
    "colon present but no output identifier"
)]
fn missing_output_signature_variants(#[case] src: &str, #[case] description: &str) {
    let tokens = tokenize(src);
    let (spans, errors) = collect_transformer_spans(&tokens, src);
    assert!(spans.is_empty(), "{description}: expected no spans");
    assert_custom_parse_error_contains(&errors, MISSING_OUTPUT_SIGNATURE_ERROR);
}

// ── Unbalanced parentheses ──────────────────────────────────────────

#[test]
fn unbalanced_open_paren_produces_error_not_panic() {
    let src = "extern transformer t(a: T\n";
    let tokens = tokenize(src);
    let (spans, errors) = collect_transformer_spans(&tokens, src);
    assert!(spans.is_empty());
    assert!(!errors.is_empty(), "expected error for unclosed paren");
}

// ── Non-extern rejection ────────────────────────────────────────────

#[test]
fn non_extern_transformer_emits_must_be_extern_error() {
    let src = "transformer local(a: T): Out\n";
    let tokens = tokenize(src);
    let (spans, errors) = collect_transformer_spans(&tokens, src);
    assert!(spans.is_empty());
    assert_custom_parse_error_contains(&errors, "must be extern");
}

// ── Recovery isolation ──────────────────────────────────────────────

#[test]
fn malformed_declaration_followed_by_valid_one_recovers() {
    let src = concat!(
        "extern transformer bad(a: T):\n",
        "extern transformer good(b: U): Out\n",
    );
    let tokens = tokenize(src);
    let (spans, errors) = collect_transformer_spans(&tokens, src);
    assert_eq!(spans.len(), 1, "second transformer should still parse");
    assert!(
        !errors.is_empty(),
        "first transformer should produce errors"
    );
    assert_no_custom_parse_error_contains(&errors, "must be extern");
}
