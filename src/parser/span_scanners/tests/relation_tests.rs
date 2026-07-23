//! Relation span-scanner regressions.
//!
//! These cases protect top-level candidate discovery and rule/fact
//! disambiguation without expanding the shared scanner-test module.

use super::super::collect_relation_spans;
use crate::test_util::tokenize;

#[test]
fn collect_relation_spans_ignores_nested_relation_candidates() {
    let src = "Rule(\n    input Nested(value: u32)\n) :- Source().";
    let tokens = tokenize(src);

    let (spans, errors) = collect_relation_spans(&tokens, src);

    assert!(spans.is_empty());
    assert!(errors.is_empty());
}

#[test]
fn collect_relation_spans_ignores_bare_bracket_rule_candidates() {
    let src = "R[u32] :- S().";
    let tokens = tokenize(src);

    let (spans, errors) = collect_relation_spans(&tokens, src);

    assert!(spans.is_empty());
    assert!(errors.is_empty());
}

#[test]
fn collect_relation_spans_recovers_consecutive_bare_relations() {
    // The newline between the two bare relations must remain visible to
    // `is_bare_rule_or_fact` so the first relation is not misclassified as a
    // bare rule or fact and silently dropped.
    let src = "R(x: u32)\nS(y: u32)\n";
    let tokens = tokenize(src);

    let (spans, errors) = collect_relation_spans(&tokens, src);

    assert!(errors.is_empty());
    assert_eq!(spans.len(), 2, "both bare relations should be collected");
    let Some(first) = spans.first().and_then(|span| src.get(span.clone())) else {
        panic!("first relation span out of bounds");
    };
    assert!(
        first.trim_start().starts_with("R("),
        "unexpected first relation text: {first:?}"
    );
    let Some(second) = spans.get(1).and_then(|span| src.get(span.clone())) else {
        panic!("second relation span out of bounds");
    };
    assert!(
        second.trim_start().starts_with("S("),
        "unexpected second relation text: {second:?}"
    );
}
