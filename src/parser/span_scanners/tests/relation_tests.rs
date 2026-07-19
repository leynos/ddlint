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
