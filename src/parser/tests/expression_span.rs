//! Tests for expression span utilities.

use crate::parser::expression_span::{
    ExpressionError, rule_body_literal_spans, validate_expression,
};
use crate::test_util::tokenize;

#[test]
fn validate_expression_reports_out_of_bounds() {
    let src = "a + b";
    let span = 0..src.len() + 5;
    match validate_expression(src, span.clone()) {
        Err(ExpressionError::OutOfBounds { span: sp }) => assert_eq!(sp, span),
        _ => panic!("expected out-of-bounds error"),
    }
}

#[expect(
    clippy::expect_used,
    reason = "tests unwrap spans to aid debugging on failure"
)]
#[test]
fn splits_rule_body_literals() {
    let src = "R(x) :- A(x), if cond(x) { B(x) } else { C(x) }.";
    let tokens = tokenize(src);
    let spans = rule_body_literal_spans(&tokens, 0, src.len());
    assert_eq!(spans.len(), 2);
    let first = spans
        .first()
        .cloned()
        .and_then(|sp| src.get(sp))
        .expect("first span missing");
    assert_eq!(first, "A(x)");
    let second = spans
        .get(1)
        .cloned()
        .and_then(|sp| src.get(sp))
        .expect("second span missing");
    assert_eq!(second.trim(), "if cond(x) { B(x) } else { C(x) }");
}

#[test]
fn fact_rules_have_no_body_spans() {
    let src = "SystemAlert(\"ok\").";
    let tokens = tokenize(src);
    let spans = rule_body_literal_spans(&tokens, 0, src.len());
    assert!(spans.is_empty());
}
