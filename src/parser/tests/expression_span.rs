//! Tests for expression span utilities.

use rstest::rstest;

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

/// Tests that `T_IMPLIES` and `T_DOT` inside nested delimiters are not treated as
/// rule body boundaries. The depth-tracking logic should correctly gate these tokens.
#[rstest]
#[case::dot_inside_parens(
    "R(x) :- A(x.field), B(x).",
    &["A(x.field)", "B(x)"]
)]
#[case::implies_inside_parens(
    "R(x) :- A(x => y), B(x).",
    &["A(x => y)", "B(x)"]
)]
#[case::dot_inside_braces(
    "R(x) :- A({ key: x.val }), B(x).",
    &["A({ key: x.val })", "B(x)"]
)]
#[case::implies_inside_braces(
    "R(x) :- A({ a => b }), B(x).",
    &["A({ a => b })", "B(x)"]
)]
#[case::dot_inside_brackets(
    "R(x) :- A([x.field]), B(x).",
    &["A([x.field])", "B(x)"]
)]
#[case::implies_inside_brackets(
    "R(x) :- A([a => b]), B(x).",
    &["A([a => b])", "B(x)"]
)]
#[case::deeply_nested_parens_in_braces_in_brackets(
    "R(x) :- A([{ (x.y.z) }]), B(x).",
    &["A([{ (x.y.z) }])", "B(x)"]
)]
#[case::deeply_nested_brackets_in_parens_in_braces(
    "R(x) :- A({ ([x.a.b]) }), B(x).",
    &["A({ ([x.a.b]) })", "B(x)"]
)]
#[case::multiple_nested_implies(
    "R(x) :- A((a => b) => c), B(x).",
    &["A((a => b) => c)", "B(x)"]
)]
#[case::transition_in_out_of_nesting(
    "R(x) :- A(x), B({ y.z }), C(x), D([a => b]), E(x).",
    &["A(x)", "B({ y.z })", "C(x)", "D([a => b])", "E(x)"]
)]
fn depth_tracking_ignores_delimiters_inside_nested_structures(
    #[case] src: &str,
    #[case] expected: &[&str],
) {
    let tokens = tokenize(src);
    let spans = rule_body_literal_spans(&tokens, 0, src.len());

    assert_eq!(
        spans.len(),
        expected.len(),
        "expected {} literals, got {} for: {src}",
        expected.len(),
        spans.len()
    );

    for (i, (span, &exp)) in spans.iter().zip(expected.iter()).enumerate() {
        let actual = src
            .get(span.clone())
            .map_or("<out of bounds>", str::trim);
        assert_eq!(
            actual, exp,
            "literal {i} mismatch for: {src}\nexpected: {exp}\nactual: {actual}"
        );
    }
}
