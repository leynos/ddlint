//! Focused tests for control-flow expression parsing.
//!
//! These ensure dedicated coverage for `break`, `continue`, and `return`
//! expressions, including contexts where delimiters terminate the return
//! value.

use crate::parser::ast::Expr;
use crate::parser::expression::parse_expression;
use crate::test_util::{assert_parse_error, break_expr, continue_expr, return_expr, tuple, var};
use rstest::rstest;

#[rstest]
#[case("break", break_expr())]
#[case("{ break }", Expr::Group(Box::new(break_expr())))]
#[case("(break)", Expr::Group(Box::new(break_expr())))]
fn parses_break_expressions(#[case] src: &str, #[case] expected: Expr) {
    let expr = parse_expression(src).unwrap_or_else(|e| panic!("source {src:?} errors: {e:?}"));
    assert_eq!(expr, expected);
}

#[rstest]
#[case("continue", continue_expr())]
#[case("{ continue }", Expr::Group(Box::new(continue_expr())))]
#[case("(continue)", Expr::Group(Box::new(continue_expr())))]
fn parses_continue_expressions(#[case] src: &str, #[case] expected: Expr) {
    let expr = parse_expression(src).unwrap_or_else(|e| panic!("source {src:?} errors: {e:?}"));
    assert_eq!(expr, expected);
}

#[rstest]
#[case("return", return_expr(None))]
#[case("return value", return_expr(Some(var("value"))))]
#[case("return (x, y)", return_expr(Some(tuple(vec![var("x"), var("y")]))))]
#[case("{ return }", Expr::Group(Box::new(return_expr(None))))]
#[case("(return)", Expr::Group(Box::new(return_expr(None))))]
fn parses_return_expressions(#[case] src: &str, #[case] expected: Expr) {
    let expr = parse_expression(src).unwrap_or_else(|e| panic!("source {src:?} errors: {e:?}"));
    assert_eq!(expr, expected);
}

#[test]
fn return_reports_missing_value_error() {
    let Err(errors) = parse_expression("return {") else {
        panic!("expected parse failure");
    };
    assert_parse_error(&errors, "expected expression after 'return'", 8, 8);
}
