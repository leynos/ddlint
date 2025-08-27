//! Integration tests for compound expressions.
//!
//! These tests cover struct literals, tuple literals and closure expressions
//! through the public `parse_expression` API.

use ddlint::parser::ast::{BinaryOp, Expr};
use ddlint::parser::expression::parse_expression;
mod test_util;
use rstest::rstest;
use test_util::{
    assert_parse_error, assert_unclosed_delimiter_error, closure, field, lit_num, struct_expr,
    tuple, var,
};

#[rstest]
#[case("(1, 2)", tuple(vec![lit_num("1"), lit_num("2")]))]
#[case("(1,)", tuple(vec![lit_num("1")]))]
#[case("Point { x: 1 }", struct_expr("Point", vec![field("x", lit_num("1"))]))]
#[case("|x, y| x + y", closure(vec!["x", "y"], Expr::Binary { op: BinaryOp::Add, lhs: Box::new(var("x")), rhs: Box::new(var("y")) }))]
fn parses_compound_expressions(#[case] src: &str, #[case] expected: Expr) {
    let expr = parse_expression(src).unwrap_or_else(|e| panic!("source {src:?} errors: {e:?}"));
    assert_eq!(expr, expected);
}

#[rstest]
#[case("Point { x: 1", "expected T_RBRACE", 12, 12, true)]
#[case("(1, 2", "expected T_RPAREN", 5, 5, true)]
#[case("|x|", "invalid expression", 3, 3, false)]
fn compound_expression_errors(
    #[case] src: &str,
    #[case] msg: &str,
    #[case] start: usize,
    #[case] end: usize,
    #[case] unclosed: bool,
) {
    let Err(errors) = parse_expression(src) else {
        panic!("expected error");
    };
    if unclosed {
        assert_unclosed_delimiter_error(&errors, msg, start, end);
    } else {
        assert_parse_error(&errors, msg, start, end);
    }
}
