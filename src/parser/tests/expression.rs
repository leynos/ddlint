//! Tests for the Pratt expression parser.

use crate::parser::ast::{BinaryOp, Expr, UnaryOp};
use crate::parser::expression::parse_expression;
use crate::test_util::{call, lit_bool, lit_num, lit_str, var};
use rstest::rstest;

#[rstest]
#[case("1 + 2 * 3", Expr::Binary { op: BinaryOp::Add, lhs: Box::new(lit_num("1")), rhs: Box::new(Expr::Binary { op: BinaryOp::Mul, lhs: Box::new(lit_num("2")), rhs: Box::new(lit_num("3")) }) })]
#[case("8 - 4 - 2", Expr::Binary { op: BinaryOp::Sub, lhs: Box::new(Expr::Binary { op: BinaryOp::Sub, lhs: Box::new(lit_num("8")), rhs: Box::new(lit_num("4")) }), rhs: Box::new(lit_num("2")) })]
#[case("-5 + 2", Expr::Binary { op: BinaryOp::Add, lhs: Box::new(Expr::Unary { op: UnaryOp::Neg, expr: Box::new(lit_num("5")) }), rhs: Box::new(lit_num("2")) })]
#[case("-(5 + 2)", Expr::Unary { op: UnaryOp::Neg, expr: Box::new(Expr::Group(Box::new(Expr::Binary { op: BinaryOp::Add, lhs: Box::new(lit_num("5")), rhs: Box::new(lit_num("2")) }))) })]
#[case("x", var("x"))]
#[case("foo()", call("foo", vec![]))]
#[case("add(x, 1)", call("add", vec![var("x"), lit_num("1")]))]
fn parses_expressions(#[case] src: &str, #[case] expected: Expr) {
    let expr = parse_expression(src).unwrap_or_else(|errs| panic!("errors: {errs:?}"));
    assert_eq!(expr, expected);
}

#[rstest]
#[case("\"hi\"", lit_str("hi"))]
#[case("true", lit_bool(true))]
#[case("false", lit_bool(false))]
#[case("42", lit_num("42"))]
fn parses_literals(#[case] src: &str, #[case] expected: Expr) {
    let expr = parse_expression(src).unwrap_or_else(|errs| panic!("errors: {errs:?}"));
    assert_eq!(expr, expected);
}

#[rstest]
#[case("1 +", 1)]
#[case("(1 + 2", 1)]
#[case("1 ? 2", 1)]
#[case("", 1)]
fn reports_errors(#[case] src: &str, #[case] min_errs: usize) {
    match parse_expression(src) {
        Ok(_) => panic!("expected parse error"),
        Err(errs) => assert!(errs.len() >= min_errs),
    }
}
