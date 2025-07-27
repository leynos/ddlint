//! Tests for the Pratt expression parser.

use crate::parser::ast::{BinaryOp, Expr, Literal, UnaryOp};
use crate::parser::expression::parse_expression;
use rstest::rstest;

fn lit_num(n: &str) -> Expr {
    Expr::Literal(Literal::Number(n.into()))
}

#[rstest]
#[case("1 + 2 * 3", Expr::Binary { op: BinaryOp::Add, lhs: Box::new(lit_num("1")), rhs: Box::new(Expr::Binary { op: BinaryOp::Mul, lhs: Box::new(lit_num("2")), rhs: Box::new(lit_num("3")) }) })]
#[case("8 - 4 - 2", Expr::Binary { op: BinaryOp::Sub, lhs: Box::new(Expr::Binary { op: BinaryOp::Sub, lhs: Box::new(lit_num("8")), rhs: Box::new(lit_num("4")) }), rhs: Box::new(lit_num("2")) })]
#[case("-5 + 2", Expr::Binary { op: BinaryOp::Add, lhs: Box::new(Expr::Unary { op: UnaryOp::Neg, expr: Box::new(lit_num("5")) }), rhs: Box::new(lit_num("2")) })]
#[case("-(5 + 2)", Expr::Unary { op: UnaryOp::Neg, expr: Box::new(Expr::Group(Box::new(Expr::Binary { op: BinaryOp::Add, lhs: Box::new(lit_num("5")), rhs: Box::new(lit_num("2")) }))) })]
fn parses_expressions(#[case] src: &str, #[case] expected: Expr) {
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
