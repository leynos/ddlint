//! Tests for the `parse_expression` entry point.
//!
//! These cases ensure token drainage, precedence, and error collection work
//! as expected when calling the public API.

use ddlint::parser::ast::{BinaryOp, Expr};
use ddlint::parser::expression::parse_expression;
use ddlint::test_util::{assert_parse_error, lit_num};
use rstest::rstest;

#[rstest]
fn parses_single_literal() {
    let expr = parse_expression("42").unwrap_or_else(|e| panic!("source errors: {e:?}"));
    assert_eq!(expr, lit_num("42"));
}

#[rstest]
fn parses_binary_precedence_chain() {
    let expr = parse_expression("1 + 2 * 3").unwrap_or_else(|e| panic!("source errors: {e:?}"));
    assert_eq!(
        expr,
        Expr::Binary {
            op: BinaryOp::Add,
            lhs: Box::new(lit_num("1")),
            rhs: Box::new(Expr::Binary {
                op: BinaryOp::Mul,
                lhs: Box::new(lit_num("2")),
                rhs: Box::new(lit_num("3")),
            }),
        },
    );
}

#[rstest]
#[case("", "invalid expression", 0, 0)]
#[case("1 2", "unexpected token: number", 2, 3)]
fn parse_expression_errors(
    #[case] src: &str,
    #[case] msg: &str,
    #[case] start: usize,
    #[case] end: usize,
) {
    let Err(errors) = parse_expression(src) else {
        panic!("expected error");
    };
    assert_parse_error(&errors, msg, start, end);
}

#[rstest]
fn accumulates_trailing_token_errors() {
    let Err(errors) = parse_expression("1 2 3") else {
        panic!("expected error");
    };
    assert_eq!(errors.len(), 2);
}
