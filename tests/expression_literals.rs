//! Integration tests for parsing literal expressions.
//!
//! These tests exercise the public `parse_expression` API by feeding it
//! standalone literals and verifying the resulting AST nodes.

use ddlint::parser::ast::{Expr, Literal};
use ddlint::parser::expression::parse_expression;
use rstest::rstest;

fn lit_num(n: &str) -> Expr {
    Expr::Literal(Literal::Number(n.into()))
}

fn lit_str(s: &str) -> Expr {
    Expr::Literal(Literal::String(s.into()))
}

fn lit_bool(b: bool) -> Expr {
    Expr::Literal(Literal::Bool(b))
}

#[rstest]
#[case("42", lit_num("42"))]
#[case("\"hi\"", lit_str("hi"))]
#[case("true", lit_bool(true))]
#[case("false", lit_bool(false))]
fn parses_literal_expressions(#[case] src: &str, #[case] expected: Expr) {
    let expr = parse_expression(src).unwrap_or_else(|e| panic!("errors: {e:?}"));
    assert_eq!(expr, expected);
}
