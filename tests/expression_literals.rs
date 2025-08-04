//! Integration tests for parsing literal expressions.
//!
//! These tests exercise the public `parse_expression` API by feeding it
//! standalone literals and verifying the resulting AST nodes.

use ddlint::parser::ast::Expr;
use ddlint::parser::expression::parse_expression;
use ddlint::test_util::{lit_bool, lit_num, lit_str};
use rstest::rstest;

#[rstest]
#[case("42", lit_num("42"))]
#[case("\"hi\"", lit_str("hi"))]
#[case("true", lit_bool(true))]
#[case("false", lit_bool(false))]
fn parses_literal_expressions(#[case] src: &str, #[case] expected: Expr) {
    let expr = parse_expression(src).unwrap_or_else(|e| panic!("errors: {e:?}"));
    assert_eq!(expr, expected);
}
