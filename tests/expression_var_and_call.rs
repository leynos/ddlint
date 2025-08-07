//! Integration tests for variable references and function calls.
//!
//! These tests cover the public `parse_expression` API to ensure identifiers
//! become variables and that calls parse their argument lists correctly.

use ddlint::parser::ast::Expr;
use ddlint::parser::expression::parse_expression;
use ddlint::test_util::{call, lit_num, var};
use rstest::rstest;

#[rstest]
#[case("x", var("x"))]
#[case("foo()", call("foo", vec![]))]
#[case("foo(x, 1)", call("foo", vec![var("x"), lit_num("1")]))]
fn parses_vars_and_calls(#[case] src: &str, #[case] expected: Expr) {
    let expr = parse_expression(src).unwrap_or_else(|e| panic!("errors: {e:?}"));
    assert_eq!(expr, expected);
}
