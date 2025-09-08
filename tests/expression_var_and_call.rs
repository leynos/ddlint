//! Integration tests for variable references and function calls.
//!
//! These tests cover the public `parse_expression` API to ensure identifiers
//! become variables and that calls parse their argument lists correctly.

use ddlint::parser::ast::Expr;
use ddlint::parser::expression::parse_expression;
use ddlint::test_util::{
    assert_delimiter_error, assert_parse_error, assert_unclosed_delimiter_error, call, lit_num, var,
};
use rstest::rstest;

#[rstest]
#[case("x", var("x"))]
#[case("foo()", call("foo", vec![]))]
#[case("foo(x, 1)", call("foo", vec![var("x"), lit_num("1")]))]
fn parses_vars_and_calls(#[case] src: &str, #[case] expected: Expr) {
    let expr = parse_expression(src).unwrap_or_else(|e| panic!("source {src:?} errors: {e:?}"));
    assert_eq!(expr, expected);
}

#[rstest]
#[case("foo(", "invalid expression", 4, 4, "unclosed")]
#[case("foo(]", "unexpected token", 4, 5, "other")]
#[case("foo(x,)", "unexpected trailing comma in argument list", 6, 7, "other")]
#[case("foo(1 2)", "expected right paren", 6, 7, "other")]
fn call_parsing_errors(
    #[case] src: &str,
    #[case] msg: &str,
    #[case] start: usize,
    #[case] end: usize,
    #[case] kind: &str,
) {
    let Err(errors) = parse_expression(src) else {
        panic!("expected error");
    };
    match kind {
        "unclosed" => assert_unclosed_delimiter_error(&errors, msg, start, end),
        "mismatch" => assert_delimiter_error(&errors, msg, start, end),
        _ => assert_parse_error(&errors, msg, start, end),
    }
}
