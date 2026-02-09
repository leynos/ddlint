//! Integration tests for variable references and function calls.
//!
//! These tests cover the public `parse_expression` API to ensure identifiers
//! become variables and that calls parse their argument lists correctly.

use ddlint::parser::ast::Expr;
use ddlint::parser::expression::parse_expression;
use ddlint::test_util::{
    assert_delimiter_error, assert_parse_error, assert_unclosed_delimiter_error, call, lit_num,
    qualified_call, var,
};
use rstest::rstest;

#[rstest]
#[case("x", var("x"))]
#[case("foo()", call("foo", vec![]))]
#[case("foo(x, 1)", call("foo", vec![var("x"), lit_num("1")]))]
#[case("pkg::Foo(x)", call("pkg::Foo", vec![var("x")]))]
#[case(
    "pkg::foo(x, 1)",
    qualified_call("pkg::foo", vec![var("x"), lit_num("1")]),
)]
fn parses_vars_and_calls(#[case] src: &str, #[case] expected: Expr) {
    let expr = parse_expression(src).unwrap_or_else(|e| panic!("source {src:?} errors: {e:?}"));
    assert_eq!(expr, expected);
}

#[derive(Debug, Copy, Clone)]
enum Kind {
    Unclosed,
    Mismatch,
    Other,
}

#[rstest]
#[case("foo(", "right paren", 4, 4, Kind::Unclosed)]
#[case("foo(]", "unexpected token", 4, 5, Kind::Mismatch)]
#[case(
    "foo(x,)",
    "unexpected trailing comma in argument list",
    6,
    7,
    Kind::Other
)]
#[case("foo(1 2)", "expected right paren", 6, 7, Kind::Other)]
fn call_parsing_errors(
    #[case] src: &str,
    #[case] msg: &str,
    #[case] start: usize,
    #[case] end: usize,
    #[case] kind: Kind,
) {
    let Err(errors) = parse_expression(src) else {
        panic!("expected error");
    };
    match kind {
        Kind::Unclosed => assert_unclosed_delimiter_error(&errors, msg, start, end),
        Kind::Mismatch => assert_delimiter_error(&errors, msg, start, end),
        Kind::Other => assert_parse_error(&errors, msg, start, end),
    }
}
