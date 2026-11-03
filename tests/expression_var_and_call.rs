//! Integration tests for variable references and function calls.
//!
//! These tests cover the public `parse_expression` API to ensure identifiers
//! become variables and that calls parse their argument lists correctly.

use ddlint::{
    parser::{ast::Expr, expression::parse_expression},
    test_util::{
        assert_delimiter_error,
        assert_parse_error,
        assert_unclosed_delimiter_error,
        call,
        lit_num,
        qualified_call,
        var,
    },
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

/// One call-parsing error case.
struct CallErrorCase {
    src: &'static str,
    msg: &'static str,
    start: usize,
    end: usize,
    kind: Kind,
}

#[rstest]
#[case(CallErrorCase {
    src: "foo(",
    msg: "right paren",
    start: 4,
    end: 4,
    kind: Kind::Unclosed,
})]
#[case(CallErrorCase {
    src: "foo(]",
    msg: "unexpected token",
    start: 4,
    end: 5,
    kind: Kind::Mismatch,
})]
#[case(CallErrorCase {
    src: "foo(x,)",
    msg: "unexpected trailing comma in argument list",
    start: 5,
    end: 6,
    kind: Kind::Other,
})]
#[case(CallErrorCase {
    src: "foo(1 2)",
    msg: "expected right paren",
    start: 6,
    end: 7,
    kind: Kind::Other,
})]
fn call_parsing_errors(#[case] case: CallErrorCase) {
    let CallErrorCase {
        src,
        msg,
        start,
        end,
        kind,
    } = case;
    let Err(errors) = parse_expression(src) else {
        panic!("expected error");
    };
    match kind {
        Kind::Unclosed => assert_unclosed_delimiter_error(&errors, msg, start, end),
        Kind::Mismatch => assert_delimiter_error(&errors, msg, start, end),
        Kind::Other => assert_parse_error(&errors, msg, start, end),
    }
}
