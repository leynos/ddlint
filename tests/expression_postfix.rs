//! Integration tests for postfix expressions.
//!
//! These tests cover method calls, field accesses, tuple indexing, and bit
//! slice expressions through the public `parse_expression` API.

use ddlint::parser::ast::Expr;
use ddlint::parser::expression::parse_expression;
use ddlint::test_util::{
    assert_delimiter_error, assert_parse_error, bit_slice, field_access, lit_num, method_call,
    tuple_index, var,
};
use rstest::rstest;

#[rstest]
#[case("foo.bar(x)", method_call(var("foo"), "bar", vec![var("x")]))]
#[case("foo.bar", field_access(var("foo"), "bar"))]
#[case("e[1,0]", bit_slice(var("e"), lit_num("1"), lit_num("0")))]
#[case("t.0", tuple_index(var("t"), "0"))]
fn parses_postfix_expressions(#[case] src: &str, #[case] expected: Expr) {
    let expr = parse_expression(src).unwrap_or_else(|e| panic!("source {src:?} errors: {e:?}"));
    assert_eq!(expr, expected);
}

#[rstest]
#[case("foo.bar(", "invalid expression", 8, 8, true)]
#[case("foo.", "expected identifier or tuple index after '.'", 4, 4, false)]
#[case("e[1]", "expected T_COMMA", 3, 4, false)]
#[case("e[1,0", "expected T_RBRACKET", 5, 5, true)]
fn postfix_expression_errors(
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
        assert_delimiter_error(&errors, msg, start, end);
    } else {
        assert_parse_error(&errors, msg, start, end);
    }
}
