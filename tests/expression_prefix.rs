//! Integration tests for prefix forms and diagnostics.
//!
//! These tests exercise brace grouping, tuple disambiguation, closures,
//! struct literals, and literal parsing through the public `parse_expression`
//! API.

use ddlint::parser::ast::Expr;
use ddlint::parser::expression::parse_expression;
use ddlint::test_util::{
    assert_delimiter_error, assert_parse_error, closure, field, lit_bool, lit_num, lit_str,
    struct_expr, tuple, var,
};
use rstest::rstest;

#[rstest]
#[case("{1}", Expr::Group(Box::new(lit_num("1"))))]
#[case("(1)", Expr::Group(Box::new(lit_num("1"))))]
#[case("(1,)", tuple(vec![lit_num("1")]))]
#[case("|x| x", closure(vec!["x"], var("x")))]
#[case("|a,b| a", closure(vec!["a", "b"], var("a")))]
#[case(
    "Point { x: 1, y: 2 }",
    struct_expr("Point", vec![field("x", lit_num("1")), field("y", lit_num("2"))]),
)]
#[case("{ x }", Expr::Group(Box::new(var("x"))))]
#[case("\"hi\"", lit_str("hi"))]
#[case("false", lit_bool(false))]
fn parses_prefix_forms(#[case] src: &str, #[case] expected: Expr) {
    let expr = parse_expression(src).unwrap_or_else(|e| panic!("source {src:?} errors: {e:?}"));
    assert_eq!(expr, expected);
}

#[rstest]
#[case("{}", "expected expression", 1, 2, false)]
#[case("|x x", "expected T_PIPE", 3, 4, false)]
fn prefix_form_errors(
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
