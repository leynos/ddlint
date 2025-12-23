//! Integration tests for postfix expressions.
//!
//! These tests cover method calls, field accesses, tuple indexing, and bit
//! slice expressions through the public `parse_expression` API.

use ddlint::parser::ast::Expr;
use ddlint::parser::expression::parse_expression;
use ddlint::test_util::{
    assert_parse_error, assert_unclosed_delimiter_error, bit_slice, call, field_access, lit_num,
    method_call, tuple_index, var,
};
use rstest::rstest;

#[rstest]
#[case::method_call("foo.bar(x)", method_call(var("foo"), "bar", vec![var("x")]))]
#[case::field_access("foo.bar", field_access(var("foo"), "bar"))]
#[case::bit_slice("e[1,0]", bit_slice(var("e"), lit_num("1"), lit_num("0")))]
#[case::tuple_index("t.0", tuple_index(var("t"), "0"))]
#[case::multi_arg_call("foo.bar(1, 2)", method_call(var("foo"), "bar", vec![lit_num("1"), lit_num("2")]))]
#[case::chained_postfix("foo.bar().baz[1,0]", bit_slice(field_access(method_call(var("foo"), "bar", vec![]), "baz"), lit_num("1"), lit_num("0")))]
#[case::diff_atom("S'(x)", Expr::AtomDiff { expr: Box::new(call("S", vec![var("x")])) })]
#[case::delay_atom("A() -<10>", Expr::AtomDelay { delay: 10, expr: Box::new(call("A", vec![])) })]
#[case::diff_and_delay("S'(x) -<1>", Expr::AtomDelay { delay: 1, expr: Box::new(Expr::AtomDiff { expr: Box::new(call("S", vec![var("x")])) }) })]
fn parses_postfix_expressions(#[case] src: &str, #[case] expected: Expr) {
    let expr = parse_expression(src).unwrap_or_else(|e| panic!("source {src:?} errors: {e:?}"));
    assert_eq!(expr, expected);
}

#[rstest]
#[case::unclosed_call("foo.bar(", "right paren", 8, 8, true)]
#[case::trailing_dot("foo.", "expected identifier or tuple index after '.'", 4, 4, false)]
#[case::bit_slice_missing_comma("e[1]", "expected comma", 3, 4, false)]
#[case::bit_slice_unclosed("e[1,0", "expected right bracket", 5, 5, true)]
#[case::diff_marker_without_args("S'", "diff marker must precede atom arguments", 1, 2, false)]
#[case::delay_out_of_range("A() -<4294967296>", "delay must fit u32", 6, 16, false)]
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
        assert_unclosed_delimiter_error(&errors, msg, start, end);
    } else {
        assert_parse_error(&errors, msg, start, end);
    }
}
