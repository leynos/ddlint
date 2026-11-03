//! Integration tests for postfix expressions.
//!
//! These tests cover method calls, field accesses, tuple indexing, and bit
//! slice expressions through the public `parse_expression` API.

use ddlint::{
    parser::{ast::Expr, expression::parse_expression},
    test_util::{
        assert_parse_error,
        assert_unclosed_delimiter_error,
        bit_slice,
        call,
        field_access,
        lit_num,
        method_call,
        tuple_index,
        var,
    },
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

/// One postfix-expression error case.
struct PostfixErrorCase {
    src: &'static str,
    msg: &'static str,
    start: usize,
    end: usize,
    is_unclosed: bool,
}

#[rstest]
#[case::unclosed_call(PostfixErrorCase {
    src: "foo.bar(",
    msg: "right paren",
    start: 8,
    end: 8,
    is_unclosed: true,
})]
#[case::trailing_dot(PostfixErrorCase {
    src: "foo.",
    msg: "expected identifier or tuple index after '.'",
    start: 4,
    end: 4,
    is_unclosed: false,
})]
#[case::bit_slice_missing_comma(PostfixErrorCase {
    src: "e[1]",
    msg: "expected comma",
    start: 3,
    end: 4,
    is_unclosed: false,
})]
#[case::bit_slice_unclosed(PostfixErrorCase {
    src: "e[1,0",
    msg: "expected right bracket",
    start: 5,
    end: 5,
    is_unclosed: true,
})]
#[case::diff_marker_without_args(PostfixErrorCase {
    src: "S'",
    msg: "diff marker must be followed by atom arguments",
    start: 1,
    end: 2,
    is_unclosed: false,
})]
#[case::delay_out_of_range(PostfixErrorCase {
    src: "A() -<4294967296>",
    msg: "delay must fit u32",
    start: 6,
    end: 16,
    is_unclosed: false,
})]
fn postfix_expression_errors(#[case] case: PostfixErrorCase) {
    let PostfixErrorCase {
        src,
        msg,
        start,
        end,
        is_unclosed,
    } = case;
    let Err(errors) = parse_expression(src) else {
        panic!("expected error");
    };
    if is_unclosed {
        assert_unclosed_delimiter_error(&errors, msg, start, end);
    } else {
        assert_parse_error(&errors, msg, start, end);
    }
}
