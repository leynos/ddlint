//! Integration tests for parsing literal expressions.
//!
//! These tests exercise the public `parse_expression` API by feeding it
//! standalone literals and verifying the resulting AST nodes.

use ddlint::parser::ast::Expr;
use ddlint::parser::expression::parse_expression;
use ddlint::test_util::{
    assert_parse_error, assert_unclosed_delimiter_error, lit_bool,
    lit_interned_raw_interpolated_str, lit_interned_raw_str, lit_interned_str,
    lit_interpolated_str, lit_num, lit_raw_interpolated_str, lit_raw_str, lit_str,
};
use rstest::rstest;

#[rstest]
#[case("42", lit_num("42"))]
#[case("\"hi\"", lit_str("hi"))]
#[case("\"hi ${user}\"", lit_interpolated_str("hi ${user}"))]
#[case("\"\\${user}\"", lit_str("\\${user}"))]
#[case("[|raw|]", lit_raw_str("raw"))]
#[case("$[|raw ${x}|]", lit_raw_interpolated_str("raw ${x}"))]
#[case("i\"hi\"", lit_interned_str("hi"))]
#[case("i[|raw|]", lit_interned_raw_str("raw"))]
#[case("i$[|raw ${x}|]", lit_interned_raw_interpolated_str("raw ${x}"))]
#[case("true", lit_bool(true))]
#[case("false", lit_bool(false))]
fn parses_literal_expressions(#[case] src: &str, #[case] expected: Expr) {
    let expr = parse_expression(src).unwrap_or_else(|e| panic!("source {src:?} errors: {e:?}"));
    assert_eq!(expr, expected);
}

#[rstest]
#[case("\"no end", "unexpected token", 0, 7, true)]
#[case("12abc", "unexpected token", 2, 5, false)]
fn literal_parsing_errors(
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
