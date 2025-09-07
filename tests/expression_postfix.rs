//! Integration tests for postfix expressions.
//!
//! These tests cover method calls, field accesses, tuple indexing, and bit
//! slice expressions through the public `parse_expression` API.

use ddlint::parser::ast::Expr;
use ddlint::parser::expression::parse_expression;
use ddlint::test_util::{bit_slice, field_access, lit_num, method_call, tuple_index, var};
use rstest::rstest;

#[rstest]
#[case("foo.bar(x)", method_call(var("foo"), "bar", vec![var("x")]))]
#[case("foo.bar", field_access(var("foo"), "bar"))]
#[case("e[1,0]", bit_slice(var("e"), lit_num("1"), lit_num("0")))]
#[case("t.0", tuple_index(var("t"), "0"))]
#[case("foo.bar(1, 2)", method_call(var("foo"), "bar", vec![lit_num("1"), lit_num("2")]))]
#[case("foo.bar().baz[1,0]", bit_slice(field_access(method_call(var("foo"), "bar", vec![]), "baz"), lit_num("1"), lit_num("0")))]
fn parses_postfix_expressions(#[case] src: &str, #[case] expected: Expr) {
    let expr = parse_expression(src).unwrap_or_else(|e| panic!("source {src:?} errors: {e:?}"));
    assert_eq!(expr, expected);
}
