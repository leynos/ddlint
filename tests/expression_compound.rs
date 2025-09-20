//! Integration tests for compound expressions.
//!
//! These tests cover struct literals, tuple literals and closure expressions
//! through the public `parse_expression` API.

use ddlint::parser::ast::{BinaryOp, Expr};
use ddlint::parser::expression::parse_expression;
use ddlint::test_util::{
    assert_parse_error, assert_unclosed_delimiter_error, closure, field, if_expr, lit_num,
    struct_expr, tuple, var,
};
use rstest::rstest;

#[rstest]
#[case("(1, 2)", tuple(vec![lit_num("1"), lit_num("2")]))]
#[case("(1, 2, 3)", tuple(vec![lit_num("1"), lit_num("2"), lit_num("3")]))]
#[case("(1,)", tuple(vec![lit_num("1")]))]
#[case("()", tuple(vec![]))]
#[case("(1)", Expr::Group(Box::new(lit_num("1"))))]
#[case("Point { x: 1 }", struct_expr("Point", vec![field("x", lit_num("1"))]))]
#[case("Point {}", struct_expr("Point", vec![]))]
#[case(
    "Point { x: 1, }",
    struct_expr("Point", vec![field("x", lit_num("1"))]),
)]
#[case(
    "Point { x: 1, y: 2, }",
    struct_expr(
        "Point",
        vec![field("x", lit_num("1")), field("y", lit_num("2"))],
    ),
)]
#[case("|x, y| x + y", closure(vec!["x", "y"], Expr::Binary { op: BinaryOp::Add, lhs: Box::new(var("x")), rhs: Box::new(var("y")) }))]
#[case("|| 1", closure(std::iter::empty::<&str>(), lit_num("1")))]
#[case("|x,| x", closure(vec!["x"], var("x")))]
fn parses_compound_expressions(#[case] src: &str, #[case] expected: Expr) {
    let expr = parse_expression(src).unwrap_or_else(|e| panic!("source {src:?} errors: {e:?}"));
    assert_eq!(expr, expected);
}

#[rstest]
#[case("Point { x: 1", "expected", 12, 12, true)]
#[case("(1, 2", "expected", 5, 5, true)]
#[case("|x|", "invalid expression", 3, 3, false)]
#[case("|x x", "expected pipe", 3, 4, false)]
#[case("||", "invalid expression", 2, 2, false)]
fn compound_expression_errors(
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

#[rstest]
#[case(
    "if x { y } else { z }",
    if_expr(
        var("x"),
        Expr::Group(Box::new(var("y"))),
        Some(Expr::Group(Box::new(var("z")))),
    )
)]
#[case(
    "if (Point { x: 1 }) { y } else { z }",
    if_expr(
        Expr::Group(Box::new(struct_expr(
            "Point",
            vec![field("x", lit_num("1"))],
        ))),
        Expr::Group(Box::new(var("y"))),
        Some(Expr::Group(Box::new(var("z")))),
    )
)]
#[case("if flag value", if_expr(var("flag"), var("value"), None))]
fn parses_if_expressions(#[case] src: &str, #[case] expected: Expr) {
    let expr = parse_expression(src).unwrap_or_else(|e| panic!("source {src:?} errors: {e:?}"));
    assert_eq!(expr, expected);
}

#[rstest]
#[case(
    "if cond else value",
    "expected expression for 'then' branch of 'if'",
    8,
    12
)]
#[case(
    "if cond value else",
    "expected expression for 'else' branch of 'if'",
    14,
    18
)]
#[case("if", "expected condition expression after 'if'", 2, 2)]
fn if_expression_errors(
    #[case] src: &str,
    #[case] msg: &str,
    #[case] start: usize,
    #[case] end: usize,
) {
    let Err(errors) = parse_expression(src) else {
        panic!("expected error");
    };
    assert_parse_error(&errors, msg, start, end);
}
