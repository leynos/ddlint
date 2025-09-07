//! Tests for the Pratt expression parser.

use crate::parser::ast::{BinaryOp, Expr, UnaryOp};
use crate::parser::expression::parse_expression;
use crate::test_util::{
    bit_slice, call, call_expr, closure, field, field_access, lit_bool, lit_num, lit_str,
    method_call, struct_expr, tuple, tuple_index, var,
};
use rstest::rstest;

#[rstest]
#[case("1 + 2 * 3", Expr::Binary { op: BinaryOp::Add, lhs: Box::new(lit_num("1")), rhs: Box::new(Expr::Binary { op: BinaryOp::Mul, lhs: Box::new(lit_num("2")), rhs: Box::new(lit_num("3")) }) })]
#[case("8 - 4 - 2", Expr::Binary { op: BinaryOp::Sub, lhs: Box::new(Expr::Binary { op: BinaryOp::Sub, lhs: Box::new(lit_num("8")), rhs: Box::new(lit_num("4")) }), rhs: Box::new(lit_num("2")) })]
#[case("-5 + 2", Expr::Binary { op: BinaryOp::Add, lhs: Box::new(Expr::Unary { op: UnaryOp::Neg, expr: Box::new(lit_num("5")) }), rhs: Box::new(lit_num("2")) })]
#[case("-(5 + 2)", Expr::Unary { op: UnaryOp::Neg, expr: Box::new(Expr::Group(Box::new(Expr::Binary { op: BinaryOp::Add, lhs: Box::new(lit_num("5")), rhs: Box::new(lit_num("2")) }))) })]
#[case("x", var("x"))]
#[case("foo()", call("foo", vec![]))]
#[case("add(x, 1)", call("add", vec![var("x"), lit_num("1")]))]
#[case("(1, 2)", tuple(vec![lit_num("1"), lit_num("2")]))]
#[case("(1, 2, 3)", tuple(vec![lit_num("1"), lit_num("2"), lit_num("3")]))]
#[case("(1,)", tuple(vec![lit_num("1")]))]
#[case("()", tuple(vec![]))]
#[case("(1)", Expr::Group(Box::new(lit_num("1"))))]
#[case("Point { x: 1, y: 2 }", struct_expr("Point", vec![field("x", lit_num("1")), field("y", lit_num("2"))]))]
#[case("Point {}", struct_expr("Point", vec![]))]
#[case(
    "Point { x: 1, y: 2, }",
    struct_expr("Point", vec![field("x", lit_num("1")), field("y", lit_num("2"))]),
)]
#[case("|x, y| x + y", closure(vec!["x", "y"], Expr::Binary { op: BinaryOp::Add, lhs: Box::new(var("x")), rhs: Box::new(var("y")) }))]
#[case("|| 1", closure(std::iter::empty::<&str>(), lit_num("1")))]
#[case("|x,| x", closure(vec!["x"], var("x")))]
#[case("Point { pair: (1, 2) }", struct_expr("Point", vec![field("pair", tuple(vec![lit_num("1"), lit_num("2")]))]))]
#[case(
    "(|| 1, |x| x)",
    tuple(vec![
        closure(Vec::<&str>::new(), lit_num("1")),
        closure(vec!["x"], var("x")),
    ]),
)]
#[case("|x| Point { x: x }", closure(vec!["x"], struct_expr("Point", vec![field("x", var("x"))])))]
#[case("(f)(x)", call_expr(Expr::Group(Box::new(var("f"))), vec![var("x")]))]
#[case("foo.bar(x)", method_call(var("foo"), "bar", vec![var("x")]))]
#[case("foo.bar", field_access(var("foo"), "bar"))]
#[case("e[1,0]", bit_slice(var("e"), lit_num("1"), lit_num("0")))]
#[case("t.0", tuple_index(var("t"), "0"))]
#[case("x: T", Expr::Binary { op: BinaryOp::Ascribe, lhs: Box::new(var("x")), rhs: Box::new(var("T")) })]
#[case("x as T", Expr::Binary { op: BinaryOp::Cast, lhs: Box::new(var("x")), rhs: Box::new(var("T")) })]
#[case("a = b = c", Expr::Binary { op: BinaryOp::Assign, lhs: Box::new(var("a")), rhs: Box::new(Expr::Binary { op: BinaryOp::Assign, lhs: Box::new(var("b")), rhs: Box::new(var("c")) }) })]
#[case("a = b; c", Expr::Binary { op: BinaryOp::Seq, lhs: Box::new(Expr::Binary { op: BinaryOp::Assign, lhs: Box::new(var("a")), rhs: Box::new(var("b")) }), rhs: Box::new(var("c")) })]
#[case("a and b => c or d", Expr::Binary { op: BinaryOp::Imply, lhs: Box::new(Expr::Binary { op: BinaryOp::And, lhs: Box::new(var("a")), rhs: Box::new(var("b")) }), rhs: Box::new(Expr::Binary { op: BinaryOp::Or, lhs: Box::new(var("c")), rhs: Box::new(var("d")) }) })]
#[case("a or b and c", Expr::Binary { op: BinaryOp::Or, lhs: Box::new(var("a")), rhs: Box::new(Expr::Binary { op: BinaryOp::And, lhs: Box::new(var("b")), rhs: Box::new(var("c")) }) })]
#[case("a and b = c", Expr::Binary { op: BinaryOp::Assign, lhs: Box::new(Expr::Binary { op: BinaryOp::And, lhs: Box::new(var("a")), rhs: Box::new(var("b")) }), rhs: Box::new(var("c")) })]
#[case("a or b = c", Expr::Binary { op: BinaryOp::Assign, lhs: Box::new(Expr::Binary { op: BinaryOp::Or, lhs: Box::new(var("a")), rhs: Box::new(var("b")) }), rhs: Box::new(var("c")) })]
#[case("a + b: T", Expr::Binary { op: BinaryOp::Ascribe, lhs: Box::new(Expr::Binary { op: BinaryOp::Add, lhs: Box::new(var("a")), rhs: Box::new(var("b")) }), rhs: Box::new(var("T")) })]
#[case("x: T = y", Expr::Binary { op: BinaryOp::Assign, lhs: Box::new(Expr::Binary { op: BinaryOp::Ascribe, lhs: Box::new(var("x")), rhs: Box::new(var("T")) }), rhs: Box::new(var("y")) })]
#[case("x as T = y", Expr::Binary { op: BinaryOp::Assign, lhs: Box::new(Expr::Binary { op: BinaryOp::Cast, lhs: Box::new(var("x")), rhs: Box::new(var("T")) }), rhs: Box::new(var("y")) })]
#[case("a => b; c", Expr::Binary { op: BinaryOp::Seq, lhs: Box::new(Expr::Binary { op: BinaryOp::Imply, lhs: Box::new(var("a")), rhs: Box::new(var("b")) }), rhs: Box::new(var("c")) })]
fn parses_expressions(#[case] src: &str, #[case] expected: Expr) {
    let expr = parse_expression(src).unwrap_or_else(|errs| panic!("errors: {errs:?}"));
    assert_eq!(expr, expected);
}

#[rstest]
#[case("\"hi\"", lit_str("hi"))]
#[case("true", lit_bool(true))]
#[case("false", lit_bool(false))]
#[case("42", lit_num("42"))]
fn parses_literals(#[case] src: &str, #[case] expected: Expr) {
    let expr = parse_expression(src).unwrap_or_else(|errs| panic!("errors: {errs:?}"));
    assert_eq!(expr, expected);
}

#[rstest]
#[case("1 +", 1)]
#[case("(1 + 2", 1)]
#[case("1 ? 2", 1)]
#[case("x :", 1)]
#[case("x as", 1)]
#[case("x =", 1)]
#[case("x ;", 1)]
#[case("x =>", 1)]
#[case("", 1)]
fn reports_errors(#[case] src: &str, #[case] min_errs: usize) {
    match parse_expression(src) {
        Ok(_) => panic!("expected parse error"),
        Err(errs) => assert!(errs.len() >= min_errs),
    }
}
