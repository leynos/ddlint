//! Tests for `Expr::to_sexpr` formatting.
//!
//! These unit tests construct AST nodes directly and assert that
//! `Expr::to_sexpr` yields the expected S-expression strings.

use ddlint::parser::ast::{BinaryOp, Expr, Literal, UnaryOp};
use rstest::rstest;

fn num(n: &str) -> Expr {
    Expr::Literal(Literal::Number(n.into()))
}

fn str_lit(s: &str) -> Expr {
    Expr::Literal(Literal::String(s.into()))
}

fn bool_lit(b: bool) -> Expr {
    Expr::Literal(Literal::Bool(b))
}

fn var(name: &str) -> Expr {
    Expr::Variable(name.into())
}

#[rstest]
#[case(num("1"), "1")]
#[case(bool_lit(true), "true")]
fn literals_render(#[case] expr: Expr, #[case] expected: &str) {
    assert_eq!(expr.to_sexpr(), expected);
}

#[test]
fn variable_renders() {
    assert_eq!(var("x").to_sexpr(), "x");
}

#[rstest]
#[case(UnaryOp::Neg, num("1"), "(- 1)")]
#[case(UnaryOp::Not, var("x"), "(not x)")]
fn unary_renders(#[case] op: UnaryOp, #[case] expr: Expr, #[case] expected: &str) {
    let unary = Expr::Unary {
        op,
        expr: Box::new(expr),
    };
    assert_eq!(unary.to_sexpr(), expected);
}

#[rstest]
#[case(BinaryOp::Add, num("1"), num("2"), "(+ 1 2)")]
#[case(BinaryOp::And, var("x"), var("y"), "(and x y)")]
fn binary_renders(
    #[case] op: BinaryOp,
    #[case] lhs: Expr,
    #[case] rhs: Expr,
    #[case] expected: &str,
) {
    let bin = Expr::Binary {
        op,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
    };
    assert_eq!(bin.to_sexpr(), expected);
}

#[rstest]
#[case(vec![], "(call f)")]
#[case(vec![num("1"), var("x")], "(call f 1 x)")]
fn call_renders(#[case] args: Vec<Expr>, #[case] expected: &str) {
    let call = Expr::Call {
        callee: Box::new(var("f")),
        args,
    };
    assert_eq!(call.to_sexpr(), expected);
}

#[rstest]
#[case(vec![], "(method a b)")]
#[case(vec![num("1"), var("x")], "(method a b 1 x)")]
fn method_call_renders(#[case] args: Vec<Expr>, #[case] expected: &str) {
    let method = Expr::MethodCall {
        recv: Box::new(var("a")),
        name: "b".into(),
        args,
    };
    assert_eq!(method.to_sexpr(), expected);
}

#[test]
fn field_access_renders() {
    let expr = Expr::FieldAccess {
        expr: Box::new(var("a")),
        field: "b".into(),
    };
    assert_eq!(expr.to_sexpr(), "(field a b)");
}

#[test]
fn tuple_index_renders() {
    let expr = Expr::TupleIndex {
        expr: Box::new(var("t")),
        index: "0".into(),
    };
    assert_eq!(expr.to_sexpr(), "(tuple-index t 0)");
}

#[test]
fn bitslice_renders() {
    let expr = Expr::BitSlice {
        expr: Box::new(var("x")),
        hi: Box::new(num("7")),
        lo: Box::new(num("0")),
    };
    assert_eq!(expr.to_sexpr(), "(bitslice x 7 0)");
}

#[test]
fn struct_renders_field_order() {
    let expr = Expr::Struct {
        name: "Pair".into(),
        fields: vec![("a".into(), num("1")), ("b".into(), num("2"))],
    };
    assert_eq!(expr.to_sexpr(), "(struct Pair (a 1) (b 2))");
}

#[rstest]
#[case(vec![num("1")], "(tuple 1)")]
#[case(vec![num("1"), num("2")], "(tuple 1 2)")]
fn tuple_renders(#[case] items: Vec<Expr>, #[case] expected: &str) {
    assert_eq!(Expr::Tuple(items).to_sexpr(), expected);
}

#[test]
fn closure_renders() {
    let expr = Expr::Closure {
        params: vec!["x".into(), "y".into()],
        body: Box::new(Expr::Binary {
            op: BinaryOp::Add,
            lhs: Box::new(var("x")),
            rhs: Box::new(var("y")),
        }),
    };
    assert_eq!(expr.to_sexpr(), "(closure (x y) (+ x y))");
}

#[test]
fn nested_closures_render() {
    let expr = Expr::Closure {
        params: vec!["x".into()],
        body: Box::new(Expr::Closure {
            params: vec!["y".into()],
            body: Box::new(var("y")),
        }),
    };
    assert_eq!(expr.to_sexpr(), "(closure (x) (closure (y) y))");
}

#[test]
fn group_renders() {
    let expr = Expr::Group(Box::new(var("x")));
    assert_eq!(expr.to_sexpr(), "(group x)");
}

#[test]
fn nested_call_and_method_render() {
    let expr = Expr::Call {
        callee: Box::new(Expr::MethodCall {
            recv: Box::new(var("a")),
            name: "b".into(),
            args: vec![num("1")],
        }),
        args: vec![Expr::FieldAccess {
            expr: Box::new(var("c")),
            field: "d".into(),
        }],
    };
    assert_eq!(expr.to_sexpr(), "(call (method a b 1) (field c d))");
}

#[test]
fn string_literal_renders() {
    let expr = str_lit("a \"b\" (c)");
    let expected = format!("{:?}", "a \"b\" (c)");
    assert_eq!(expr.to_sexpr(), expected);
}

#[test]
fn for_loop_renders() {
    let expr = Expr::ForLoop {
        pattern: "item".into(),
        iterable: Box::new(var("items")),
        guard: Some(Box::new(var("ready"))),
        body: Box::new(Expr::Call {
            callee: Box::new(var("emit")),
            args: vec![var("item")],
        }),
    };
    assert_eq!(expr.to_sexpr(), "(for item items ready (call emit item))");
}
