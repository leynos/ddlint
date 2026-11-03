//! Integration tests for compound expressions.
//!
//! These tests cover struct literals, tuple literals and closure expressions
//! through the public `parse_expression` API.

use ddlint::{
    parser::{
        ast::{BinaryOp, Expr},
        expression::parse_expression,
    },
    test_util::{
        assert_parse_error,
        assert_unclosed_delimiter_error,
        closure,
        field,
        if_expr,
        lit_bool,
        lit_num,
        struct_expr,
        tuple,
        var,
    },
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

/// One compound-expression error case.
struct CompoundErrorCase {
    src: &'static str,
    msg: &'static str,
    start: usize,
    end: usize,
    is_unclosed: bool,
}

#[rstest]
#[case(CompoundErrorCase {
    src: "Point { x: 1",
    msg: "expected",
    start: 12,
    end: 12,
    is_unclosed: true,
})]
#[case(CompoundErrorCase {
    src: "(1, 2",
    msg: "expected",
    start: 5,
    end: 5,
    is_unclosed: true,
})]
#[case(CompoundErrorCase {
    src: "|x|",
    msg: "invalid expression",
    start: 3,
    end: 3,
    is_unclosed: false,
})]
#[case(CompoundErrorCase {
    src: "|x x",
    msg: "expected pipe",
    start: 3,
    end: 4,
    is_unclosed: false,
})]
#[case(CompoundErrorCase {
    src: "||",
    msg: "invalid expression",
    start: 2,
    end: 2,
    is_unclosed: false,
})]
fn compound_expression_errors(#[case] case: CompoundErrorCase) {
    let CompoundErrorCase {
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
#[case(
    "if flag { Point { x: 1 } } else { z }",
    if_expr(
        var("flag"),
        Expr::Group(Box::new(struct_expr(
            "Point",
            vec![field("x", lit_num("1"))],
        ))),
        Some(Expr::Group(Box::new(var("z")))),
    )
)]
#[case(
    "if a and b { x } else { y }",
    if_expr(
        Expr::Binary {
            op: BinaryOp::And,
            lhs: Box::new(var("a")),
            rhs: Box::new(var("b")),
        },
        Expr::Group(Box::new(var("x"))),
        Some(Expr::Group(Box::new(var("y")))),
    )
)]
#[case("if flag value", if_expr(var("flag"), var("value"), None))]
fn parses_if_expressions(#[case] src: &str, #[case] expected: Expr) {
    let expr = parse_expression(src).unwrap_or_else(|e| panic!("source {src:?} errors: {e:?}"));
    assert_eq!(expr, expected);
}

#[rstest]
#[case(
    "if cond { Outer { inner: Inner { a: 1, b: 2 }, flag: true } } else { fallback }",
    if_expr(
        var("cond"),
        Expr::Group(Box::new(struct_expr(
            "Outer",
            vec![
                field(
                    "inner",
                    struct_expr(
                        "Inner",
                        vec![
                            field("a", lit_num("1")),
                            field("b", lit_num("2")),
                        ],
                    ),
                ),
                field("flag", lit_bool(true)),
            ],
        ))),
        Some(Expr::Group(Box::new(var("fallback")))),
    )
)]
#[case(
    "if ok { S { f: T { x: 1, y: U { z: 2 } }, g: 3 } } else { alt }",
    if_expr(
        var("ok"),
        Expr::Group(Box::new(struct_expr(
            "S",
            vec![
                field(
                    "f",
                    struct_expr(
                        "T",
                        vec![
                            field("x", lit_num("1")),
                            field(
                                "y",
                                struct_expr(
                                    "U",
                                    vec![field("z", lit_num("2"))],
                                ),
                            ),
                        ],
                    ),
                ),
                field("g", lit_num("3")),
            ],
        ))),
        Some(Expr::Group(Box::new(var("alt")))),
    )
)]
fn parses_if_then_with_struct_literal(#[case] src: &str, #[case] expected: Expr) {
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
