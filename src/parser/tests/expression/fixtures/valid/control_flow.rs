//! Control-flow fixtures for expression parser tests.
//!
//! These grouped cases cover `if`, `match`, and `for` expressions, including
//! nested forms that build larger ASTs for the shared valid-expression suite.

use crate::parser::ast::{BinaryOp, Expr};
use crate::parser::tests::expression::fixtures::ExpressionCase;
use crate::test_util::{
    call, field, field_access, for_loop, if_expr, lit_bool, lit_num, match_arm, match_expr,
    struct_expr, var,
};

/// Returns control-flow expression fixtures grouped by kind.
///
/// The returned list is constructed by appending `match_and_for_expression_cases`
/// after `if_expression_cases`, so consumers can rely on the ordering of groups
/// (`if` cases first, then `match`/`for` cases).
pub(super) fn control_flow_expression_cases() -> Vec<ExpressionCase> {
    let mut cases = if_expression_cases();
    cases.extend(match_and_for_expression_cases());
    cases
}

fn if_expression_cases() -> Vec<ExpressionCase> {
    let mut cases = basic_if_expression_cases();
    cases.extend(struct_if_expression_cases());
    cases
}

fn basic_if_expression_cases() -> Vec<ExpressionCase> {
    vec![
        ExpressionCase {
            src: "if x { y } else { z }",
            expected: if_expr(
                var("x"),
                Expr::Group(Box::new(var("y"))),
                Some(Expr::Group(Box::new(var("z")))),
            ),
        },
        ExpressionCase {
            src: "if (Point { x: 1 }) { y } else { z }",
            expected: if_expr(
                Expr::Group(Box::new(struct_expr(
                    "Point",
                    vec![field("x", lit_num("1"))],
                ))),
                Expr::Group(Box::new(var("y"))),
                Some(Expr::Group(Box::new(var("z")))),
            ),
        },
        ExpressionCase {
            src: "if flag { Point { x: 1 } } else { z }",
            expected: if_expr(
                var("flag"),
                Expr::Group(Box::new(struct_expr(
                    "Point",
                    vec![field("x", lit_num("1"))],
                ))),
                Some(Expr::Group(Box::new(var("z")))),
            ),
        },
        ExpressionCase {
            src: "if a and b { x } else { y }",
            expected: if_expr(
                Expr::Binary {
                    op: BinaryOp::And,
                    lhs: Box::new(var("a")),
                    rhs: Box::new(var("b")),
                },
                Expr::Group(Box::new(var("x"))),
                Some(Expr::Group(Box::new(var("y")))),
            ),
        },
        ExpressionCase {
            src: "if flag value",
            expected: if_expr(var("flag"), var("value"), None),
        },
        ExpressionCase {
            src: "if cond { left } else if other { mid } else { right }",
            expected: if_expr(
                var("cond"),
                Expr::Group(Box::new(var("left"))),
                Some(if_expr(
                    var("other"),
                    Expr::Group(Box::new(var("mid"))),
                    Some(Expr::Group(Box::new(var("right")))),
                )),
            ),
        },
    ]
}

fn struct_if_expression_cases() -> Vec<ExpressionCase> {
    vec![
        ExpressionCase {
            src: "if cond { Outer { inner: Inner { a: 1, b: 2 }, flag: true } } else { fallback }",
            expected: if_expr(
                var("cond"),
                Expr::Group(Box::new(struct_expr(
                    "Outer",
                    vec![
                        field(
                            "inner",
                            struct_expr(
                                "Inner",
                                vec![field("a", lit_num("1")), field("b", lit_num("2"))],
                            ),
                        ),
                        field("flag", lit_bool(true)),
                    ],
                ))),
                Some(Expr::Group(Box::new(var("fallback")))),
            ),
        },
        ExpressionCase {
            src: "if ok { S { f: T { x: 1, y: U { z: 2 } }, g: 3 } } else { alt }",
            expected: if_expr(
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
                                    field("y", struct_expr("U", vec![field("z", lit_num("2"))])),
                                ],
                            ),
                        ),
                        field("g", lit_num("3")),
                    ],
                ))),
                Some(Expr::Group(Box::new(var("alt")))),
            ),
        },
    ]
}

fn match_and_for_expression_cases() -> Vec<ExpressionCase> {
    vec![
        ExpressionCase {
            src: "match (flag) { true -> false, false -> true }",
            expected: match_expr(
                var("flag"),
                vec![
                    match_arm("true", lit_bool(false)),
                    match_arm("false", lit_bool(true)),
                ],
            ),
        },
        ExpressionCase {
            src: "match (value) { Point { field: (x, y) } -> x, _ -> 0, }",
            expected: match_expr(
                var("value"),
                vec![
                    match_arm("Point { field: (x, y) }", var("x")),
                    match_arm("_", lit_num("0")),
                ],
            ),
        },
        ExpressionCase {
            src: "match (x) { _ -> x, }",
            expected: match_expr(var("x"), vec![match_arm("_", var("x"))]),
        },
        ExpressionCase {
            src: "for (item in items) item",
            expected: for_loop("item", var("items"), None, var("item")),
        },
        ExpressionCase {
            src: "for (entry in items if entry.active) process(entry)",
            expected: for_loop(
                "entry",
                var("items"),
                Some(field_access(var("entry"), "active")),
                call("process", vec![var("entry")]),
            ),
        },
    ]
}
