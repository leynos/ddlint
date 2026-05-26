//! Structured-expression fixtures for parser tests.
//!
//! These cases cover struct literals and closures, including grouped
//! expressions inside compound forms and parameter lists used by the parent
//! expression suite.

use crate::parser::ast::{BinaryOp, Expr};
use crate::parser::tests::expression::fixtures::ExpressionCase;
use crate::test_util::{closure, field, lit_num, struct_expr, tuple, var};

pub(super) fn struct_and_closure_cases() -> Vec<ExpressionCase> {
    vec![
        ExpressionCase {
            src: "Point { x: 1, y: 2 }",
            expected: struct_expr(
                "Point",
                vec![field("x", lit_num("1")), field("y", lit_num("2"))],
            ),
        },
        ExpressionCase {
            src: "Point {}",
            expected: struct_expr("Point", vec![]),
        },
        ExpressionCase {
            src: "Point { x: 1, y: 2, }",
            expected: struct_expr(
                "Point",
                vec![field("x", lit_num("1")), field("y", lit_num("2"))],
            ),
        },
        ExpressionCase {
            src: "|x, y| x + y",
            expected: closure(
                vec!["x", "y"],
                Expr::Binary {
                    op: BinaryOp::Add,
                    lhs: Box::new(var("x")),
                    rhs: Box::new(var("y")),
                },
            ),
        },
        ExpressionCase {
            src: "|| 1",
            expected: closure(std::iter::empty::<&str>(), lit_num("1")),
        },
        ExpressionCase {
            src: "|x,| x",
            expected: closure(vec!["x"], var("x")),
        },
        ExpressionCase {
            src: "Point { pair: (1, 2) }",
            expected: struct_expr(
                "Point",
                vec![field("pair", tuple(vec![lit_num("1"), lit_num("2")]))],
            ),
        },
        ExpressionCase {
            src: "(|| 1, |x| x)",
            expected: tuple(vec![
                closure(Vec::<&str>::new(), lit_num("1")),
                closure(vec!["x"], var("x")),
            ]),
        },
        ExpressionCase {
            src: "|x| Point { x: x }",
            expected: closure(vec!["x"], struct_expr("Point", vec![field("x", var("x"))])),
        },
    ]
}
