//! Collection literal fixtures for expression parser tests.
//!
//! These cases exercise list and map forms, including nested expressions,
//! trailing commas, and key-value pairs that feed the shared valid-expression
//! suite.

use crate::parser::ast::{BinaryOp, Expr};
use crate::parser::tests::expression::fixtures::ExpressionCase;
use crate::test_util::{lit_num, map_entry, map_lit, var, vec_lit};

/// Returns collection literal cases as [`Vec<ExpressionCase>`].
///
/// Covers empty and numeric vectors (`[]`, `[1]`, `[1, 2]`, `[1, 2, 3]`),
/// trailing-comma vector variants (`[1,]`, `[1, 2,]`), nested element forms
/// (`[x, y + 1]`), and map literals (`{}`, `{a: 1}`, `{a: 1,}`, `{x: y, z: w}`).
/// Tests consume the returned vector to populate parameterized parser fixture
/// cases.
pub(super) fn collection_expression_cases() -> Vec<ExpressionCase> {
    vec![
        ExpressionCase {
            src: "[]",
            expected: vec_lit(vec![]),
        },
        ExpressionCase {
            src: "[1]",
            expected: vec_lit(vec![lit_num("1")]),
        },
        ExpressionCase {
            src: "[1, 2]",
            expected: vec_lit(vec![lit_num("1"), lit_num("2")]),
        },
        ExpressionCase {
            src: "[1, 2, 3]",
            expected: vec_lit(vec![lit_num("1"), lit_num("2"), lit_num("3")]),
        },
        ExpressionCase {
            src: "[1,]",
            expected: vec_lit(vec![lit_num("1")]),
        },
        ExpressionCase {
            src: "[1, 2,]",
            expected: vec_lit(vec![lit_num("1"), lit_num("2")]),
        },
        ExpressionCase {
            src: "[x, y + 1]",
            expected: vec_lit(vec![
                var("x"),
                Expr::Binary {
                    op: BinaryOp::Add,
                    lhs: Box::new(var("y")),
                    rhs: Box::new(lit_num("1")),
                },
            ]),
        },
        ExpressionCase {
            src: "{}",
            expected: map_lit(vec![]),
        },
        ExpressionCase {
            src: "{a: 1}",
            expected: map_lit(vec![map_entry(var("a"), lit_num("1"))]),
        },
        ExpressionCase {
            src: "{a: 1, b: 2}",
            expected: map_lit(vec![
                map_entry(var("a"), lit_num("1")),
                map_entry(var("b"), lit_num("2")),
            ]),
        },
        ExpressionCase {
            src: "{a: 1,}",
            expected: map_lit(vec![map_entry(var("a"), lit_num("1"))]),
        },
        ExpressionCase {
            src: "{x: y, z: w}",
            expected: map_lit(vec![
                map_entry(var("x"), var("y")),
                map_entry(var("z"), var("w")),
            ]),
        },
    ]
}
