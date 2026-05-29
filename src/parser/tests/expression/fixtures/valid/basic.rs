//! Core expression fixtures for the simplest valid forms.
//!
//! These cases cover atoms, calls, tuples, and literals that the parser must
//! accept before the specialised fixture modules add broader syntax coverage.

use crate::parser::ast::Expr;
use crate::parser::tests::expression::fixtures::ExpressionCase;
use crate::test_util::{call, lit_bool, lit_num, lit_str, qualified_call, tuple, var};

/// Returns test cases for basic expressions as [`Vec<ExpressionCase>`].
///
/// # Returns
/// A vector of simple atomic and grouped valid expressions used by shared parser
/// tests.
pub(super) fn basic_expression_cases() -> Vec<ExpressionCase> {
    vec![
        ExpressionCase {
            src: "x",
            expected: var("x"),
        },
        ExpressionCase {
            src: "foo()",
            expected: call("foo", vec![]),
        },
        ExpressionCase {
            src: "pkg::Foo()",
            expected: call("pkg::Foo", vec![]),
        },
        ExpressionCase {
            src: "pkg::foo()",
            expected: qualified_call("pkg::foo", vec![]),
        },
        ExpressionCase {
            src: "add(x, 1)",
            expected: call("add", vec![var("x"), lit_num("1")]),
        },
        ExpressionCase {
            src: "(1, 2)",
            expected: tuple(vec![lit_num("1"), lit_num("2")]),
        },
        ExpressionCase {
            src: "(1, 2, 3)",
            expected: tuple(vec![lit_num("1"), lit_num("2"), lit_num("3")]),
        },
        ExpressionCase {
            src: "(1,)",
            expected: tuple(vec![lit_num("1")]),
        },
        ExpressionCase {
            src: "()",
            expected: tuple(vec![]),
        },
        ExpressionCase {
            src: "(1)",
            expected: Expr::Group(Box::new(lit_num("1"))),
        },
    ]
}

/// Returns invalid for-loop source fixtures as [`[&'static str; 4]`].
///
/// # Returns
/// A fixed-size array of source strings intentionally written to trigger parse
/// failures in for-loop syntax tests.
pub(super) fn invalid_for_loop_sources() -> [&'static str; 4] {
    [
        "for item in items) item",
        "for (in items) item",
        "for (item items) item",
        "for (item in items if) item",
    ]
}

/// Returns literal expression cases as [`Vec<ExpressionCase>`].
///
/// # Returns
/// String, boolean, and numeric literal fixtures used by parser test cases that
/// focus on atomic expression parsing.
pub(super) fn literal_cases() -> Vec<ExpressionCase> {
    vec![
        ExpressionCase {
            src: "\"hi\"",
            expected: lit_str("hi"),
        },
        ExpressionCase {
            src: "true",
            expected: lit_bool(true),
        },
        ExpressionCase {
            src: "false",
            expected: lit_bool(false),
        },
        ExpressionCase {
            src: "42",
            expected: lit_num("42"),
        },
    ]
}
