//! Basic expression fixtures that do not need specialised helper groupings.

use crate::parser::ast::Expr;
use crate::parser::tests::expression::fixtures::ExpressionCase;
use crate::test_util::{call, lit_bool, lit_num, lit_str, qualified_call, tuple, var};

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

pub(super) fn invalid_for_loop_sources() -> [&'static str; 4] {
    [
        "for item in items) item",
        "for (in items) item",
        "for (item items) item",
        "for (item in items if) item",
    ]
}

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
