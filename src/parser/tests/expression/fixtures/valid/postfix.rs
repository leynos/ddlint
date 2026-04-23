//! Postfix-expression fixtures for parser tests.

use crate::parser::ast::Expr;
use crate::parser::tests::expression::fixtures::ExpressionCase;
use crate::test_util::{
    bit_slice, call_expr, field_access, lit_num, method_call, tuple_index, var,
};

pub(super) fn postfix_expression_cases() -> Vec<ExpressionCase> {
    vec![
        ExpressionCase {
            src: "(f)(x)",
            expected: call_expr(Expr::Group(Box::new(var("f"))), vec![var("x")]),
        },
        ExpressionCase {
            src: "foo.bar(x)",
            expected: method_call(var("foo"), "bar", vec![var("x")]),
        },
        ExpressionCase {
            src: "foo.bar(x).baz",
            expected: field_access(method_call(var("foo"), "bar", vec![var("x")]), "baz"),
        },
        ExpressionCase {
            src: "foo.bar",
            expected: field_access(var("foo"), "bar"),
        },
        ExpressionCase {
            src: "foo.bar.baz(x)",
            expected: method_call(field_access(var("foo"), "bar"), "baz", vec![var("x")]),
        },
        ExpressionCase {
            src: "foo.bar.baz().qux",
            expected: field_access(
                method_call(field_access(var("foo"), "bar"), "baz", vec![]),
                "qux",
            ),
        },
        ExpressionCase {
            src: "foo.bar().baz(x)",
            expected: method_call(
                method_call(var("foo"), "bar", vec![]),
                "baz",
                vec![var("x")],
            ),
        },
        ExpressionCase {
            src: "foo.bar.baz",
            expected: field_access(field_access(var("foo"), "bar"), "baz"),
        },
        ExpressionCase {
            src: "foo.bar().baz.qux(x)",
            expected: method_call(
                field_access(method_call(var("foo"), "bar", vec![]), "baz"),
                "qux",
                vec![var("x")],
            ),
        },
        ExpressionCase {
            src: "e[1,0]",
            expected: bit_slice(var("e"), lit_num("1"), lit_num("0")),
        },
        ExpressionCase {
            src: "t.0",
            expected: tuple_index(var("t"), "0"),
        },
    ]
}
