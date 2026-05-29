//! Postfix-expression fixtures for parser tests.
//!
//! These cases cover field access, method calls, indexing, diff markers, and
//! delay syntax that extend an already parsed atom in the shared fixture suite.

use crate::parser::ast::Expr;
use crate::parser::tests::expression::fixtures::ExpressionCase;
use crate::test_util::{
    atom_delay, atom_diff, bit_slice, call_expr, field_access, lit_num, method_call, tuple_index,
    var,
};

fn field_and_method_call_cases() -> Vec<ExpressionCase> {
    vec![
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
    ]
}

fn call_and_slice_cases() -> Vec<ExpressionCase> {
    vec![
        ExpressionCase {
            src: "(f)(x)",
            expected: call_expr(Expr::Group(Box::new(var("f"))), vec![var("x")]),
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

fn diff_marker_cases() -> Vec<ExpressionCase> {
    vec![
        ExpressionCase {
            src: "e'(x)",
            expected: atom_diff(call_expr(var("e"), vec![var("x")])),
        },
        ExpressionCase {
            src: "e'[1,0]",
            expected: atom_diff(bit_slice(var("e"), lit_num("1"), lit_num("0"))),
        },
        ExpressionCase {
            src: "S'(x)",
            expected: Expr::AtomDiff {
                expr: Box::new(call_expr(var("S"), vec![var("x")])),
            },
        },
    ]
}

fn delay_cases() -> Vec<ExpressionCase> {
    vec![
        ExpressionCase {
            src: "e-<5>",
            expected: atom_delay(5, var("e")),
        },
        ExpressionCase {
            src: "e-<0>",
            expected: atom_delay(0, var("e")),
        },
        ExpressionCase {
            src: "A() -<10>",
            expected: Expr::AtomDelay {
                delay: 10,
                expr: Box::new(call_expr(var("A"), vec![])),
            },
        },
    ]
}

/// Returns postfix parsing expression fixtures in ordered groups.
///
/// The result appends `field_and_method_call_cases`, then `call_and_slice_cases`,
/// then `diff_marker_cases`, then `delay_cases`, preserving that ordering for the
/// shared parser test suite.
pub(super) fn postfix_expression_cases() -> Vec<ExpressionCase> {
    let mut cases = field_and_method_call_cases();
    cases.extend(call_and_slice_cases());
    cases.extend(diff_marker_cases());
    cases.extend(delay_cases());
    cases
}
