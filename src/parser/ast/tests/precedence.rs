//! Tests for AST operator precedence.
//!
//! Ensures implication associates to the right.

use crate::SyntaxKind;
use crate::parser::ast::{BinaryOp, Expr, UnaryOp, infix_binding_power, prefix_binding_power};
use crate::parser::expression::parse_expression;
use crate::test_util::var;

#[test]
fn implication_is_right_associative() {
    let expr = parse_expression("a => b => c").unwrap_or_else(|errs| panic!("errors: {errs:?}"));
    match expr {
        Expr::Binary {
            op: BinaryOp::Imply,
            lhs,
            rhs,
        } => {
            assert_eq!(*lhs, var("a"));
            match *rhs {
                Expr::Binary {
                    op: BinaryOp::Imply,
                    lhs: ref b,
                    rhs: ref c,
                } => {
                    assert_eq!(**b, var("b"));
                    assert_eq!(**c, var("c"));
                }
                other => panic!("expected RHS implication, got {other:?}"),
            }
        }
        other => panic!("expected implication, got {other:?}"),
    }
}

#[test]
fn prefix_binding_power_matches_constants() {
    assert_eq!(
        prefix_binding_power(SyntaxKind::T_TILDE),
        Some((80, UnaryOp::BitNot))
    );
    assert_eq!(
        prefix_binding_power(SyntaxKind::T_AMP),
        Some((80, UnaryOp::Ref))
    );
}

#[test]
fn infix_binding_power_matches_constants() {
    use SyntaxKind as K;

    let cases = [
        (K::T_PLUSPLUS, (60, 61, BinaryOp::Concat)),
        (K::T_SHL, (55, 56, BinaryOp::Shl)),
        (K::T_SHR, (55, 56, BinaryOp::Shr)),
        (K::T_AMP, (45, 46, BinaryOp::BitAnd)),
        (K::T_CARET, (40, 41, BinaryOp::BitXor)),
        (K::T_PIPE, (35, 36, BinaryOp::BitOr)),
        (K::T_EQEQ, (30, 31, BinaryOp::Eq)),
        (K::T_NEQ, (30, 31, BinaryOp::Neq)),
        (K::T_LT, (30, 31, BinaryOp::Lt)),
        (K::T_LTE, (30, 31, BinaryOp::Lte)),
        (K::T_GT, (30, 31, BinaryOp::Gt)),
        (K::T_GTE, (30, 31, BinaryOp::Gte)),
    ];

    for (kind, expected) in cases {
        assert_eq!(infix_binding_power(kind), Some(expected));
    }
}
