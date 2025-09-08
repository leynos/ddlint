//! Tests for AST operator precedence.
//!
//! Ensures implication associates to the right.

use crate::parser::ast::{BinaryOp, Expr};
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
