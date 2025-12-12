//! Tests for `BinaryOp` symbol formatting.

use super::{BinaryOp, UnaryOp};

#[test]
fn binary_op_symbols_match_expected() {
    let cases = [
        (BinaryOp::Add, "+"),
        (BinaryOp::Sub, "-"),
        (BinaryOp::Mul, "*"),
        (BinaryOp::Div, "/"),
        (BinaryOp::Mod, "%"),
        (BinaryOp::Concat, "++"),
        (BinaryOp::Shl, "<<"),
        (BinaryOp::Shr, ">>"),
        (BinaryOp::BitAnd, "&"),
        (BinaryOp::BitXor, "^"),
        (BinaryOp::BitOr, "|"),
        (BinaryOp::Eq, "=="),
        (BinaryOp::Neq, "!="),
        (BinaryOp::Lt, "<"),
        (BinaryOp::Lte, "<="),
        (BinaryOp::Gt, ">"),
        (BinaryOp::Gte, ">="),
        (BinaryOp::And, "and"),
        (BinaryOp::Or, "or"),
        (BinaryOp::Ascribe, ":"),
        (BinaryOp::Cast, "as"),
        (BinaryOp::Assign, "="),
        (BinaryOp::Seq, ";"),
        (BinaryOp::Imply, "=>"),
    ];

    for (op, expected) in cases {
        assert_eq!(op.symbol(), expected, "BinaryOp::{op:?} symbol mismatch");
    }
}

#[test]
fn unary_op_symbols_match_expected() {
    let cases = [
        (UnaryOp::Not, "not"),
        (UnaryOp::Neg, "-"),
        (UnaryOp::BitNot, "~"),
        (UnaryOp::Ref, "&"),
    ];

    for (op, expected) in cases {
        assert_eq!(op.symbol(), expected, "UnaryOp::{op:?} symbol mismatch");
    }
}
