//! Operator precedence table for `DDlog` expressions.
//!
//! This module centralizes binding power definitions for prefix and infix
//! operators. Keeping the precedence data here avoids inconsistencies when
//! adjusting operator rules.

use crate::SyntaxKind;

use super::{BinaryOp, UnaryOp};

#[derive(Debug, Clone, Copy)]
pub(super) struct PrefixEntry {
    pub bp: u8,
    pub op: UnaryOp,
}

#[derive(Debug, Clone, Copy)]
pub(super) struct InfixEntry {
    pub l_bp: u8,
    pub r_bp: u8,
    pub op: BinaryOp,
}

const PREFIX_TABLE: &[(SyntaxKind, PrefixEntry)] = &[
    (
        SyntaxKind::T_MINUS,
        PrefixEntry {
            bp: 80,
            op: UnaryOp::Neg,
        },
    ),
    (
        SyntaxKind::K_NOT,
        PrefixEntry {
            bp: 80,
            op: UnaryOp::Not,
        },
    ),
];

const INFIX_TABLE: &[(SyntaxKind, InfixEntry)] = &[
    // Operator precedence rationale:
    // Ascribe (:) and Cast (as) should bind tighter than Assign (=), but looser than arithmetic.
    // Assign (=) should be lower than Ascribe/Cast, but higher than Seq (;).
    // Seq (;) should be lowest, and Imply (=>) should be just above Seq.
    (
        SyntaxKind::T_COLON,
        InfixEntry {
            l_bp: 50, // Ascribe binds tighter than assign, looser than arithmetic
            r_bp: 51,
            op: BinaryOp::Ascribe,
        },
    ),
    (
        SyntaxKind::K_AS,
        InfixEntry {
            l_bp: 50, // Cast matches Ascribe
            r_bp: 51,
            op: BinaryOp::Cast,
        },
    ),
    (
        SyntaxKind::T_STAR,
        InfixEntry {
            l_bp: 70,
            r_bp: 71,
            op: BinaryOp::Mul,
        },
    ),
    (
        SyntaxKind::T_SLASH,
        InfixEntry {
            l_bp: 70,
            r_bp: 71,
            op: BinaryOp::Div,
        },
    ),
    (
        SyntaxKind::T_PERCENT,
        InfixEntry {
            l_bp: 70,
            r_bp: 71,
            op: BinaryOp::Mod,
        },
    ),
    (
        SyntaxKind::T_PLUS,
        InfixEntry {
            l_bp: 60,
            r_bp: 61,
            op: BinaryOp::Add,
        },
    ),
    (
        SyntaxKind::T_MINUS,
        InfixEntry {
            l_bp: 60,
            r_bp: 61,
            op: BinaryOp::Sub,
        },
    ),
    (
        SyntaxKind::T_EQEQ,
        InfixEntry {
            l_bp: 30,
            r_bp: 31,
            op: BinaryOp::Eq,
        },
    ),
    (
        SyntaxKind::T_NEQ,
        InfixEntry {
            l_bp: 30,
            r_bp: 31,
            op: BinaryOp::Neq,
        },
    ),
    (
        SyntaxKind::K_AND,
        InfixEntry {
            l_bp: 20,
            r_bp: 21,
            op: BinaryOp::And,
        },
    ),
    (
        SyntaxKind::T_FAT_ARROW,
        InfixEntry {
            l_bp: 5, // Imply is above Seq, below Assign
            r_bp: 4,
            op: BinaryOp::Imply,
        },
    ),
    (
        SyntaxKind::K_OR,
        InfixEntry {
            l_bp: 10,
            r_bp: 11,
            op: BinaryOp::Or,
        },
    ),
    (
        SyntaxKind::T_EQ,
        InfixEntry {
            l_bp: 30, // Assign is lower than Ascribe/Cast
            r_bp: 29,
            op: BinaryOp::Assign,
        },
    ),
    (
        SyntaxKind::T_SEMI,
        InfixEntry {
            l_bp: 0, // Seq is lowest
            r_bp: 1,
            op: BinaryOp::Seq,
        },
    ),
];

/// Lookup the binding power and [`UnaryOp`] for a prefix operator.
pub(crate) fn prefix_binding_power(kind: SyntaxKind) -> Option<(u8, UnaryOp)> {
    PREFIX_TABLE
        .iter()
        .find_map(|(k, entry)| (kind == *k).then_some((entry.bp, entry.op)))
}

/// Lookup the binding power and [`BinaryOp`] for an infix operator.
///
/// Returns the left and right binding powers along with the operator variant.
pub(crate) fn infix_binding_power(kind: SyntaxKind) -> Option<(u8, u8, BinaryOp)> {
    INFIX_TABLE
        .iter()
        .find_map(|(k, entry)| (kind == *k).then_some((entry.l_bp, entry.r_bp, entry.op)))
}
