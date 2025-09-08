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

const BP_PREFIX: u8 = 80;
const BP_MUL_DIV_MOD: u8 = 70;
const BP_ADD_SUB: u8 = 60;
const BP_TYPE: u8 = 50;
const BP_CMP: u8 = 30;
const BP_AND: u8 = 20;
const BP_OR: u8 = 10;
const BP_ASSIGN: u8 = 8;
const BP_IMPLY: u8 = 5;
const BP_SEQ: u8 = 0;

const PREFIX_TABLE: &[(SyntaxKind, PrefixEntry)] = &[
    (
        SyntaxKind::T_MINUS,
        PrefixEntry {
            bp: BP_PREFIX,
            op: UnaryOp::Neg,
        },
    ),
    (
        SyntaxKind::K_NOT,
        PrefixEntry {
            bp: BP_PREFIX,
            op: UnaryOp::Not,
        },
    ),
];

const INFIX_TABLE: &[(SyntaxKind, InfixEntry)] = &[
    // Operator precedence rationale:
    // Ascribe (:) and Cast (as) bind tighter than logical operators and
    // assignment yet looser than arithmetic.
    // Logical operators outrank assignment, which still binds more tightly than
    // Imply (=>) and Seq (;).
    // Seq (;) remains lowest, with Imply (=>) just above it.
    (
        SyntaxKind::T_COLON,
        InfixEntry {
            l_bp: BP_TYPE, // Ascribe binds tighter than assign, looser than arithmetic
            r_bp: BP_TYPE + 1,
            op: BinaryOp::Ascribe,
        },
    ),
    (
        SyntaxKind::K_AS,
        InfixEntry {
            l_bp: BP_TYPE, // Cast matches Ascribe
            r_bp: BP_TYPE + 1,
            op: BinaryOp::Cast,
        },
    ),
    (
        SyntaxKind::T_STAR,
        InfixEntry {
            l_bp: BP_MUL_DIV_MOD,
            r_bp: BP_MUL_DIV_MOD + 1,
            op: BinaryOp::Mul,
        },
    ),
    (
        SyntaxKind::T_SLASH,
        InfixEntry {
            l_bp: BP_MUL_DIV_MOD,
            r_bp: BP_MUL_DIV_MOD + 1,
            op: BinaryOp::Div,
        },
    ),
    (
        SyntaxKind::T_PERCENT,
        InfixEntry {
            l_bp: BP_MUL_DIV_MOD,
            r_bp: BP_MUL_DIV_MOD + 1,
            op: BinaryOp::Mod,
        },
    ),
    (
        SyntaxKind::T_PLUS,
        InfixEntry {
            l_bp: BP_ADD_SUB,
            r_bp: BP_ADD_SUB + 1,
            op: BinaryOp::Add,
        },
    ),
    (
        SyntaxKind::T_MINUS,
        InfixEntry {
            l_bp: BP_ADD_SUB,
            r_bp: BP_ADD_SUB + 1,
            op: BinaryOp::Sub,
        },
    ),
    (
        SyntaxKind::T_EQEQ,
        InfixEntry {
            l_bp: BP_CMP,
            r_bp: BP_CMP + 1,
            op: BinaryOp::Eq,
        },
    ),
    (
        SyntaxKind::T_NEQ,
        InfixEntry {
            l_bp: BP_CMP,
            r_bp: BP_CMP + 1,
            op: BinaryOp::Neq,
        },
    ),
    (
        SyntaxKind::K_AND,
        InfixEntry {
            l_bp: BP_AND,
            r_bp: BP_AND + 1,
            op: BinaryOp::And,
        },
    ),
    (
        SyntaxKind::T_FAT_ARROW,
        InfixEntry {
            l_bp: BP_IMPLY, // Imply is above Seq, below Assign
            r_bp: BP_IMPLY - 1,
            op: BinaryOp::Imply,
        },
    ),
    (
        SyntaxKind::K_OR,
        InfixEntry {
            l_bp: BP_OR,
            r_bp: BP_OR + 1,
            op: BinaryOp::Or,
        },
    ),
    (
        SyntaxKind::T_EQ,
        InfixEntry {
            l_bp: BP_ASSIGN, // Assign binds looser than logical operators but above Imply and Seq
            r_bp: BP_ASSIGN - 1,
            op: BinaryOp::Assign,
        },
    ),
    (
        SyntaxKind::T_SEMI,
        InfixEntry {
            l_bp: BP_SEQ, // Seq is lowest
            r_bp: BP_SEQ + 1,
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
