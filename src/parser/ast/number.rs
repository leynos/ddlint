//! Numeric literal types used by the expression AST.
//!
//! These structures capture parsed widths, bases, and signedness so later
//! stages can reason about numeric semantics without re-parsing source text.

use num_bigint::BigInt;

/// Base used by integer literals.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IntBase {
    /// Binary integer literal.
    Binary,
    /// Octal integer literal.
    Octal,
    /// Decimal integer literal.
    Decimal,
    /// Hexadecimal integer literal.
    Hex,
}

impl IntBase {
    /// Radix associated with the base.
    #[must_use]
    pub const fn radix(self) -> u32 {
        match self {
            Self::Binary => 2,
            Self::Octal => 8,
            Self::Decimal => 10,
            Self::Hex => 16,
        }
    }
}

/// Parsed integer literal capturing width and base information.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IntLiteral {
    /// Original source text for rendering and diagnostics.
    pub raw: String,
    /// Optional bit width specified by the literal.
    pub width: Option<u32>,
    /// Numeric base used for parsing.
    pub base: IntBase,
    /// True when the literal uses a signed base prefix (e.g. `'sd`).
    pub signed: bool,
    /// Parsed numeric value for range checks and semantic analysis.
    pub value: BigInt,
}

/// Parsed floating-point literal, optionally width-qualified.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FloatLiteral {
    /// Original source text for rendering and diagnostics.
    pub raw: String,
    /// Optional bit width specified by the literal (e.g. `32` or `64`).
    pub width: Option<u32>,
}

/// Numeric literal covering integer and floating-point forms.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum NumberLiteral {
    /// Integer literal with optional width and sign.
    Int(IntLiteral),
    /// Floating-point literal with optional width.
    Float(FloatLiteral),
}

impl NumberLiteral {
    /// Render the literal in source form for `Expr::to_sexpr`.
    #[must_use]
    pub fn to_sexpr(&self) -> String {
        match self {
            Self::Int(int) => int.raw.clone(),
            Self::Float(float) => float.raw.clone(),
        }
    }
}
