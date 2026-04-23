//! Expression AST for `DDlog`.
//!
//! Provides a minimal structured representation of parsed expressions.
//! This is used by the Pratt parser to build a tree that higher layers
//! can inspect without re-parsing tokens.
use std::fmt;

mod sexpr;

use super::number::NumberLiteral;
use super::pattern::Pattern;
use super::string_literal::StringLiteral;

/// Literal values that can appear in expressions.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Literal {
    /// Numeric literal with optional width and sign.
    Number(NumberLiteral),
    /// String literal including escape sequences.
    String(StringLiteral),
    /// Boolean literal.
    Bool(bool),
}
/// Unary operators in expressions.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
    /// Logical negation.
    Not,
    /// Arithmetic negation.
    Neg,
    /// Bitwise NOT.
    BitNot,
    /// Reference (address-of).
    Ref,
}

impl UnaryOp {
    fn symbol(self) -> &'static str {
        match self {
            Self::Not => "not",
            Self::Neg => "-",
            Self::BitNot => "~",
            Self::Ref => "&",
        }
    }
}

impl fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.symbol())
    }
}

/// Binary operators in expressions.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp {
    /// Addition operator.
    Add,
    /// Subtraction operator.
    Sub,
    /// Multiplication operator.
    Mul,
    /// Division operator.
    Div,
    /// Modulo operator.
    Mod,
    /// Concatenation operator.
    Concat,
    /// Left shift operator.
    Shl,
    /// Right shift operator.
    Shr,
    /// Bitwise AND operator.
    BitAnd,
    /// Bitwise XOR operator.
    BitXor,
    /// Bitwise OR operator.
    BitOr,
    /// Equality operator.
    Eq,
    /// Inequality operator.
    Neq,
    /// Less than operator.
    Lt,
    /// Less than or equal operator.
    Lte,
    /// Greater than operator.
    Gt,
    /// Greater than or equal operator.
    Gte,
    /// Logical AND operator.
    And,
    /// Logical OR operator.
    Or,
    /// Type ascription operator.
    Ascribe,
    /// Cast operator.
    Cast,
    /// Assignment operator.
    Assign,
    /// Sequencing operator.
    Seq,
    /// Logical implication operator.
    Imply,
}

impl BinaryOp {
    // Keep this list in the same order as the enum declaration.
    const SYMBOLS: [&'static str; 24] = [
        "+",   // Add
        "-",   // Sub
        "*",   // Mul
        "/",   // Div
        "%",   // Mod
        "++",  // Concat
        "<<",  // Shl
        ">>",  // Shr
        "&",   // BitAnd
        "^",   // BitXor
        "|",   // BitOr
        "==",  // Eq
        "!=",  // Neq
        "<",   // Lt
        "<=",  // Lte
        ">",   // Gt
        ">=",  // Gte
        "and", // And
        "or",  // Or
        ":",   // Ascribe
        "as",  // Cast
        "=",   // Assign
        ";",   // Seq
        "=>",  // Imply
    ];

    #[expect(
        clippy::indexing_slicing,
        reason = "SYMBOLS is indexed by BinaryOp discriminants and is kept in sync via a compile-time length check"
    )]
    fn symbol(self) -> &'static str {
        Self::SYMBOLS[self as usize]
    }
}

const _: [(); BinaryOp::SYMBOLS.len()] = [(); (BinaryOp::Imply as usize) + 1];

impl fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.symbol())
    }
}

#[cfg(test)]
mod tests;

/// Pattern arm inside a [`Expr::Match`] expression.
#[derive(Debug, Clone, PartialEq)]
pub struct MatchArm {
    /// Pattern text matched against the scrutinee.
    pub pattern: Pattern,
    /// Expression evaluated when the pattern matches.
    pub body: Expr,
}

/// Parsed expression tree.
#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    /// Literal value expression.
    Literal(Literal),
    /// Variable reference expression.
    Variable(String),
    /// Unresolved application expression.
    ///
    /// This variant captures syntactic `name(...)` application forms that are
    /// not parse-time qualified function calls. Name resolution disambiguates
    /// these later.
    Apply {
        /// Callee expression being applied.
        callee: Box<Expr>,
        /// Argument expressions supplied to the callee.
        args: Vec<Expr>,
    },
    /// Function call expression.
    Call {
        /// Callee expression being invoked.
        callee: Box<Expr>,
        /// Argument expressions supplied to the function.
        args: Vec<Expr>,
    },
    /// Method call expression.
    MethodCall {
        /// Receiver expression providing the method context.
        recv: Box<Expr>,
        /// Name of the method being invoked.
        name: String,
        /// Argument expressions supplied to the method.
        args: Vec<Expr>,
    },
    /// Field access expression.
    FieldAccess {
        /// Expression whose field is being accessed.
        expr: Box<Expr>,
        /// Name of the field.
        field: String,
    },
    /// Tuple index expression.
    TupleIndex {
        /// Expression to index.
        expr: Box<Expr>,
        /// String form of the tuple index.
        index: String,
    },
    /// Bit slice expression.
    BitSlice {
        /// Expression being sliced.
        expr: Box<Expr>,
        /// High index of the slice.
        hi: Box<Expr>,
        /// Low index of the slice.
        lo: Box<Expr>,
    },
    /// Struct literal expression.
    Struct {
        /// Name of the struct being constructed.
        name: String,
        /// Field initializers in declaration order.
        fields: Vec<(String, Expr)>,
    },
    /// Tuple literal expression.
    Tuple(Vec<Expr>),
    /// Closure literal expression.
    Closure {
        /// Parameters introduced by the closure.
        params: Vec<String>,
        /// Body expression executed when invoked.
        body: Box<Expr>,
    },
    /// If expression with an optional `else` branch.
    IfElse {
        /// Condition controlling the selected branch.
        condition: Box<Expr>,
        /// Expression evaluated when the condition is truthy.
        then_branch: Box<Expr>,
        /// Expression evaluated when the condition is falsy.
        else_branch: Box<Expr>,
    },
    /// Unary operation expression.
    Unary { op: UnaryOp, expr: Box<Expr> },
    /// Atom diff marker (`Rel'(...)`), represented as a wrapper around the
    /// underlying atom expression.
    AtomDiff {
        /// Atom expression being marked as a diff atom.
        expr: Box<Expr>,
    },
    /// Atom delay marker (`Atom -<N>`), represented as a wrapper around the
    /// underlying atom expression.
    AtomDelay {
        /// Delay in ticks applied to the atom.
        delay: u32,
        /// Atom expression receiving the delay.
        expr: Box<Expr>,
    },
    /// Binary operation expression.
    Binary {
        op: BinaryOp,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    /// Grouped expression (parenthesised).
    Group(Box<Expr>),
    /// For-loop expression with optional guard.
    ForLoop {
        /// Pattern introduced by the loop header.
        pattern: Pattern,
        /// Iterable expression supplying loop items.
        iterable: Box<Expr>,
        /// Optional guard expression restricting iterations.
        guard: Option<Box<Expr>>,
        /// Statement executed for each matching element.
        body: Box<Expr>,
    },
    /// Match expression with one or more pattern arms.
    Match {
        /// Expression being scrutinised.
        scrutinee: Box<Expr>,
        /// Pattern arms evaluated in order.
        arms: Vec<MatchArm>,
    },
    /// Break expression terminating the innermost loop.
    Break,
    /// Continue expression resuming the next loop iteration.
    Continue,
    /// Return expression exiting the current function.
    Return {
        /// Value returned to the caller (defaults to `()` when omitted).
        value: Box<Expr>,
    },
    /// Vector literal expression.
    ///
    /// Syntactically `[e1, e2, ...]`, preserved as a raw AST node during
    /// parsing. Builder-sequence desugaring (for example to
    /// `vec_with_capacity(n); push(e1); push(e2); ...`) is scheduled work for
    /// later lowering stages, not current parser behaviour.
    VecLit(Vec<Expr>),
    /// Map literal expression.
    ///
    /// Syntactically `{k1: v1, k2: v2, ...}`, preserved as a raw AST node
    /// during parsing. Builder-sequence desugaring (for example to
    /// `map_empty(); insert(k1, v1); insert(k2, v2); ...`) is scheduled work
    /// for later lowering stages, not current parser behaviour.
    MapLit(Vec<(Expr, Expr)>),
}
