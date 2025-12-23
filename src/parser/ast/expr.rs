//! Expression AST for `DDlog`.
//!
//! Provides a minimal structured representation of parsed expressions.
//! This is used by the Pratt parser to build a tree that higher layers
//! can inspect without re-parsing tokens.
use std::fmt;

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
    /// Syntactically `[e1, e2, ...]`, desugars to a builder sequence:
    /// `vec_with_capacity(n); push(e1); push(e2); ...`
    VecLit(Vec<Expr>),
    /// Map literal expression.
    ///
    /// Syntactically `{k1: v1, k2: v2, ...}`, desugars to a builder sequence:
    /// `map_empty(); insert(k1, v1); insert(k2, v2); ...`
    MapLit(Vec<(Expr, Expr)>),
}
impl Expr {
    /// Display the expression as a simple S-expression for tests.
    #[must_use]
    pub fn to_sexpr(&self) -> String {
        match self {
            Self::Literal(Literal::Number(n)) => n.to_sexpr(),
            Self::Literal(Literal::String(s)) => s.to_sexpr(),
            Self::Literal(Literal::Bool(b)) => b.to_string(),
            Self::Variable(name) => name.clone(),
            Self::Call { callee, args } => format_nary(
                "call",
                std::iter::once(callee.to_sexpr()).chain(args.iter().map(Self::to_sexpr)),
            ),
            Self::MethodCall { recv, name, args } => format_nary(
                "method",
                std::iter::once(recv.to_sexpr())
                    .chain(std::iter::once(name.clone()))
                    .chain(args.iter().map(Self::to_sexpr)),
            ),
            Self::FieldAccess { expr, field } => {
                format_nary("field", [expr.to_sexpr(), field.clone()])
            }
            Self::TupleIndex { expr, index } => {
                format_nary("tuple-index", [expr.to_sexpr(), index.clone()])
            }
            Self::BitSlice { expr, hi, lo } => {
                format_nary("bitslice", [expr.to_sexpr(), hi.to_sexpr(), lo.to_sexpr()])
            }
            Self::Struct { name, fields } => format_nary(
                "struct",
                std::iter::once(name.clone())
                    .chain(fields.iter().map(|(n, e)| format_field(n, &e.to_sexpr()))),
            ),
            Self::Tuple(items) => format_nary("tuple", items.iter().map(Self::to_sexpr)),
            Self::Closure { params, body } => format_nary(
                "closure",
                [format!("({})", params.join(" ")), body.to_sexpr()],
            ),
            Self::IfElse {
                condition,
                then_branch,
                else_branch,
            } => format_if_else(condition, then_branch, else_branch),
            Self::Unary { op, expr } => format_nary(op.symbol(), std::iter::once(expr.to_sexpr())),
            Self::AtomDiff { expr } => format_nary("diff", [expr.to_sexpr()]),
            Self::AtomDelay { delay, expr } => {
                format_nary("delay", [delay.to_string(), expr.to_sexpr()])
            }
            Self::Binary { op, lhs, rhs } => {
                format_nary(op.symbol(), [lhs.to_sexpr(), rhs.to_sexpr()])
            }
            Self::Group(e) => format_nary("group", std::iter::once(e.to_sexpr())),
            Self::ForLoop {
                pattern,
                iterable,
                guard,
                body,
            } => format_for_loop(pattern, iterable, guard.as_deref(), body),
            Self::Match { scrutinee, arms } => format_match(scrutinee, arms),
            Self::Break => "(break)".to_string(),
            Self::Continue => "(continue)".to_string(),
            Self::Return { value } => format_nary("return", [value.to_sexpr()]),
            Self::VecLit(items) => format_nary("vec", items.iter().map(Self::to_sexpr)),
            Self::MapLit(entries) => {
                format_nary("map", entries.iter().map(|(k, v)| format_kv(k, v)))
            }
        }
    }
}

#[inline]
fn format_field(name: &str, value: &str) -> String {
    let mut s = String::with_capacity(name.len() + value.len() + 3);
    s.push('(');
    s.push_str(name);
    s.push(' ');
    s.push_str(value);
    s.push(')');
    s
}

#[inline]
fn format_kv(key: &Expr, value: &Expr) -> String {
    let k = key.to_sexpr();
    let v = value.to_sexpr();
    let mut s = String::with_capacity(k.len() + v.len() + 3);
    s.push('(');
    s.push_str(&k);
    s.push(' ');
    s.push_str(&v);
    s.push(')');
    s
}

fn format_nary<I>(label: &str, parts: I) -> String
where
    I: IntoIterator<Item = String>,
{
    let parts_vec: Vec<String> = parts.into_iter().collect();
    let cap = 2 + label.len() + parts_vec.iter().map(|p| 1 + p.len()).sum::<usize>();
    let mut out = String::with_capacity(cap);
    out.push('(');
    out.push_str(label);
    for part in parts_vec {
        out.push(' ');
        out.push_str(&part);
    }
    out.push(')');
    out
}

fn format_if_else(condition: &Expr, then_branch: &Expr, else_branch: &Expr) -> String {
    format_nary(
        "if",
        [
            condition.to_sexpr(),
            then_branch.to_sexpr(),
            else_branch.to_sexpr(),
        ],
    )
}

fn format_for_loop(
    pattern: &Pattern,
    iterable: &Expr,
    guard: Option<&Expr>,
    body: &Expr,
) -> String {
    let mut parts = vec![pattern.to_source(), iterable.to_sexpr()];
    if let Some(cond) = guard {
        parts.push(cond.to_sexpr());
    }
    parts.push(body.to_sexpr());
    format_nary("for", parts)
}

fn format_match(scrutinee: &Expr, arms: &[MatchArm]) -> String {
    let arm_parts = arms
        .iter()
        .map(|arm| format_nary("arm", [arm.pattern.to_source(), arm.body.to_sexpr()]));
    format_nary(
        "match",
        std::iter::once(scrutinee.to_sexpr()).chain(arm_parts),
    )
}
