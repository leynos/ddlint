//! Expression AST for `DDlog`.
//!
//! Provides a minimal structured representation of parsed expressions.
//! This is used by the Pratt parser to build a tree that higher layers
//! can inspect without re-parsing tokens.

/// Literal values that can appear in expressions.
#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    /// Numeric literal, stored as written in the source.
    Number(String),
    /// String literal including escape sequences.
    String(String),
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
}

/// Binary operators in expressions.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Eq,
    Neq,
    And,
    Or,
}

/// Parsed expression tree.
#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Literal(Literal),
    Variable(String),
    Unary {
        op: UnaryOp,
        expr: Box<Expr>,
    },
    Binary {
        op: BinaryOp,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    Group(Box<Expr>),
}

impl Expr {
    /// Display the expression as a simple S-expression for tests.
    #[must_use]
    pub fn to_sexpr(&self) -> String {
        match self {
            Self::Literal(Literal::Number(n)) => n.clone(),
            Self::Literal(Literal::String(s)) => format!("\"{s}\""),
            Self::Literal(Literal::Bool(b)) => b.to_string(),
            Self::Variable(name) => name.clone(),
            Self::Unary { op, expr } => {
                let op_str = match op {
                    UnaryOp::Not => "not",
                    UnaryOp::Neg => "-",
                };
                format!("({} {})", op_str, expr.to_sexpr())
            }
            Self::Binary { op, lhs, rhs } => {
                let op_str = match op {
                    BinaryOp::Add => "+",
                    BinaryOp::Sub => "-",
                    BinaryOp::Mul => "*",
                    BinaryOp::Div => "/",
                    BinaryOp::Mod => "%",
                    BinaryOp::Eq => "==",
                    BinaryOp::Neq => "!=",
                    BinaryOp::And => "and",
                    BinaryOp::Or => "or",
                };
                format!("({} {} {})", op_str, lhs.to_sexpr(), rhs.to_sexpr())
            }
            Self::Group(e) => format!("(group {})", e.to_sexpr()),
        }
    }
}
