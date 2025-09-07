//! Expression AST for `DDlog`.
//!
//! Provides a minimal structured representation of parsed expressions.
//! This is used by the Pratt parser to build a tree that higher layers
//! can inspect without re-parsing tokens.
use std::fmt;

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

impl UnaryOp {
    fn symbol(self) -> &'static str {
        match self {
            Self::Not => "not",
            Self::Neg => "-",
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
    /// Equality operator.
    Eq,
    /// Inequality operator.
    Neq,
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
    fn symbol(self) -> &'static str {
        match self {
            Self::Add => "+",
            Self::Sub => "-",
            Self::Mul => "*",
            Self::Div => "/",
            Self::Mod => "%",
            Self::Eq => "==",
            Self::Neq => "!=",
            Self::And => "and",
            Self::Or => "or",
            Self::Ascribe => ":",
            Self::Cast => "as",
            Self::Assign => "=",
            Self::Seq => ";",
            Self::Imply => "=>",
        }
    }
}

impl fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.symbol())
    }
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
    /// Unary operation expression.
    Unary { op: UnaryOp, expr: Box<Expr> },
    /// Binary operation expression.
    Binary {
        op: BinaryOp,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    /// Grouped expression (parenthesised).
    Group(Box<Expr>),
}
impl Expr {
    /// Display the expression as a simple S-expression for tests.
    #[must_use]
    pub fn to_sexpr(&self) -> String {
        match self {
            Self::Literal(Literal::Number(n)) => n.clone(),
            Self::Literal(Literal::String(s)) => format!("{s:?}"),
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
                std::iter::once(name.clone()).chain(
                    fields
                        .iter()
                        .map(|(n, e)| format!("({} {})", n, e.to_sexpr())),
                ),
            ),
            Self::Tuple(items) => format_nary("tuple", items.iter().map(Self::to_sexpr)),
            Self::Closure { params, body } => format_nary(
                "closure",
                [format!("({})", params.join(" ")), body.to_sexpr()],
            ),
            Self::Unary { op, expr } => format_nary(op.symbol(), std::iter::once(expr.to_sexpr())),
            Self::Binary { op, lhs, rhs } => {
                format_nary(op.symbol(), [lhs.to_sexpr(), rhs.to_sexpr()])
            }
            Self::Group(e) => format_nary("group", std::iter::once(e.to_sexpr())),
        }
    }
}

fn format_nary<I>(label: &str, parts: I) -> String
where
    I: IntoIterator<Item = String>,
{
    let mut out = String::from("(");
    out.push_str(label);
    for part in parts {
        out.push(' ');
        out.push_str(&part);
    }
    out.push(')');
    out
}
