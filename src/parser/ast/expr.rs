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
        /// Name of the function being invoked.
        name: String,
        /// Argument expressions supplied to the function.
        args: Vec<Expr>,
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
            Self::Literal(Literal::String(s)) => format!("\"{s}\""),
            Self::Literal(Literal::Bool(b)) => b.to_string(),
            Self::Variable(name) => name.clone(),
            Self::Call { name, args } => {
                if args.is_empty() {
                    format!("({name})")
                } else {
                    let args = args.iter().map(Self::to_sexpr).collect::<Vec<_>>();
                    format!("({} {})", name, args.join(" "))
                }
            }
            Self::Struct { name, fields } => {
                let fields = fields
                    .iter()
                    .map(|(n, e)| format!("({n} {})", e.to_sexpr()))
                    .collect::<Vec<_>>();
                if fields.is_empty() {
                    format!("(struct {name})")
                } else {
                    format!("(struct {name} {})", fields.join(" "))
                }
            }
            Self::Tuple(items) => {
                let items = items
                    .iter()
                    .map(Self::to_sexpr)
                    .collect::<Vec<_>>()
                    .join(" ");
                if items.is_empty() {
                    "(tuple)".to_string()
                } else {
                    format!("(tuple {items})")
                }
            }
            Self::Closure { params, body } => {
                let params = params.join(" ");
                format!("(closure ({params}) {})", body.to_sexpr())
            }
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
