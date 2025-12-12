//! Pattern AST nodes.
//!
//! Patterns appear in `match` arms, `for` bindings, and FlatMap-style rule
//! assignments. This module provides a shared representation so downstream
//! analyses can inspect pattern structure without re-parsing text.

use super::{IntLiteral, StringLiteral};

/// Literal values that can appear in patterns.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PatternLiteral {
    /// Integer literal.
    Int(IntLiteral),
    /// Constant string literal (no interpolation).
    String(StringLiteral),
    /// Boolean literal.
    Bool(bool),
}

/// Pattern nodes used in `match` arms, `for` bindings, and rule assignments.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Pattern {
    /// Wildcard pattern `_`.
    Wildcard,
    /// Variable binding pattern, optionally introduced by `var`.
    Var { declared: bool, name: String },
    /// Tuple pattern `(p1, p2, …)`.
    Tuple(Vec<Pattern>),
    /// Struct pattern `Type { field: pat, … }`.
    Struct {
        name: String,
        fields: Vec<(String, Pattern)>,
    },
    /// Typed pattern `pat: Type`.
    Typed { pattern: Box<Pattern>, ty: String },
    /// Literal pattern.
    Literal(PatternLiteral),
}

impl Pattern {
    /// Render the pattern using `DDlog` surface syntax.
    #[must_use]
    pub fn to_source(&self) -> String {
        match self {
            Self::Wildcard => "_".to_string(),
            Self::Var { declared, name } => {
                if *declared {
                    format!("var {name}")
                } else {
                    name.clone()
                }
            }
            Self::Tuple(items) => {
                let mut out = String::from("(");
                for (idx, item) in items.iter().enumerate() {
                    if idx > 0 {
                        out.push_str(", ");
                    }
                    out.push_str(&item.to_source());
                }
                out.push(')');
                out
            }
            Self::Struct { name, fields } => {
                let mut out = String::new();
                out.push_str(name);
                out.push_str(" {");
                if !fields.is_empty() {
                    out.push(' ');
                }
                for (idx, (field, pat)) in fields.iter().enumerate() {
                    if idx > 0 {
                        out.push_str(", ");
                    }
                    out.push_str(field);
                    out.push_str(": ");
                    out.push_str(&pat.to_source());
                }
                if !fields.is_empty() {
                    out.push(' ');
                }
                out.push('}');
                out
            }
            Self::Typed { pattern, ty } => format!("{}: {ty}", pattern.to_source()),
            Self::Literal(lit) => match lit {
                PatternLiteral::Int(i) => i.raw.clone(),
                PatternLiteral::String(s) => s.to_sexpr(),
                PatternLiteral::Bool(b) => b.to_string(),
            },
        }
    }
}
