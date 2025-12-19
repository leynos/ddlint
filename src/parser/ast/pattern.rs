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
            Self::Var { declared, name } => Self::format_var(*declared, name),
            Self::Tuple(items) => Self::format_tuple(items),
            Self::Struct { name, fields } => Self::format_struct(name, fields),
            Self::Typed { pattern, ty } => format!("{}: {ty}", pattern.to_source()),
            Self::Literal(lit) => Self::format_literal(lit),
        }
    }

    fn format_var(declared: bool, name: &str) -> String {
        if declared {
            format!("var {name}")
        } else {
            name.to_string()
        }
    }

    fn format_tuple(items: &[Self]) -> String {
        let inner = items
            .iter()
            .map(Self::to_source)
            .reduce(|mut acc, item| {
                acc.push_str(", ");
                acc.push_str(&item);
                acc
            })
            .unwrap_or_default();
        format!("({inner})")
    }

    fn format_struct(name: &str, fields: &[(String, Self)]) -> String {
        let inner = fields
            .iter()
            .map(|(field, pat)| format!("{field}: {}", pat.to_source()))
            .reduce(|mut acc, item| {
                acc.push_str(", ");
                acc.push_str(&item);
                acc
            })
            .unwrap_or_default();

        if inner.is_empty() {
            format!("{name} {{}}")
        } else {
            format!("{name} {{ {inner} }}")
        }
    }

    fn format_literal(lit: &PatternLiteral) -> String {
        match lit {
            PatternLiteral::Int(i) => i.raw.clone(),
            PatternLiteral::String(s) => s.to_source(),
            PatternLiteral::Bool(b) => b.to_string(),
        }
    }
}
