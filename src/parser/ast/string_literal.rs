//! String literal types used by expression AST nodes.
//!
//! Captures interpolation and interning flags so downstream passes can reason
//! about string semantics without re-parsing the source text.

/// Distinguishes the surface syntax of string literals.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StringKind {
    /// Standard quoted string supporting escape sequences and interpolation.
    Standard { interpolated: bool },
    /// Raw string delimited by `[|` and `|]`, with optional interpolation.
    Raw { interpolated: bool },
}

/// Representation of string literals including interning and interpolation.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StringLiteral {
    /// Contents of the literal without delimiters.
    pub body: String,
    /// Source syntax used to write the string.
    pub kind: StringKind,
    /// Whether the literal is prefixed with `i` and should be interned.
    pub interned: bool,
}

impl StringLiteral {
    /// True when the literal includes interpolation markers.
    #[must_use]
    pub fn is_interpolated(&self) -> bool {
        matches!(
            self.kind,
            StringKind::Standard { interpolated: true } | StringKind::Raw { interpolated: true }
        )
    }

    /// Render the literal for `Expr::to_sexpr` output.
    #[must_use]
    pub fn to_sexpr(&self) -> String {
        match self.kind {
            StringKind::Standard { .. } => {
                let rendered = format!("{:?}", self.body);
                if self.interned {
                    format!("i{rendered}")
                } else {
                    rendered
                }
            }
            StringKind::Raw { interpolated } => {
                let mut prefix = String::new();
                if self.interned {
                    prefix.push('i');
                }
                if interpolated {
                    prefix.push('$');
                }
                format!("{prefix}[|{}|]", self.body)
            }
        }
    }
}
