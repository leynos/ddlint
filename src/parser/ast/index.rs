//!
//! AST wrapper for index declarations.

use super::AstNode;
use crate::{DdlogLanguage, SyntaxKind};

/// Typed wrapper for an index declaration.
#[derive(Debug, Clone)]
pub struct Index {
    pub(crate) syntax: rowan::SyntaxNode<DdlogLanguage>,
}

impl Index {
    /// Name of the index if present.
    #[must_use]
    pub fn name(&self) -> Option<String> {
        self.syntax
            .children_with_tokens()
            .skip_while(|e| !matches!(e.kind(), SyntaxKind::K_INDEX))
            .skip(1)
            .find_map(|e| match e {
                rowan::NodeOrToken::Token(t) if t.kind() == SyntaxKind::T_IDENT => {
                    Some(t.text().to_string())
                }
                _ => None,
            })
    }

    /// Target relation name.
    #[must_use]
    pub fn relation(&self) -> Option<String> {
        self.syntax
            .children_with_tokens()
            .skip_while(|e| !matches!(e.kind(), SyntaxKind::K_ON))
            .skip(1)
            .find_map(|e| match e {
                rowan::NodeOrToken::Token(t) if t.kind() == SyntaxKind::T_IDENT => {
                    Some(t.text().to_string())
                }
                _ => None,
            })
    }

    /// Column expressions included in the index.
    #[must_use]
    pub fn columns(&self) -> Vec<String> {
        use rowan::NodeOrToken;

        let mut iter = self.syntax.children_with_tokens().peekable();
        for e in &mut iter {
            if e.kind() == SyntaxKind::T_LPAREN {
                break;
            }
        }

        let mut cols = Vec::new();
        let mut buf = String::new();
        let mut depth = 0usize;

        for e in iter {
            match e {
                NodeOrToken::Token(t) => match t.kind() {
                    SyntaxKind::T_LPAREN => {
                        depth += 1;
                        buf.push_str(t.text());
                    }
                    SyntaxKind::T_RPAREN => {
                        if depth == 0 {
                            let col = buf.trim();
                            if !col.is_empty() {
                                cols.push(col.to_string());
                            }
                            break;
                        }
                        depth -= 1;
                        buf.push_str(t.text());
                    }
                    SyntaxKind::T_COMMA if depth == 0 => {
                        let col = buf.trim();
                        if !col.is_empty() {
                            cols.push(col.to_string());
                        }
                        buf.clear();
                    }
                    _ => buf.push_str(t.text()),
                },
                NodeOrToken::Node(n) => buf.push_str(&n.text().to_string()),
            }
        }

        cols
    }
}

impl_ast_node!(Index);

#[cfg(test)]
mod tests {

    use crate::parse;

    #[expect(clippy::expect_used, reason = "Using expect for clearer test failures")]
    #[test]
    fn index_columns() {
        let parsed = parse("index I on R(lower(name))");
        let idx = parsed
            .root()
            .indexes()
            .first()
            .cloned()
            .expect("index missing");
        assert_eq!(idx.relation(), Some("R".into()));
        assert_eq!(idx.columns(), vec!["lower(name)".to_string()]);
    }
}
