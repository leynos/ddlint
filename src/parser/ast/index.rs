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
        crate::syntax_utils::parse_parenthesized_list(self.syntax.children_with_tokens())
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
