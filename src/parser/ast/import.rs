//!
//! AST wrapper for an `import` statement.
//!
//! Provides helpers to read the module path and optional alias.

use super::AstNode;
use crate::{DdlogLanguage, SyntaxKind};

/// Typed wrapper for an `import` statement.
#[derive(Debug, Clone)]
pub struct Import {
    pub(crate) syntax: rowan::SyntaxNode<DdlogLanguage>,
}

impl Import {
    /// The module path text as written in the source.
    #[must_use]
    pub fn path(&self) -> String {
        self.syntax
            .children_with_tokens()
            .skip_while(|e| !matches!(e.kind(), SyntaxKind::K_IMPORT))
            .skip(1)
            .take_while(|e| !matches!(e.kind(), SyntaxKind::K_AS))
            .filter_map(|e| match e {
                rowan::NodeOrToken::Token(t) if t.kind() == SyntaxKind::T_IDENT => {
                    Some(t.text().to_string())
                }
                rowan::NodeOrToken::Token(t) if t.kind() == SyntaxKind::T_COLON_COLON => {
                    Some("::".to_string())
                }
                _ => None,
            })
            .collect::<String>()
    }

    /// The alias assigned with `as`, if any.
    #[must_use]
    pub fn alias(&self) -> Option<String> {
        self.syntax
            .children_with_tokens()
            .skip_while(|e| !matches!(e.kind(), SyntaxKind::K_AS))
            .skip(1)
            .find_map(|e| match e {
                rowan::NodeOrToken::Token(t) if t.kind() == SyntaxKind::T_IDENT => {
                    Some(t.text().to_string())
                }
                _ => None,
            })
    }
}

impl_ast_node!(Import);

#[cfg(test)]
mod tests {

    use crate::parse;
    #[expect(clippy::expect_used, reason = "Using expect for clearer test failures")]
    #[test]
    fn parses_simple_import() {
        let parsed = parse("import foo");
        let first = parsed
            .root()
            .imports()
            .first()
            .cloned()
            .expect("import missing");
        assert_eq!(first.path(), "foo");
        assert!(first.alias().is_none());
    }
}
