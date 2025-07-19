//!
//! AST wrapper for `import` statements in `DDlog`.
//!
//! This module provides the `Import` struct for reading module paths such as
//! `foo::bar` and optional aliases introduced with the `as` keyword. It allows
//! consumers to navigate import statements without reimplementing token logic.

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
    #[test]
    fn parses_simple_import() {
        let parsed = parse("import foo");
        #[expect(clippy::expect_used, reason = "Using expect for clearer test failures")]
        let first = parsed
            .root()
            .imports()
            .first()
            .cloned()
            .expect("import missing");
        assert_eq!(first.path(), "foo");
        assert!(first.alias().is_none());
    }

    #[test]
    fn parses_import_with_alias() {
        let parsed = parse("import foo::bar as baz");
        #[expect(clippy::expect_used, reason = "Using expect for clearer test failures")]
        let first = parsed
            .root()
            .imports()
            .first()
            .cloned()
            .expect("import missing");
        assert_eq!(first.path(), "foo::bar");
        assert_eq!(first.alias(), Some("baz".to_string()));
    }

    #[test]
    fn parses_complex_path() {
        let parsed = parse("import std::collections::HashMap");
        #[expect(clippy::expect_used, reason = "Using expect for clearer test failures")]
        let first = parsed
            .root()
            .imports()
            .first()
            .cloned()
            .expect("import missing");
        assert_eq!(first.path(), "std::collections::HashMap");
        assert!(first.alias().is_none());
    }
}
