//!
//! AST wrapper for index declarations.
//!
//! This module provides a typed wrapper around `DDlog` index syntax nodes,
//! enabling structured access to the optional name, target relation and column
//! expressions used to build the index.

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
        self.find_identifier_after_keyword(SyntaxKind::K_INDEX)
    }

    /// Target relation name.
    #[must_use]
    pub fn relation(&self) -> Option<String> {
        self.find_identifier_after_keyword(SyntaxKind::K_ON)
    }

    fn find_identifier_after_keyword(&self, keyword: SyntaxKind) -> Option<String> {
        let mut iter = self.syntax.children_with_tokens();
        if !super::skip_to_match(&mut iter, |k| k == keyword) {
            return None;
        }
        super::take_first_ident(iter)
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
        crate::test_util::assert_no_parse_errors(parsed.errors());
        let idx = parsed
            .root()
            .indexes()
            .first()
            .cloned()
            .expect("index missing");
        assert_eq!(idx.name().as_deref(), Some("I"));
        assert_eq!(idx.relation().as_deref(), Some("R"));
        assert_eq!(idx.columns(), vec!["lower(name)".to_string()]);
    }
}
