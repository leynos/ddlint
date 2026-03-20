//!
//! AST wrapper for index declarations.
//!
//! This module provides a typed wrapper around `DDlog` index syntax nodes,
//! enabling structured access to the optional name, typed field list, and
//! `on` target used to build the index.

use super::AstNode;
use super::parse_utils::is_trivia;
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

    /// Typed field list declared for the index.
    #[must_use]
    pub fn fields(&self) -> Vec<(String, String)> {
        let (fields, errors) =
            super::parse_utils::parse_name_type_pairs(self.syntax.children_with_tokens());
        if !errors.is_empty() {
            log::debug!("Parsing errors in index fields: {errors:?}");
        }
        fields
    }

    /// Normalized `on` target text with trivia removed.
    #[must_use]
    pub fn on_target(&self) -> Option<String> {
        self.text_after_keyword(SyntaxKind::K_ON)
    }

    fn find_identifier_after_keyword(&self, keyword: SyntaxKind) -> Option<String> {
        let mut iter = self.syntax.children_with_tokens();
        if !super::skip_to_match(&mut iter, |k| k == keyword) {
            return None;
        }
        super::take_first_ident(iter)
    }

    fn text_after_keyword(&self, keyword: SyntaxKind) -> Option<String> {
        let mut iter = self.syntax.children_with_tokens();
        if !super::skip_to_match(&mut iter, |k| k == keyword) {
            return None;
        }

        let text = iter
            .filter(|element| !is_trivia(element))
            .map(|element| match element {
                rowan::NodeOrToken::Token(token) => token.text().to_string(),
                rowan::NodeOrToken::Node(node) => node.text().to_string(),
            })
            .collect::<String>();

        if text.is_empty() { None } else { Some(text) }
    }
}

impl_ast_node!(Index);

#[cfg(test)]
mod tests {

    use crate::parse;

    #[expect(clippy::expect_used, reason = "Using expect for clearer test failures")]
    #[test]
    fn index_fields_and_target() {
        let parsed = parse("index I(name: string) on R[lower(name)]");
        crate::test_util::assert_no_parse_errors(parsed.errors());
        let idx = parsed
            .root()
            .indexes()
            .first()
            .cloned()
            .expect("index missing");
        assert_eq!(idx.name().as_deref(), Some("I"));
        assert_eq!(
            idx.fields(),
            vec![("name".to_string(), "string".to_string())]
        );
        assert_eq!(idx.on_target().as_deref(), Some("R[lower(name)]"));
    }
}
