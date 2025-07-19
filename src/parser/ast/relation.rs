//!
//! AST wrapper for relation declarations.

use super::AstNode;
use crate::{DdlogLanguage, SyntaxKind};

/// Typed wrapper for a relation declaration.
#[derive(Debug, Clone)]
pub struct Relation {
    pub(crate) syntax: rowan::SyntaxNode<DdlogLanguage>,
}

impl Relation {
    /// Name of the relation if present.
    #[must_use]
    pub fn name(&self) -> Option<String> {
        self.syntax
            .children_with_tokens()
            .skip_while(|e| !matches!(e.kind(), SyntaxKind::K_RELATION))
            .skip(1)
            .find_map(|e| match e {
                rowan::NodeOrToken::Token(t) if t.kind() == SyntaxKind::T_IDENT => {
                    Some(t.text().to_string())
                }
                _ => None,
            })
    }

    /// Returns `true` if declared with the `input` keyword.
    #[must_use]
    pub fn is_input(&self) -> bool {
        self.syntax
            .children_with_tokens()
            .any(|e| e.kind() == SyntaxKind::K_INPUT)
    }

    /// Returns `true` if declared with the `output` keyword.
    #[must_use]
    pub fn is_output(&self) -> bool {
        self.syntax
            .children_with_tokens()
            .any(|e| e.kind() == SyntaxKind::K_OUTPUT)
    }

    /// Columns as pairs of (name, type).
    #[must_use]
    pub fn columns(&self) -> Vec<(String, String)> {
        let (pairs, errors) =
            super::parse_utils::parse_name_type_pairs(self.syntax.children_with_tokens());
        let _ = errors;
        pairs
    }

    /// Primary key column names if specified.
    #[must_use]
    pub fn primary_key(&self) -> Option<Vec<String>> {
        use rowan::NodeOrToken as E;

        let mut iter = self.syntax.children_with_tokens().peekable();

        super::parse_utils::extract_parenthesized(
            &mut iter,
            SyntaxKind::T_LPAREN,
            SyntaxKind::T_RPAREN,
        )?;

        super::skip_whitespace_and_comments(&mut iter);
        if !matches!(
            iter.next(),
            Some(E::Token(t)) if t.kind() == SyntaxKind::T_IDENT && t.text() == "primary"
        ) {
            return None;
        }

        super::skip_whitespace_and_comments(&mut iter);
        if !matches!(
            iter.next(),
            Some(E::Token(t)) if t.kind() == SyntaxKind::T_IDENT && t.text() == "key"
        ) {
            return None;
        }

        super::skip_whitespace_and_comments(&mut iter);
        let content = super::parse_utils::extract_parenthesized(
            &mut iter,
            SyntaxKind::T_LPAREN,
            SyntaxKind::T_RPAREN,
        )?;

        let keys = content
            .split(',')
            .map(str::trim)
            .filter(|s| !s.is_empty())
            .map(str::to_string)
            .collect::<Vec<_>>();
        if keys.is_empty() { None } else { Some(keys) }
    }
}

impl_ast_node!(Relation);

#[cfg(test)]
mod tests {

    use crate::parse;

    #[expect(clippy::expect_used, reason = "Using expect for clearer test failures")]
    #[test]
    fn relation_name() {
        let parsed = parse("input relation R(x: u32)");
        let rel = parsed
            .root()
            .relations()
            .first()
            .cloned()
            .expect("relation missing");
        assert_eq!(rel.name(), Some("R".into()));
        assert!(rel.is_input());
    }
}
