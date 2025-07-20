//!
//! AST wrapper for relation declarations.
//!
//! This module exposes [`Relation`], a thin wrapper over a `rowan` syntax node
//! representing a `relation` declaration in a `DDlog` program. The wrapper
//! provides convenient accessors for the relation name, its `input`/`output`
//! markers, declared columns and optional primary key.
//!
//! # Examples
//!
//! ```
//! use ddlint::parse;
//! use ddlint::parser::ast::Relation;
//!
//! let src = "input relation R(x: u32, y: string) primary key (x)";
//! let parsed = parse(src);
//! let rel = parsed.root().relations().first().unwrap();
//!
//! assert_eq!(rel.name(), Some("R".to_string()));
//! assert!(rel.is_input());
//! assert_eq!(rel.columns(), vec![
//!     ("x".into(), "u32".into()),
//!     ("y".into(), "string".into()),
//! ]);
//! assert_eq!(rel.primary_key(), Some(vec!["x".into()]));
//! ```

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
        if !errors.is_empty() {
            log::debug!("Parsing errors in relation columns: {errors:?}");
        }
        pairs
    }

    /// Primary key column names if specified.
    #[must_use]
    pub fn primary_key(&self) -> Option<Vec<String>> {
        let mut iter = self.syntax.children_with_tokens().peekable();
        Self::skip_to_end_of_columns(&mut iter)?;
        Self::parse_primary_key_keywords(&mut iter)?;
        Self::extract_key_list(&mut iter)
    }

    /// Skip over the column declaration list and stop on the closing `)`.
    fn skip_to_end_of_columns<I>(iter: &mut std::iter::Peekable<I>) -> Option<()>
    where
        I: Iterator<Item = rowan::SyntaxElement<DdlogLanguage>>,
    {
        let mut depth = 0usize;
        for e in iter.by_ref() {
            match e.kind() {
                SyntaxKind::T_LPAREN => depth += 1,
                SyntaxKind::T_RPAREN => {
                    if depth == 0 {
                        return None;
                    }
                    depth -= 1;
                    if depth == 0 {
                        return Some(());
                    }
                }
                _ => {}
            }
        }
        None
    }

    /// Parse the `primary key` keyword sequence.
    fn parse_primary_key_keywords<I>(iter: &mut std::iter::Peekable<I>) -> Option<()>
    where
        I: Iterator<Item = rowan::SyntaxElement<DdlogLanguage>>,
    {
        super::skip_whitespace_and_comments(iter);
        Self::expect_keyword(iter, "primary")?;
        super::skip_whitespace_and_comments(iter);
        Self::expect_keyword(iter, "key")?;
        Some(())
    }

    fn expect_keyword<I>(iter: &mut std::iter::Peekable<I>, kw: &str) -> Option<()>
    where
        I: Iterator<Item = rowan::SyntaxElement<DdlogLanguage>>,
    {
        use rowan::NodeOrToken as E;
        match iter.next() {
            Some(E::Token(t)) if t.kind() == SyntaxKind::T_IDENT && t.text() == kw => Some(()),
            _ => None,
        }
    }

    /// Extract the comma separated key names from parentheses.
    fn extract_key_list<I>(iter: &mut std::iter::Peekable<I>) -> Option<Vec<String>>
    where
        I: Iterator<Item = rowan::SyntaxElement<DdlogLanguage>>,
    {
        super::skip_whitespace_and_comments(iter);
        let content = super::parse_utils::extract_parenthesized(
            iter,
            SyntaxKind::T_LPAREN,
            SyntaxKind::T_RPAREN,
        )
        .ok()?;

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

    #[test]
    fn relation_name() {
        let parsed = parse("input relation R(x: u32)");
        #[expect(clippy::expect_used, reason = "Using expect for clearer test failures")]
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
