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
//! ```rust,no_run
//! # use ddlint::parse;
//! # use ddlint::parser::ast::Relation;
//! # fn first_relation(src: &str) -> Relation {
//! #     parse(src)
//! #         .root()
//! #         .relations()
//! #         .into_iter()
//! #         .next()
//! #         .expect("relation missing")
//! # }
//! let rel = first_relation("input relation R(x: u32, y: string) primary key (x)");
//!
//! assert_eq!(rel.name(), Some("R".into()));
//! assert!(rel.is_input());
//! assert_eq!(
//!     rel.columns(),
//!     vec![
//!         ("x".into(), "u32".into()),
//!         ("y".into(), "string".into()),
//!     ]
//! );
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
        use super::parse_utils::primary_key_clause;
        use crate::tokenize;
        use chumsky::Parser as _;

        let mut iter = self.syntax.children_with_tokens().peekable();
        Self::skip_to_end_of_columns(&mut iter)?;
        let rest = iter
            .clone()
            .map(|e| match e {
                rowan::NodeOrToken::Token(t) => t.text().to_string(),
                rowan::NodeOrToken::Node(n) => n.text().to_string(),
            })
            .collect::<String>();
        if rest.is_empty() {
            return None;
        }
        let tokens = tokenize(&rest);
        let stream = chumsky::Stream::from_iter(0..rest.len(), tokens.into_iter());
        let parser = primary_key_clause(&rest);
        let (res, errs) = parser.parse_recovery(stream);
        if !errs.is_empty() {
            log::debug!("failed to parse primary key clause: {errs:?}");
        }
        res
    }

    /// Skip over the column declaration list and stop on the closing `)`.
    fn skip_to_end_of_columns<I>(iter: &mut std::iter::Peekable<I>) -> Option<()>
    where
        I: Iterator<Item = rowan::SyntaxElement<DdlogLanguage>>,
    {
        // Consume the first parenthesised list and discard content.
        super::skip_whitespace_and_comments(iter);
        if let Err(err) =
            super::parse_utils::extract_delimited(iter, SyntaxKind::T_LPAREN, SyntaxKind::T_RPAREN)
        {
            log::debug!("failed to skip relation columns: {err}");
            return None;
        }
        Some(())
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
