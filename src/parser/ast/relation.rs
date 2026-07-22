//!
//! AST wrapper for relation declarations.
//!
//! This module exposes [`Relation`], a thin wrapper over a `rowan` syntax node
//! representing a `relation` declaration in a `DDlog` program. The wrapper
//! provides convenient accessors for the relation name, its `input`/`output`
//! markers, relation kind, body form, declared columns and optional primary
//! key.
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
//! assert_eq!(rel.name().as_deref(), Some("R"));
//! assert!(rel.is_input());
//! assert_eq!(
//!     rel.columns(),
//!     Ok(vec![
//!         ("x".into(), "u32".into()),
//!         ("y".into(), "string".into()),
//!     ])
//! );
//! assert_eq!(rel.primary_key(), Ok(Some(vec!["x".into()])));
//! ```

mod inspect;

use super::AstNode;
use crate::{DdlogLanguage, SyntaxKind};

/// Public error surface for malformed relation bodies, column lists, and
/// primary-key clauses.
///
/// Mirrors [`IndexFieldParseErrors`](super::index::IndexFieldParseErrors): a
/// vector of formatted diagnostic strings rather than raw
/// `chumsky::error::Simple<SyntaxKind>` values. `Parsed::errors()` remains the
/// primary parser-level diagnostic channel; these `Result` values make direct
/// AST querying reliable even for malformed or synthetic CST nodes.
pub type RelationParseErrors = Vec<String>;

const MALFORMED_BRACKET_BODY: &str =
    "relation bracket body is unclosed or has an unbalanced element type";
const MISSING_BODY_DELIMITER: &str = "relation declaration is missing a '(' or '[' body delimiter";

/// Typed wrapper for a relation declaration.
#[derive(Debug, Clone)]
pub struct Relation {
    pub(crate) syntax: rowan::SyntaxNode<DdlogLanguage>,
}

impl Relation {
    /// Name of the relation if present.
    #[must_use]
    pub fn name(&self) -> Option<String> {
        self.preamble().name
    }

    /// Precise source span for the relation name token, if present.
    #[must_use]
    pub fn name_span(&self) -> Option<crate::Span> {
        self.name()
            .and_then(|name| super::find_identifier_span(&self.syntax, &name))
    }

    /// Role of this relation.
    #[must_use]
    pub fn role(&self) -> RelationRole {
        self.preamble().role
    }

    /// Whether the source contained an explicit role keyword.
    #[must_use]
    pub fn role_keyword_present(&self) -> bool {
        self.preamble().role_keyword_present
    }

    /// Kind of this relation.
    #[must_use]
    pub fn kind(&self) -> RelationKind {
        self.preamble().kind
    }

    /// Whether the source contained an explicit kind keyword.
    #[must_use]
    pub fn kind_keyword_present(&self) -> bool {
        self.preamble().kind_keyword_present
    }

    /// Whether the declaration contains the `&` ref marker.
    #[must_use]
    pub fn is_ref(&self) -> bool {
        self.preamble().is_ref
    }

    /// Body form for this relation.
    ///
    /// # Errors
    ///
    /// Returns [`RelationParseErrors`] when the body cannot be structurally
    /// interpreted: an unclosed or unbalanced bracket body, a record body whose
    /// column list is malformed, or a declaration with no `(`/`[` delimiter. A
    /// malformed or missing body is never reported as an empty record body.
    pub fn body(&self) -> Result<RelationBody, RelationParseErrors> {
        match inspect::inspect_body(&self.syntax) {
            inspect::BodyInspection::Record => Ok(RelationBody::Fields(self.columns()?)),
            inspect::BodyInspection::Bracket(element) => Ok(RelationBody::ElementType(element)),
            inspect::BodyInspection::MalformedBracket => {
                Err(vec![MALFORMED_BRACKET_BODY.to_string()])
            }
            inspect::BodyInspection::MissingDelimiter => {
                Err(vec![MISSING_BODY_DELIMITER.to_string()])
            }
        }
    }

    /// Element type for bracket-form relations.
    ///
    /// `Ok(None)` means the relation has a valid record body (not malformed
    /// bracket syntax); `Ok(Some(_))` carries the bracket element type.
    ///
    /// # Errors
    ///
    /// Returns [`RelationParseErrors`] for an unclosed/unbalanced bracket body
    /// or a declaration with no `(`/`[` delimiter.
    pub fn element_type(&self) -> Result<Option<String>, RelationParseErrors> {
        match inspect::inspect_body(&self.syntax) {
            inspect::BodyInspection::Record => Ok(None),
            inspect::BodyInspection::Bracket(element) => Ok(Some(element)),
            inspect::BodyInspection::MalformedBracket => {
                Err(vec![MALFORMED_BRACKET_BODY.to_string()])
            }
            inspect::BodyInspection::MissingDelimiter => {
                Err(vec![MISSING_BODY_DELIMITER.to_string()])
            }
        }
    }

    /// Derived from [`Self::role()`]; prefer `role()` in new code.
    #[must_use]
    pub fn is_input(&self) -> bool {
        self.role() == RelationRole::Input
    }

    /// Derived from [`Self::role()`]; prefer `role()` in new code.
    #[must_use]
    pub fn is_output(&self) -> bool {
        self.role() == RelationRole::Output
    }

    /// Columns as pairs of (name, type).
    ///
    /// A valid bracket body has no columns and returns `Ok(Vec::new())`.
    ///
    /// # Errors
    ///
    /// Returns [`RelationParseErrors`] when a record body's `name: Type` list
    /// cannot be interpreted, or when the body is a malformed bracket or is
    /// missing its delimiter. A failed bracket extraction is never treated as
    /// an empty record body.
    pub fn columns(&self) -> Result<Vec<(String, String)>, RelationParseErrors> {
        match inspect::inspect_body(&self.syntax) {
            inspect::BodyInspection::Bracket(_) => Ok(Vec::new()),
            inspect::BodyInspection::Record => {
                let (pairs, errors) =
                    super::parse_utils::parse_name_type_pairs(self.syntax.children_with_tokens());
                if errors.is_empty() {
                    Ok(pairs)
                } else {
                    Err(errors.into_iter().map(|error| error.to_string()).collect())
                }
            }
            inspect::BodyInspection::MalformedBracket => {
                Err(vec![MALFORMED_BRACKET_BODY.to_string()])
            }
            inspect::BodyInspection::MissingDelimiter => {
                Err(vec![MISSING_BODY_DELIMITER.to_string()])
            }
        }
    }

    fn preamble(&self) -> inspect::RelationPreamble {
        inspect::inspect_preamble(&self.syntax)
    }

    /// Primary key binder or column names if specified.
    ///
    /// `Ok(None)` means no primary-key clause is present. Spec-form primary keys
    /// may include an opaque expression after the binder list. Typed expression
    /// access is deferred to roadmap follow-up `2.6.6.1`; this helper returns
    /// the names from the parenthesized binder/list.
    ///
    /// # Errors
    ///
    /// Returns [`RelationParseErrors`] when a `primary key` clause is present
    /// but its binder list cannot be interpreted, or when the relation body is
    /// malformed or missing so the columns cannot be skipped.
    pub fn primary_key(&self) -> Result<Option<Vec<String>>, RelationParseErrors> {
        use super::parse_utils::primary_key_clause;
        use crate::tokenize_with_trivia;
        use chumsky::Parser as _;

        match inspect::inspect_body(&self.syntax) {
            // A bracket body cannot carry a primary key; report no clause.
            inspect::BodyInspection::Bracket(_) => return Ok(None),
            inspect::BodyInspection::MalformedBracket => {
                return Err(vec![MALFORMED_BRACKET_BODY.to_string()]);
            }
            inspect::BodyInspection::MissingDelimiter => {
                return Err(vec![MISSING_BODY_DELIMITER.to_string()]);
            }
            inspect::BodyInspection::Record => {}
        }

        let mut iter = self.syntax.children_with_tokens().peekable();
        Self::skip_to_end_of_columns(&mut iter)?;
        let rest = iter
            .map(|e| match e {
                rowan::NodeOrToken::Token(t) => t.text().to_string(),
                rowan::NodeOrToken::Node(n) => n.text().to_string(),
            })
            .collect::<String>();
        let tokens = tokenize_with_trivia(&rest);
        // No `primary` keyword after the columns means there is no clause; a
        // trailing newline or comment must not be misread as a malformed one.
        if first_non_trivia_text(&tokens, &rest) != Some("primary") {
            return Ok(None);
        }
        let stream = chumsky::Stream::from_iter(0..rest.len(), tokens.into_iter());
        let parser = primary_key_clause(&rest);
        let (res, errs) = parser.parse_recovery(stream);
        if errs.is_empty() {
            Ok(res)
        } else {
            Err(errs.iter().map(|err| format!("{err:?}")).collect())
        }
    }

    /// Skip over the column declaration list and stop on the closing `)`.
    ///
    /// # Errors
    ///
    /// Returns [`RelationParseErrors`] when the parenthesized column list is
    /// absent or unclosed, so the primary-key suffix cannot be located.
    fn skip_to_end_of_columns<I>(
        iter: &mut std::iter::Peekable<I>,
    ) -> Result<(), RelationParseErrors>
    where
        I: Iterator<Item = rowan::SyntaxElement<DdlogLanguage>>,
    {
        // Consume the first parenthesised list and discard content.
        super::skip_whitespace_and_comments(iter);
        super::parse_utils::extract_delimited(iter, SyntaxKind::T_LPAREN, SyntaxKind::T_RPAREN)
            .map(|_| ())
            .map_err(|err| vec![err.to_string()])
    }
}

/// Return the source text of the first non-trivia token, if any.
fn first_non_trivia_text<'a>(
    tokens: &[(SyntaxKind, crate::Span)],
    src: &'a str,
) -> Option<&'a str> {
    tokens
        .iter()
        .find(|(kind, _)| !matches!(kind, SyntaxKind::T_WHITESPACE | SyntaxKind::T_COMMENT))
        .and_then(|(_, span)| src.get(span.clone()))
}

impl_ast_node!(Relation);

#[cfg(test)]
mod tests;

/// Kind of relation declared by a relation preamble.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RelationKind {
    /// Plain relation, either explicit `relation` or defaulted.
    Relation,
    /// Stream relation declared with `stream`.
    Stream,
    /// Multiset relation declared with `multiset`.
    Multiset,
}

/// Role of a relation declaration.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RelationRole {
    /// Relation declared with the `input` keyword.
    Input,
    /// Relation declared with the `output` keyword.
    Output,
    /// Relation with no explicit role keyword.
    Internal,
}

/// Body form used by a relation declaration.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RelationBody {
    /// Record relation body containing named fields.
    Fields(Vec<(String, String)>),
    /// Bracket relation body containing a single element type.
    ElementType(String),
}
