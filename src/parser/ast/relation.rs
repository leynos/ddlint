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
        match self.inspected_element_type()? {
            Some(element) => Ok(RelationBody::ElementType(element)),
            None => Ok(RelationBody::Fields(self.columns()?)),
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
        self.inspected_element_type()
    }

    /// Interpret the body delimiter, the single owner of the
    /// [`inspect::inspect_body`] match shared by [`Self::body`] and
    /// [`Self::element_type`].
    ///
    /// `Ok(None)` marks a valid record body, `Ok(Some(_))` a valid bracket
    /// element type, and `Err(..)` a malformed bracket or missing delimiter.
    /// It deliberately does not inspect record columns, so a valid record
    /// delimiter with a malformed column list still yields `Ok(None)` here;
    /// column validation is [`Self::columns`]'s responsibility.
    fn inspected_element_type(&self) -> Result<Option<String>, RelationParseErrors> {
        let inspection = inspect::inspect_body(&self.syntax);
        body_inspection_error(&inspection)?;
        Ok(match inspection {
            inspect::BodyInspection::Bracket(element) => Some(element),
            // `Record`; the error variants were rejected above.
            _ => None,
        })
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
        let inspection = inspect::inspect_body(&self.syntax);
        body_inspection_error(&inspection)?;
        if matches!(inspection, inspect::BodyInspection::Bracket(_)) {
            return Ok(Vec::new());
        }
        // `Record` body: parse the `name: Type` list.
        let (pairs, errors) =
            super::parse_utils::parse_name_type_pairs(self.syntax.children_with_tokens());
        if errors.is_empty() {
            Ok(pairs)
        } else {
            Err(errors.into_iter().map(|error| error.to_string()).collect())
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

        let inspection = inspect::inspect_body(&self.syntax);
        body_inspection_error(&inspection)?;
        if matches!(inspection, inspect::BodyInspection::Bracket(_)) {
            // A bracket body cannot carry a primary key; report no clause.
            return Ok(None);
        }

        // `Record` body from here.
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
            Err(errs.iter().map(render_parse_error).collect())
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

/// Map the error-carrying body inspections to their diagnostics.
///
/// Returns `Err` for a malformed bracket or a missing delimiter, and `Ok(())`
/// for the structurally valid record and bracket bodies, letting the accessors
/// short-circuit before handling their own `Record`/`Bracket` success cases.
fn body_inspection_error(inspection: &inspect::BodyInspection) -> Result<(), RelationParseErrors> {
    match inspection {
        inspect::BodyInspection::MalformedBracket => Err(vec![MALFORMED_BRACKET_BODY.to_string()]),
        inspect::BodyInspection::MissingDelimiter => Err(vec![MISSING_BODY_DELIMITER.to_string()]),
        inspect::BodyInspection::Record | inspect::BodyInspection::Bracket(_) => Ok(()),
    }
}

/// Render a chumsky `Simple` parse error as a human-readable diagnostic.
///
/// `SyntaxKind` has no `Display`, so `Simple`'s own `Display` is unavailable;
/// this uses the `reason`/`expected`/`found` accessors with token symbols to
/// avoid emitting `Debug` struct dumps into [`RelationParseErrors`].
fn render_parse_error(err: &chumsky::error::Simple<SyntaxKind>) -> String {
    use chumsky::error::SimpleReason;

    match err.reason() {
        SimpleReason::Custom(message) => message.clone(),
        SimpleReason::Unclosed { delimiter, .. } => {
            format!("unclosed {}", display_kind(*delimiter))
        }
        SimpleReason::Unexpected => {
            let found = err
                .found()
                .map_or_else(|| "end of input".to_string(), |kind| display_kind(*kind));
            let mut expected: Vec<String> = err
                .expected()
                .filter_map(|kind| kind.as_ref().map(|kind| display_kind(*kind)))
                .collect();
            expected.sort();
            if expected.is_empty() {
                format!("unexpected {found}")
            } else {
                format!("expected {} but found {found}", expected.join(" or "))
            }
        }
    }
}

/// Render a single [`SyntaxKind`] as a token symbol, falling back to its name.
fn display_kind(kind: SyntaxKind) -> String {
    match crate::parser::lexer_helpers::token_display(kind) {
        "" => format!("{kind:?}"),
        symbol => format!("`{symbol}`"),
    }
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
