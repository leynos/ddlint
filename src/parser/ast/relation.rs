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
    #[must_use]
    pub fn body(&self) -> RelationBody {
        self.element_type().map_or_else(
            || RelationBody::Fields(self.columns()),
            RelationBody::ElementType,
        )
    }

    /// Element type for bracket-form relations.
    #[must_use]
    pub fn element_type(&self) -> Option<String> {
        inspect::bracket_element_type(&self.syntax)
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
    #[must_use]
    pub fn columns(&self) -> Vec<(String, String)> {
        if inspect::body_open_kind(&self.syntax) == Some(SyntaxKind::T_LBRACKET) {
            return Vec::new();
        }
        let (pairs, errors) =
            super::parse_utils::parse_name_type_pairs(self.syntax.children_with_tokens());
        if !errors.is_empty() {
            log::debug!("Parsing errors in relation columns: {errors:?}");
        }
        pairs
    }

    fn preamble(&self) -> inspect::RelationPreamble {
        inspect::inspect_preamble(&self.syntax)
    }

    /// Primary key column names if specified.
    #[must_use]
    pub fn primary_key(&self) -> Option<Vec<String>> {
        use super::parse_utils::primary_key_clause;
        use crate::tokenize_with_trivia;
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
        let tokens = tokenize_with_trivia(&rest);
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
        assert_eq!(rel.name().as_deref(), Some("R"));
        assert!(rel.is_input());
        assert_eq!(rel.role(), RelationRole::Input);
        assert!(rel.role_keyword_present());
        assert_eq!(rel.kind(), RelationKind::Relation);
        assert!(rel.kind_keyword_present());
    }

    #[test]
    fn relation_name_span_points_to_declaration_identifier() {
        let source = "input relation Source(Source: u32)";
        let parsed = parse(source);
        crate::test_util::assert_no_parse_errors(parsed.errors());
        #[expect(clippy::expect_used, reason = "Using expect for clearer test failures")]
        let rel = parsed
            .root()
            .relations()
            .first()
            .cloned()
            .expect("relation missing");

        let span = rel
            .name_span()
            .unwrap_or_else(|| panic!("missing relation name_span in `{source}`"));

        assert_eq!(span_text(source, &span), "Source");
        assert_eq!(span.start, "input relation ".len());
    }

    #[test]
    fn relation_preamble_defaults_for_bare_relation() {
        let parsed = parse("R(x: u32)");
        crate::test_util::assert_no_parse_errors(parsed.errors());
        #[expect(clippy::expect_used, reason = "Using expect for clearer test failures")]
        let rel = parsed
            .root()
            .relations()
            .first()
            .cloned()
            .expect("relation missing");

        assert_eq!(rel.name().as_deref(), Some("R"));
        assert_eq!(rel.role(), RelationRole::Internal);
        assert!(!rel.role_keyword_present());
        assert_eq!(rel.kind(), RelationKind::Relation);
        assert!(!rel.kind_keyword_present());
        assert!(!rel.is_ref());
    }

    #[test]
    fn relation_kind_and_ref_are_exposed() {
        let parsed = parse("output stream & Events(event: Event)");
        crate::test_util::assert_no_parse_errors(parsed.errors());
        #[expect(clippy::expect_used, reason = "Using expect for clearer test failures")]
        let rel = parsed
            .root()
            .relations()
            .first()
            .cloned()
            .expect("relation missing");

        assert_eq!(rel.role(), RelationRole::Output);
        assert_eq!(rel.kind(), RelationKind::Stream);
        assert!(rel.kind_keyword_present());
        assert!(rel.is_ref());
    }

    #[test]
    fn bracket_body_exposes_element_type() {
        let parsed = parse("input multiset Items[Map<string, Vec<u32>>]");
        crate::test_util::assert_no_parse_errors(parsed.errors());
        #[expect(clippy::expect_used, reason = "Using expect for clearer test failures")]
        let rel = parsed
            .root()
            .relations()
            .first()
            .cloned()
            .expect("relation missing");

        assert_eq!(rel.columns(), Vec::<(String, String)>::new());
        assert_eq!(rel.element_type().as_deref(), Some("Map<string, Vec<u32>>"));
        assert_eq!(
            rel.body(),
            RelationBody::ElementType("Map<string, Vec<u32>>".into())
        );
    }
}

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
//!     vec![
//!         ("x".into(), "u32".into()),
//!         ("y".into(), "string".into()),
//!     ]
//! );
//! assert_eq!(rel.primary_key(), Some(vec!["x".into()]));
//! ```

mod inspect;

pub enum RelationKind {
    /// Plain relation, either explicit `relation` or defaulted.
    Relation,
    /// Stream relation declared with `stream`.
    Stream,
    /// Multiset relation declared with `multiset`.
    Multiset,
}

pub enum RelationRole {
    /// Relation declared with the `input` keyword.
    Input,
    /// Relation declared with the `output` keyword.
    Output,
    /// Relation with no explicit role keyword.
    Internal,
}

pub enum RelationBody {
    /// Record relation body containing named fields.
    Fields(Vec<(String, String)>),
    /// Bracket relation body containing a single element type.
    ElementType(String),
}
