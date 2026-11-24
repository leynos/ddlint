//! Typed access to spec-form primary-key clauses.
//!
//! `DDlog`'s specification allows an input relation to declare
//! `primary key (binder, ...)` followed by an optional expression that selects
//! the key from the bound row. The clause already lives in the relation's CST;
//! this module exposes its binders, clause span, and expression text without
//! adding a new node.

mod clause;

use chumsky::error::Simple;

use super::{Relation, RelationParseErrors};
use crate::{
    Span,
    SyntaxKind,
    parser::{ast::Expr, expression::parse_expression, span_utils::shift_errors},
};

/// A spec-form primary-key clause on a relation declaration.
///
/// The binders are the names written inside the parenthesized list. The
/// optional expression is the source text that follows the list, such as
/// `(row.author, row.title)`.
///
/// # Examples
///
/// ```
/// # use ddlint::parse;
/// #
/// # let parsed = parse("input relation Book(row: BookRow) primary key (row)");
/// # let relation = parsed
/// #     .root()
/// #     .relations()
/// #     .into_iter()
/// #     .next()
/// #     .expect("relation");
/// # let key = relation
/// #     .primary_key()
/// #     .expect("valid key")
/// #     .expect("key present");
/// assert_eq!(key.binder_names(), ["row"]);
/// assert!(key.expression().is_none());
/// ```
#[derive(Debug, Clone, PartialEq)]
pub struct PrimaryKey {
    binders: Vec<PrimaryKeyBinder>,
    span: Span,
    expression: Option<PrimaryKeyExpression>,
}

impl PrimaryKey {
    /// Binders named in the clause's parenthesized list.
    ///
    /// # Examples
    ///
    /// ```
    /// # use ddlint::parse;
    /// #
    /// # let parsed = parse("input relation R(x: u32, y: u32) primary key (x, y)");
    /// # let relation = parsed
    /// #     .root()
    /// #     .relations()
    /// #     .into_iter()
    /// #     .next()
    /// #     .expect("relation");
    /// # let key = relation
    /// #     .primary_key()
    /// #     .expect("valid key")
    /// #     .expect("key present");
    /// assert_eq!(key.binders().len(), 2);
    /// ```
    #[must_use]
    pub fn binders(&self) -> &[PrimaryKeyBinder] { &self.binders }

    /// Binder names in declaration order.
    ///
    /// # Examples
    ///
    /// ```
    /// # use ddlint::parse;
    /// #
    /// # let parsed = parse("input relation R(x: u32) primary key (x) x.id");
    /// # let relation = parsed
    /// #     .root()
    /// #     .relations()
    /// #     .into_iter()
    /// #     .next()
    /// #     .expect("relation");
    /// # let key = relation
    /// #     .primary_key()
    /// #     .expect("valid key")
    /// #     .expect("key present");
    /// assert_eq!(key.binder_names(), ["x"]);
    /// ```
    #[must_use]
    pub fn binder_names(&self) -> Vec<&str> {
        self.binders.iter().map(PrimaryKeyBinder::name).collect()
    }

    /// Span from the `primary` keyword through the end of the expression.
    ///
    /// When there is no expression, the span ends with the closing `)` of the
    /// binder list.
    ///
    /// # Examples
    ///
    /// ```
    /// # use ddlint::parse;
    /// #
    /// # let src = "input relation R(x: u32) primary key (x)";
    /// # let parsed = parse(src);
    /// # let relation = parsed
    /// #     .root()
    /// #     .relations()
    /// #     .into_iter()
    /// #     .next()
    /// #     .expect("relation");
    /// # let key = relation
    /// #     .primary_key()
    /// #     .expect("valid key")
    /// #     .expect("key present");
    /// assert_eq!(&src[key.span()], "primary key (x)");
    /// ```
    #[must_use]
    pub fn span(&self) -> Span { self.span.clone() }

    /// Expression that selects the key from the binders, if written.
    ///
    /// # Examples
    ///
    /// ```
    /// # use ddlint::parse;
    /// #
    /// # let parsed = parse("input relation R(row: Row) primary key (row) row.id");
    /// # let relation = parsed
    /// #     .root()
    /// #     .relations()
    /// #     .into_iter()
    /// #     .next()
    /// #     .expect("relation");
    /// # let key = relation
    /// #     .primary_key()
    /// #     .expect("valid key")
    /// #     .expect("key present");
    /// assert_eq!(key.expression().map(|expr| expr.text()), Some("row.id"));
    /// ```
    #[must_use]
    pub const fn expression(&self) -> Option<&PrimaryKeyExpression> { self.expression.as_ref() }
}

/// A single binder in a primary-key clause.
///
/// # Examples
///
/// ```
/// # use ddlint::parse;
/// #
/// # let src = "input relation R(row: Row) primary key (row)";
/// # let parsed = parse(src);
/// # let relation = parsed
/// #     .root()
/// #     .relations()
/// #     .into_iter()
/// #     .next()
/// #     .expect("relation");
/// # let key = relation
/// #     .primary_key()
/// #     .expect("valid key")
/// #     .expect("key present");
/// # let binder = key.binders().first().expect("binder");
/// assert_eq!(binder.name(), "row");
/// assert_eq!(&src[binder.span()], "row");
/// ```
#[derive(Debug, Clone, PartialEq)]
pub struct PrimaryKeyBinder {
    name: String,
    span: Span,
}

impl PrimaryKeyBinder {
    /// Name written for this binder.
    #[must_use]
    pub fn name(&self) -> &str { &self.name }

    /// Precise source span for the binder name.
    ///
    /// # Examples
    ///
    /// ```
    /// # use ddlint::parse;
    /// #
    /// # let src = "input relation R(x: u32) primary key (x)";
    /// # let parsed = parse(src);
    /// # let relation = parsed
    /// #     .root()
    /// #     .relations()
    /// #     .into_iter()
    /// #     .next()
    /// #     .expect("relation");
    /// # let key = relation
    /// #     .primary_key()
    /// #     .expect("valid key")
    /// #     .expect("key present");
    /// # let binder = key.binders().first().expect("binder");
    /// assert_eq!(&src[binder.span()], "x");
    /// ```
    #[must_use]
    pub fn span(&self) -> Span { self.span.clone() }
}

/// Source text of the expression that follows a primary-key binder list.
///
/// The text is bounded by non-trivia tokens, so surrounding whitespace and
/// trailing comments are excluded while interior trivia is kept verbatim.
///
/// # Examples
///
/// ```
/// # use ddlint::parse;
/// #
/// # let parsed = parse("input relation R(row: Row) primary key (row) (row.a, row.b)");
/// # let relation = parsed
/// #     .root()
/// #     .relations()
/// #     .into_iter()
/// #     .next()
/// #     .expect("relation");
/// # let key = relation
/// #     .primary_key()
/// #     .expect("valid key")
/// #     .expect("key present");
/// # let expression = key.expression().expect("expression");
/// assert_eq!(expression.text(), "(row.a, row.b)");
/// ```
#[derive(Debug, Clone, PartialEq)]
pub struct PrimaryKeyExpression {
    text: String,
    span: Span,
}

impl PrimaryKeyExpression {
    /// Expression source text.
    #[must_use]
    pub fn text(&self) -> &str { &self.text }

    /// Source span covering [`Self::text`].
    ///
    /// # Examples
    ///
    /// ```
    /// # use ddlint::parse;
    /// #
    /// # let src = "input relation R(row: Row) primary key (row) row.id";
    /// # let parsed = parse(src);
    /// # let relation = parsed
    /// #     .root()
    /// #     .relations()
    /// #     .into_iter()
    /// #     .next()
    /// #     .expect("relation");
    /// # let key = relation
    /// #     .primary_key()
    /// #     .expect("valid key")
    /// #     .expect("key present");
    /// # let expression = key.expression().expect("expression");
    /// assert_eq!(&src[expression.span()], expression.text());
    /// ```
    #[must_use]
    pub fn span(&self) -> Span { self.span.clone() }

    /// Parse the expression text into an [`Expr`].
    ///
    /// Error spans are reported in source-absolute coordinates.
    ///
    /// # Errors
    ///
    /// Returns the expression parser's diagnostics when the text is not a
    /// well-formed `DDlog` expression.
    ///
    /// # Examples
    ///
    /// ```
    /// # use ddlint::parse;
    /// #
    /// # let parsed = parse("input relation R(row: Row) primary key (row) (row.a, row.b)");
    /// # let relation = parsed
    /// #     .root()
    /// #     .relations()
    /// #     .into_iter()
    /// #     .next()
    /// #     .expect("relation");
    /// # let key = relation
    /// #     .primary_key()
    /// #     .expect("valid key")
    /// #     .expect("key present");
    /// # let expression = key.expression().expect("expression");
    /// let sexpr = expression.parse().expect("valid expression").to_sexpr();
    /// assert_eq!(sexpr, "(tuple (field row a) (field row b))");
    /// ```
    pub fn parse(&self) -> Result<Expr, Vec<Simple<SyntaxKind>>> {
        parse_expression(&self.text).map_err(|errors| shift_errors(errors, self.span.start))
    }
}

impl Relation {
    /// Primary-key clause if specified.
    ///
    /// `Ok(None)` means no primary-key clause is present, or that the body is
    /// the bracket form, which cannot carry one. For a keyed input relation the
    /// returned [`PrimaryKey`] carries the binders and the optional
    /// spec-form expression.
    ///
    /// # Errors
    ///
    /// Returns [`RelationParseErrors`] when a `primary key` clause is present
    /// but its binder list cannot be interpreted, or when the relation body is
    /// malformed or missing so the columns cannot be skipped.
    ///
    /// # Examples
    ///
    /// ```
    /// # use ddlint::parse;
    /// #
    /// # let parsed =
    /// #     parse("input relation Book(row: BookRow) primary key (row) (row.author, row.title)");
    /// # let relation = parsed
    /// #     .root()
    /// #     .relations()
    /// #     .into_iter()
    /// #     .next()
    /// #     .expect("relation");
    /// let key = relation
    ///     .primary_key()
    ///     .expect("valid key")
    ///     .expect("key present");
    /// assert_eq!(key.binder_names(), ["row"]);
    /// assert_eq!(
    ///     key.expression().map(|expr| expr.text()),
    ///     Some("(row.author, row.title)")
    /// );
    /// ```
    pub fn primary_key(&self) -> Result<Option<PrimaryKey>, RelationParseErrors> {
        clause::clause_of(&self.syntax)
    }
}

#[cfg(test)]
mod tests;
