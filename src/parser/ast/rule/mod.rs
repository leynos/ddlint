//!
//! AST wrapper for rule declarations.
//!
//! The [`Rule`] type is a lightweight view over a rule declaration in the
//! syntax tree. It exposes helpers for reading the rule head and for iterating
//! over the comma-separated body literals. This allows analyses to reason about
//! the dependencies between relations without building a full semantic model.
//!
//! Rules are discovered via the [`Root`](super::root::Root) wrapper and then
//! inspected using these methods.
//!
//! # Examples
//!
//! ```rust,ignore
//! # use ddlint::parse;
//! # use ddlint::parser::ast::Rule;
//! # fn first_rule(src: &str) -> Rule {
//! #     parse(src)
//! #         .root()
//! #         .rules()
//! #         .into_iter()
//! #         .next()
//! #         .expect("rule missing")
//! # }
//! let rule = first_rule("R(x) :- S(x), T(x).");
//! assert_eq!(rule.head().as_deref(), Some("R(x)"));
//! assert_eq!(rule.body_literals(), vec!["S(x)".into(), "T(x)".into()]);
//! ```

use chumsky::error::Simple;

mod classification;

use super::rule_head::{RuleHead, first_head_text, parse_rule_heads};
use super::{AstNode, Expr, Pattern};
use crate::parser::delimiter::find_top_level_eq_span;
use crate::parser::span_utils::trim_byte_range;
use crate::{DdlogLanguage, Span, SyntaxKind};

/// Typed wrapper for a rule declaration.
#[derive(Debug, Clone)]
pub struct Rule {
    pub(crate) syntax: rowan::SyntaxNode<DdlogLanguage>,
}

impl Rule {
    /// Text of the first rule head atom.
    #[must_use]
    pub fn head(&self) -> Option<String> {
        first_head_text(&self.syntax)
    }

    /// Parse the rule heads into structured expressions.
    ///
    /// # Errors
    /// Returns aggregated diagnostics when a head or location fails to parse.
    pub fn heads(&self) -> Result<Vec<RuleHead>, Vec<Simple<SyntaxKind>>> {
        let span = text_range_to_span(self.syntax.text_range());
        parse_rule_heads(&self.syntax.text().to_string(), span.start)
    }

    /// Return the CST nodes representing the rule body expressions.
    #[must_use]
    pub fn body_expression_nodes(&self) -> Vec<RuleBodyExpression> {
        self.syntax
            .children()
            .filter(|n| n.kind() == SyntaxKind::N_EXPR_NODE)
            .map(|syntax| RuleBodyExpression { syntax })
            .collect()
    }

    /// Text of each body literal in order of appearance.
    #[must_use]
    pub fn body_literals(&self) -> Vec<String> {
        self.body_expression_nodes()
            .into_iter()
            .map(|expr| expr.text().trim().to_string())
            .collect()
    }

    /// Parse the body literals into structured expressions.
    ///
    /// # Errors
    /// Returns aggregated expression parser diagnostics if any literal fails to parse.
    pub fn body_expressions(&self) -> Result<Vec<Expr>, Vec<Simple<SyntaxKind>>> {
        let terms = self.body_terms()?;
        let exprs = terms
            .into_iter()
            .filter_map(|term| match term {
                RuleBodyTerm::Expression(expr) => Some(expr),
                _ => None,
            })
            .collect();
        Ok(exprs)
    }

    /// Parse the rule body into semantically distinct terms.
    ///
    /// This helper recognises aggregations (`group_by`/`Aggregate`) and
    /// pattern-matching assignments (FlatMap-style binds), returning
    /// [`RuleBodyTerm`] variants describing each literal.
    ///
    /// This is the current aggregation extraction boundary. The base
    /// [`parse`](crate::parse) pipeline preserves raw rule-body literals in the
    /// CST and does not surface aggregation misuse through `Parsed::errors()`;
    /// callers must request body terms explicitly to trigger aggregation
    /// classification and validation.
    ///
    /// At most one aggregation is permitted per rule body; multiple
    /// aggregations produce a diagnostic.
    ///
    /// # Examples
    /// ```rust
    /// # use ddlint::parse;
    /// # use ddlint::parser::ast::{AggregationSource, RuleBodyTerm};
    /// let parsed = parse("Totals(u, total) :- Orders(u, amt), group_by(sum(amt), u).");
    /// let rule = parsed.root().rules().first().cloned().expect("rule missing");
    /// let terms = rule.body_terms().expect("body terms should parse");
    /// assert!(matches!(
    ///     terms.get(1),
    ///     Some(RuleBodyTerm::Aggregation(agg)) if agg.source == AggregationSource::GroupBy
    /// ));
    /// ```
    ///
    /// # Errors
    /// Returns aggregated literal diagnostics when assignments or aggregations
    /// fail to parse, or when multiple aggregations appear in the same rule
    /// body.
    pub fn body_terms(&self) -> Result<Vec<RuleBodyTerm>, Vec<Simple<SyntaxKind>>> {
        let mut terms = Vec::new();
        let mut errors = Vec::new();
        let mut first_aggregation_span: Option<Span> = None;

        for literal in self.body_expression_nodes() {
            if let Some(term) = literal.parse_term(&mut first_aggregation_span, &mut errors) {
                terms.push(term);
            }
        }

        if errors.is_empty() {
            Ok(terms)
        } else {
            Err(errors)
        }
    }

    /// Return body terms with for-loops flattened inline.
    ///
    /// This method provides the desugared form of the rule body, expanding
    /// each for-loop into its constituent terms (iterable, guard, body).
    ///
    /// # Example
    ///
    /// ```rust
    /// # use ddlint::parse;
    /// # use ddlint::parser::ast::RuleBodyTerm;
    /// let parsed = parse("R(x) :- for (x in Source(x) if pred(x)) Target(x).");
    /// let rule = parsed.root().rules().first().cloned().expect("rule missing");
    /// let flattened = rule.flattened_body_terms().expect("should flatten");
    /// // Contains: Source(x), pred(x), Target(x)
    /// assert_eq!(flattened.len(), 3);
    /// assert!(matches!(flattened.first(), Some(RuleBodyTerm::Expression(_))));
    /// ```
    ///
    /// # Errors
    /// Returns aggregated literal diagnostics when any body term fails to parse.
    pub fn flattened_body_terms(&self) -> Result<Vec<RuleBodyTerm>, Vec<Simple<SyntaxKind>>> {
        let terms = self.body_terms()?;
        let mut result = Vec::new();

        for term in terms {
            match term {
                RuleBodyTerm::ForLoop(for_loop) => {
                    result.extend(for_loop.flatten());
                }
                other => {
                    result.push(other);
                }
            }
        }

        Ok(result)
    }
}

impl_ast_node!(Rule);

/// Wrapper for a single rule body expression CST node.
#[derive(Debug, Clone)]
pub struct RuleBodyExpression {
    syntax: rowan::SyntaxNode<DdlogLanguage>,
}

impl RuleBodyExpression {
    /// Full text of the expression as written in the source.
    #[must_use]
    pub fn text(&self) -> String {
        self.syntax.text().to_string()
    }

    /// Source span covering this body expression.
    #[must_use]
    pub fn span(&self) -> Span {
        text_range_to_span(self.syntax.text_range())
    }

    fn parse_term(
        &self,
        first_aggregation_span: &mut Option<Span>,
        errors: &mut Vec<Simple<SyntaxKind>>,
    ) -> Option<RuleBodyTerm> {
        let raw = self.text();
        let trimmed = raw.trim();
        if trimmed.is_empty() {
            return None;
        }

        let literal_span = self.span();

        classification::parse_rule_body_term(&raw, literal_span, first_aggregation_span, errors)
    }
}

impl_ast_node!(RuleBodyExpression);

/// Structured literal produced when walking a rule body.
#[derive(Debug, Clone, PartialEq)]
pub enum RuleBodyTerm {
    /// Regular expression literal (atoms, conditions, control-flow, etc.).
    Expression(Expr),
    /// Pattern-matching assignment such as `var ip = FlatMap(extract_ips(ips))`.
    Assignment(RuleAssignment),
    /// Aggregation literal (`group_by` or legacy `Aggregate`).
    Aggregation(RuleAggregation),
    /// For-loop construct that iterates over a relation or expression.
    ForLoop(RuleForLoop),
}

/// For-loop term extracted from a rule body.
///
/// Represents a top-level `for` statement in a rule context, which desugars
/// into equivalent rule terms. The pattern binding becomes available in
/// subsequent terms, the guard (if present) acts as a filter condition, and
/// the body terms are executed for each matching iteration.
///
/// # Desugaring Semantics
///
/// ```text
/// Head(x) :- for (item in Source(item) if guard(item)) Body(item, x).
/// ```
///
/// Flattens to:
///
/// ```text
/// Head(x) :- Source(item), guard(item), Body(item, x).
/// ```
#[derive(Debug, Clone, PartialEq)]
pub struct RuleForLoop {
    /// Pattern binding for each iteration (e.g., `item` or `(key, value)`).
    pub pattern: Pattern,
    /// Iterable expression (often a relation call like `Items(item)`).
    pub iterable: Expr,
    /// Optional guard condition filtering iterations.
    pub guard: Option<Expr>,
    /// Body terms executed for each matching element.
    pub body_terms: Vec<RuleBodyTerm>,
}

impl RuleForLoop {
    /// Flatten this for-loop into a sequence of body terms.
    ///
    /// The iterable becomes a relation call/expression term, the guard becomes
    /// a condition term, and body terms are appended. Nested for-loops are
    /// recursively flattened.
    ///
    /// # Example
    ///
    /// ```text
    /// for (item in Source(item) if guard(item)) Body(item)
    /// ```
    ///
    /// Flattens to: `[Source(item), guard(item), Body(item)]`
    #[must_use]
    pub fn flatten(&self) -> Vec<RuleBodyTerm> {
        let mut result = Vec::new();

        // The iterable becomes a relation call/expression term
        result.push(RuleBodyTerm::Expression(self.iterable.clone()));

        // The guard becomes a condition term
        if let Some(guard) = &self.guard {
            result.push(RuleBodyTerm::Expression(guard.clone()));
        }

        // Flatten nested for-loops and add body terms
        for term in &self.body_terms {
            match term {
                RuleBodyTerm::ForLoop(inner) => {
                    result.extend(inner.flatten());
                }
                other => {
                    result.push(other.clone());
                }
            }
        }

        result
    }
}

/// Pattern assignment extracted from a rule literal.
#[derive(Debug, Clone, PartialEq)]
pub struct RuleAssignment {
    /// Left-hand pattern (e.g., `var ip` or `(key, value)`).
    pub pattern: Pattern,
    /// Expression on the right-hand side.
    pub value: Expr,
}

/// Indicates which surface form produced an aggregation literal.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AggregationSource {
    /// Canonical `group_by(project, key)` invocation.
    GroupBy,
    /// Legacy `Aggregate((key), accumulator)` form.
    LegacyAggregate,
}

impl AggregationSource {
    fn label(self) -> &'static str {
        match self {
            Self::GroupBy => "group_by",
            Self::LegacyAggregate => "Aggregate",
        }
    }
}

/// Aggregation literal extracted from the rule body.
#[derive(Debug, Clone, PartialEq)]
pub struct RuleAggregation {
    /// Projection expression (`sum(amt)` in `group_by(sum(amt), key)`).
    pub project: Expr,
    /// Grouping key (`key` in `group_by(sum(amt), key)`).
    pub key: Expr,
    /// Origin of the aggregation syntax.
    pub source: AggregationSource,
}

#[derive(Debug)]
pub(crate) struct AssignmentParts {
    pub(crate) pattern: String,
    pub(crate) value: String,
    pub(crate) pattern_offset: usize,
    pub(crate) value_offset: usize,
}

/// Split a literal on a single top-level `=` into pattern/value parts.
///
/// Only `=` tokens at delimiter depth zero are treated as assignments; equality
/// tests must use `==` (`T_EQEQ`) or occur inside delimiters.
pub(crate) fn split_assignment(raw: &str) -> Option<AssignmentParts> {
    let eq_span = find_top_level_eq_span(raw)?;
    let lhs_full = raw.get(..eq_span.start).unwrap_or("");
    let (lhs_start, lhs_end) = trim_byte_range(lhs_full);
    let pattern = lhs_full.get(lhs_start..lhs_end).unwrap_or("").to_string();

    let rhs_full = raw.get(eq_span.end..).unwrap_or("");
    let (rhs_start, rhs_end) = trim_byte_range(rhs_full);
    let value = rhs_full.get(rhs_start..rhs_end).unwrap_or("").to_string();

    Some(AssignmentParts {
        pattern,
        value,
        pattern_offset: lhs_start,
        value_offset: eq_span.end.saturating_add(rhs_start),
    })
}

/// Convert a Rowan text range to a byte-offset span.
///
/// This helper bridges the CST layer (which uses `rowan::TextRange`) and the
/// parser/diagnostic layer (which uses byte-offset `Span`s).
pub(crate) fn text_range_to_span(range: rowan::TextRange) -> Span {
    let start: usize = range.start().into();
    let end: usize = range.end().into();
    start..end
}

#[cfg(test)]
mod tests;
