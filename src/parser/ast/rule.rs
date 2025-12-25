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

use super::rule_head::{RuleHead, first_head_text, parse_rule_heads};
use super::{AstNode, Expr, Pattern};
use crate::parser::delimiter::find_top_level_eq_span;
use crate::parser::expression::parse_expression;
use crate::parser::pattern::parse_pattern;
use crate::parser::span_utils::{shift_errors, trim_byte_range};
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
            let literal_span = literal.span();
            match literal.parse_term() {
                Ok(Some(term)) => {
                    validate_aggregation(
                        &term,
                        &literal_span,
                        &mut first_aggregation_span,
                        &mut errors,
                    );
                    terms.push(term);
                }
                Ok(None) => {}
                Err(mut errs) => errors.append(&mut errs),
            }
        }

        if errors.is_empty() {
            Ok(terms)
        } else {
            Err(errors)
        }
    }
}

/// Validate that at most one aggregation appears in the rule body.
///
/// If `term` is an aggregation, checks whether a previous aggregation was already
/// seen. Records an error if so, otherwise stores the current span as the first.
fn validate_aggregation(
    term: &RuleBodyTerm,
    literal_span: &Span,
    first_aggregation_span: &mut Option<Span>,
    errors: &mut Vec<Simple<SyntaxKind>>,
) {
    if let RuleBodyTerm::Aggregation(_) = term {
        match first_aggregation_span {
            Some(first_span) => {
                errors.push(multiple_aggregations_error(first_span, literal_span));
            }
            None => {
                *first_aggregation_span = Some(literal_span.clone());
            }
        }
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

    fn span(&self) -> Span {
        text_range_to_span(self.syntax.text_range())
    }

    fn parse_term(&self) -> Result<Option<RuleBodyTerm>, Vec<Simple<SyntaxKind>>> {
        let raw = self.text();
        let trimmed = raw.trim();
        if trimmed.is_empty() {
            return Ok(None);
        }

        let literal_span = self.span();

        if let Some(parts) = split_assignment(&raw) {
            return parse_assignment(&parts, &literal_span);
        }

        match parse_expression(trimmed) {
            Ok(expr) => classify_expression(expr, &literal_span),
            Err(errs) => Err(errs),
        }
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

fn parse_assignment(
    parts: &AssignmentParts,
    literal_span: &Span,
) -> Result<Option<RuleBodyTerm>, Vec<Simple<SyntaxKind>>> {
    if parts.pattern.trim().is_empty() {
        return Err(vec![Simple::custom(
            literal_span.clone(),
            "expected pattern before '=' in rule literal",
        )]);
    }

    if parts.value.is_empty() {
        return Err(vec![Simple::custom(
            literal_span.clone(),
            "expected expression after '=' in rule literal",
        )]);
    }

    let pattern_base_offset = literal_span.start.saturating_add(parts.pattern_offset);
    let pattern = match parse_pattern(&parts.pattern) {
        Ok(pattern) => pattern,
        Err(errs) => return Err(shift_errors(errs, pattern_base_offset)),
    };

    let value_base_offset = literal_span.start.saturating_add(parts.value_offset);
    match parse_expression(&parts.value) {
        Ok(expr) => Ok(Some(RuleBodyTerm::Assignment(RuleAssignment {
            pattern,
            value: expr,
        }))),
        Err(errs) => Err(shift_errors(errs, value_base_offset)),
    }
}

fn classify_expression(
    expr: Expr,
    literal_span: &Span,
) -> Result<Option<RuleBodyTerm>, Vec<Simple<SyntaxKind>>> {
    match expr {
        Expr::Call { callee, args } => {
            if let Expr::Variable(name) = &*callee
                && let Some(source) = aggregation_source_for(name.as_str())
            {
                return classify_aggregation(args, literal_span, source);
            }
            Ok(Some(RuleBodyTerm::Expression(Expr::Call { callee, args })))
        }
        other => Ok(Some(RuleBodyTerm::Expression(other))),
    }
}

fn aggregation_source_for(name: &str) -> Option<AggregationSource> {
    match name {
        "group_by" => Some(AggregationSource::GroupBy),
        "Aggregate" => Some(AggregationSource::LegacyAggregate),
        _ => None,
    }
}

fn classify_aggregation(
    args: Vec<Expr>,
    literal_span: &Span,
    source: AggregationSource,
) -> Result<Option<RuleBodyTerm>, Vec<Simple<SyntaxKind>>> {
    if args.len() != 2 {
        return Err(aggregation_arity_error(literal_span, source));
    }

    let mut iter = args.into_iter();
    let first = iter
        .next()
        .unwrap_or_else(|| unreachable!("len pre-checked as 2; first arg missing"));
    let second = iter
        .next()
        .unwrap_or_else(|| unreachable!("len pre-checked as 2; second arg missing"));
    let (project, key) = match source {
        AggregationSource::GroupBy => (first, second),
        AggregationSource::LegacyAggregate => (second, first),
    };

    Ok(Some(RuleBodyTerm::Aggregation(RuleAggregation {
        project,
        key,
        source,
    })))
}

fn aggregation_arity_error(
    literal_span: &Span,
    source: AggregationSource,
) -> Vec<Simple<SyntaxKind>> {
    vec![Simple::custom(
        literal_span.clone(),
        format!("{} expects exactly two arguments", source.label()),
    )]
}

fn multiple_aggregations_error(first_span: &Span, second_span: &Span) -> Simple<SyntaxKind> {
    Simple::custom(
        second_span.clone(),
        format!(
            "at most one aggregation (group_by or Aggregate) is permitted per rule; \
             first aggregation at {}..{}",
            first_span.start, first_span.end
        ),
    )
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

fn text_range_to_span(range: rowan::TextRange) -> Span {
    let start: usize = range.start().into();
    let end: usize = range.end().into();
    start..end
}

#[cfg(test)]
mod tests;
