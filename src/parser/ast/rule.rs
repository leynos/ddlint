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
use super::{AstNode, BinaryOp, Expr, Pattern};
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

/// Validate that at most one aggregation appears in the rule body.
///
/// If `term` is an aggregation, checks whether a previous aggregation was already
/// seen. Records an error and returns `true` (skip) if so, otherwise stores the
/// current span as the first and returns `false` (include).
fn validate_aggregation(
    term: &RuleBodyTerm,
    literal_span: &Span,
    first_aggregation_span: &mut Option<Span>,
    errors: &mut Vec<Simple<SyntaxKind>>,
) -> bool {
    if let RuleBodyTerm::Aggregation(_) = term {
        match first_aggregation_span {
            Some(first_span) => {
                errors.push(multiple_aggregations_error(first_span, literal_span));
                return true; // skip duplicate aggregation
            }
            None => {
                *first_aggregation_span = Some(literal_span.clone());
            }
        }
    }
    false
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

        if let Some(parts) = split_assignment(&raw) {
            match parse_assignment(&parts, &literal_span) {
                Ok(term) => return term,
                Err(mut errs) => {
                    errors.append(&mut errs);
                    return None;
                }
            }
        }

        match parse_expression(trimmed) {
            Ok(expr) => {
                let mut ctx = ClassificationContext {
                    literal_span: &literal_span,
                    first_aggregation_span,
                    errors,
                };
                classify_expression(expr, &mut ctx)
            }
            Err(mut errs) => {
                errors.append(&mut errs);
                None
            }
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

/// Context for classifying rule body expressions with aggregation tracking.
struct ClassificationContext<'a> {
    literal_span: &'a Span,
    first_aggregation_span: &'a mut Option<Span>,
    errors: &'a mut Vec<Simple<SyntaxKind>>,
}

fn classify_expression(expr: Expr, ctx: &mut ClassificationContext<'_>) -> Option<RuleBodyTerm> {
    match expr {
        Expr::ForLoop {
            pattern,
            iterable,
            guard,
            body,
        } => Some(classify_for_loop(
            pattern,
            *iterable,
            guard.map(|g| *g),
            *body,
            ctx,
        )),
        Expr::Call { callee, args } => {
            if let Expr::Variable(name) = &*callee
                && let Some(source) = aggregation_source_for(name.as_str())
            {
                return classify_aggregation_with_tracking(args, source, ctx);
            }
            Some(RuleBodyTerm::Expression(Expr::Call { callee, args }))
        }
        other => Some(RuleBodyTerm::Expression(other)),
    }
}

fn aggregation_source_for(name: &str) -> Option<AggregationSource> {
    match name {
        "group_by" => Some(AggregationSource::GroupBy),
        "Aggregate" => Some(AggregationSource::LegacyAggregate),
        _ => None,
    }
}

/// Classify an aggregation with tracking to enforce at-most-one-per-rule-body.
fn classify_aggregation_with_tracking(
    args: Vec<Expr>,
    source: AggregationSource,
    ctx: &mut ClassificationContext<'_>,
) -> Option<RuleBodyTerm> {
    if args.len() != 2 {
        ctx.errors
            .extend(aggregation_arity_error(ctx.literal_span, source));
        return None;
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

    let term = RuleBodyTerm::Aggregation(RuleAggregation {
        project,
        key,
        source,
    });

    // Track and validate aggregation
    if validate_aggregation(
        &term,
        ctx.literal_span,
        ctx.first_aggregation_span,
        ctx.errors,
    ) {
        None // skip duplicate aggregation
    } else {
        Some(term)
    }
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

/// Classify a for-loop expression into a structured `RuleForLoop` term.
///
/// Recursively classifies the body expression to produce nested body terms.
/// Aggregation validation is delegated to `classify_for_body_with_aggregation_tracking`.
fn classify_for_loop(
    pattern: Pattern,
    iterable: Expr,
    guard: Option<Expr>,
    body: Expr,
    ctx: &mut ClassificationContext<'_>,
) -> RuleBodyTerm {
    let body_terms = classify_for_body_with_aggregation_tracking(body, ctx);

    RuleBodyTerm::ForLoop(RuleForLoop {
        pattern,
        iterable,
        guard,
        body_terms,
    })
}

/// Recursively classify a for-loop body into a sequence of body terms with aggregation tracking.
///
/// Handles sequence expressions (block bodies with multiple statements),
/// nested for-loops, and single expressions. Aggregations are tracked to enforce the
/// "at most one aggregation per rule body" rule.
fn classify_for_body_with_aggregation_tracking(
    body: Expr,
    ctx: &mut ClassificationContext<'_>,
) -> Vec<RuleBodyTerm> {
    match body {
        // Handle sequence expressions from block bodies
        Expr::Binary {
            op: BinaryOp::Seq,
            lhs,
            rhs,
        } => {
            let mut terms = classify_for_body_with_aggregation_tracking(*lhs, ctx);
            terms.extend(classify_for_body_with_aggregation_tracking(*rhs, ctx));
            terms
        }
        // Handle nested for-loops
        Expr::ForLoop {
            pattern,
            iterable,
            guard,
            body,
        } => {
            vec![classify_for_loop(
                pattern,
                *iterable,
                guard.map(|g| *g),
                *body,
                ctx,
            )]
        }
        // Unwrap groups (parentheses/braces) to handle `(a; b)` bodies
        Expr::Group(inner) => classify_for_body_with_aggregation_tracking(*inner, ctx),
        // Single expression - classify it directly
        other => classify_expression(other, ctx).into_iter().collect(),
    }
}

fn multiple_aggregations_error(_first_span: &Span, second_span: &Span) -> Simple<SyntaxKind> {
    Simple::custom(
        second_span.clone(),
        "at most one aggregation (group_by or Aggregate) is permitted per rule body".to_string(),
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
