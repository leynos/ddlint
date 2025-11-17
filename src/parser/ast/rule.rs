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

use super::{AstNode, Expr};
use crate::parser::expression::parse_expression;
use crate::{DdlogLanguage, Span, SyntaxKind, tokenize_without_trivia};

/// Typed wrapper for a rule declaration.
#[derive(Debug, Clone)]
pub struct Rule {
    pub(crate) syntax: rowan::SyntaxNode<DdlogLanguage>,
}

impl Rule {
    /// Text of the rule head atom.
    #[must_use]
    pub fn head(&self) -> Option<String> {
        use rowan::NodeOrToken;

        let mut buf = String::new();
        for e in self.syntax.children_with_tokens() {
            match e {
                NodeOrToken::Token(t) => match t.kind() {
                    SyntaxKind::T_IMPLIES | SyntaxKind::T_DOT => break,
                    _ => buf.push_str(t.text()),
                },
                NodeOrToken::Node(n) => buf.push_str(&n.text().to_string()),
            }
        }

        let text = buf.trim();
        if text.is_empty() {
            None
        } else {
            Some(text.to_string())
        }
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
        let mut exprs = Vec::new();
        let mut errors = Vec::new();

        for literal in self.body_expression_nodes() {
            let text = literal.text();
            let trimmed = text.trim();
            if trimmed.is_empty() {
                continue;
            }
            match parse_expression(trimmed) {
                Ok(expr) => exprs.push(expr),
                Err(mut errs) => errors.append(&mut errs),
            }
        }

        if errors.is_empty() {
            Ok(exprs)
        } else {
            Err(errors)
        }
    }

    /// Parse the rule body into semantically distinct terms.
    ///
    /// This helper recognises aggregations (`group_by`/`Aggregate`) and
    /// pattern-matching assignments (FlatMap-style binds), returning
    /// [`RuleBodyTerm`] variants describing each literal.
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
    /// fail to parse.
    pub fn body_terms(&self) -> Result<Vec<RuleBodyTerm>, Vec<Simple<SyntaxKind>>> {
        let mut terms = Vec::new();
        let mut errors = Vec::new();

        for literal in self.body_expression_nodes() {
            match literal.parse_term() {
                Ok(Some(term)) => terms.push(term),
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
            return parse_assignment(parts, &literal_span);
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
///
/// The `pattern` field stores the left-hand side exactly as written (after
/// trimming), so later stages can re-parse it once a full pattern parser lands.
#[derive(Debug, Clone, PartialEq)]
pub struct RuleAssignment {
    /// Left-hand pattern (e.g., `var ip` or `(key, value)`).
    pub pattern: String,
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
    parts: AssignmentParts,
    literal_span: &Span,
) -> Result<Option<RuleBodyTerm>, Vec<Simple<SyntaxKind>>> {
    if parts.pattern.is_empty() {
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

    match parse_expression(&parts.value) {
        Ok(expr) => Ok(Some(RuleBodyTerm::Assignment(RuleAssignment {
            pattern: parts.pattern,
            value: expr,
        }))),
        Err(errs) => Err(errs),
    }
}

fn classify_expression(
    expr: Expr,
    literal_span: &Span,
) -> Result<Option<RuleBodyTerm>, Vec<Simple<SyntaxKind>>> {
    match expr {
        Expr::Call { callee, args } => {
            if let Expr::Variable(name) = &*callee {
                match name.as_str() {
                    "group_by" => {
                        return build_aggregation(args, literal_span, AggregationSource::GroupBy);
                    }
                    "Aggregate" => {
                        return build_aggregation(
                            args,
                            literal_span,
                            AggregationSource::LegacyAggregate,
                        );
                    }
                    _ => {}
                }
            }
            Ok(Some(RuleBodyTerm::Expression(Expr::Call { callee, args })))
        }
        other => Ok(Some(RuleBodyTerm::Expression(other))),
    }
}

fn build_aggregation(
    args: Vec<Expr>,
    literal_span: &Span,
    source: AggregationSource,
) -> Result<Option<RuleBodyTerm>, Vec<Simple<SyntaxKind>>> {
    let label = match source {
        AggregationSource::GroupBy => "group_by",
        AggregationSource::LegacyAggregate => "Aggregate",
    };
    if args.len() != 2 {
        return Err(aggregation_arity_error(literal_span, label));
    }

    let mut iter = args.into_iter();
    let first = iter
        .next()
        .ok_or_else(|| aggregation_arity_error(literal_span, label))?;
    let second = iter
        .next()
        .ok_or_else(|| aggregation_arity_error(literal_span, label))?;
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

fn aggregation_arity_error(literal_span: &Span, label: &str) -> Vec<Simple<SyntaxKind>> {
    vec![Simple::custom(
        literal_span.clone(),
        format!("{label} expects exactly two arguments"),
    )]
}

#[derive(Debug)]
struct AssignmentParts {
    pattern: String,
    value: String,
}

fn split_assignment(raw: &str) -> Option<AssignmentParts> {
    let mut depths = DelimiterDepths::default();
    for (kind, span) in tokenize_without_trivia(raw) {
        depths.apply(kind);
        if depths.is_top_level() && kind == SyntaxKind::T_EQ {
            let pattern = raw.get(..span.start).unwrap_or("").trim().to_string();
            let value = raw.get(span.end..).unwrap_or("").trim().to_string();
            return Some(AssignmentParts { pattern, value });
        }
    }
    None
}

#[derive(Default)]
struct DelimiterDepths {
    paren: usize,
    brace: usize,
    bracket: usize,
}

impl DelimiterDepths {
    fn apply(&mut self, kind: SyntaxKind) {
        match kind {
            SyntaxKind::T_LPAREN => self.paren += 1,
            SyntaxKind::T_RPAREN => self.paren = self.paren.saturating_sub(1),
            SyntaxKind::T_LBRACE => self.brace += 1,
            SyntaxKind::T_RBRACE => self.brace = self.brace.saturating_sub(1),
            SyntaxKind::T_LBRACKET => self.bracket += 1,
            SyntaxKind::T_RBRACKET => self.bracket = self.bracket.saturating_sub(1),
            _ => {}
        }
    }

    fn is_top_level(&self) -> bool {
        self.paren == 0 && self.brace == 0 && self.bracket == 0
    }
}

fn text_range_to_span(range: rowan::TextRange) -> Span {
    let start: usize = range.start().into();
    let end: usize = range.end().into();
    start..end
}

#[cfg(test)]
mod tests {

    use super::Expr;
    use crate::parse;

    #[expect(clippy::expect_used, reason = "Using expect for clearer test failures")]
    #[test]
    fn rule_head_and_body() {
        let parsed = parse("A(x) :- B(x).");
        let rule = parsed
            .root()
            .rules()
            .first()
            .cloned()
            .expect("rule missing");
        assert_eq!(rule.head().as_deref(), Some("A(x)"));
        assert_eq!(rule.body_literals(), vec!["B(x)".to_string()]);
    }

    #[expect(clippy::expect_used, reason = "test requires parsed expressions")]
    #[test]
    fn body_expressions_parse_control_flow() {
        let src = "R(x) :- for (item in Items(item)) Process(item), if (ready(x)) { Accept(x) } else { Skip() }.";
        let parsed = parse(src);
        crate::test_util::assert_no_parse_errors(parsed.errors());
        let rule = parsed
            .root()
            .rules()
            .first()
            .cloned()
            .expect("rule missing");
        let exprs = rule.body_expressions().expect("expressions should parse");
        assert_eq!(exprs.len(), 2);
        assert!(matches!(exprs.first(), Some(Expr::ForLoop { .. })));
        assert!(matches!(exprs.get(1), Some(Expr::IfElse { .. })));
    }
}
