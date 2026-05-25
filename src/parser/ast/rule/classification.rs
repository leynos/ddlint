//! Rule-body term classification helpers for [`super::Rule`].
//!
//! This module keeps the raw-literal classification pipeline separate from the
//! main AST wrapper types so `rule.rs` stays focused on the public surface.

use chumsky::error::Simple;

use super::{
    AggregationSource, AssignmentParts, RuleAggregation, RuleAssignment, RuleBodyTerm, RuleForLoop,
    split_assignment,
};
use crate::parser::ast::{BinaryOp, Expr, Pattern};
use crate::parser::expression::parse_expression;
use crate::parser::pattern::parse_pattern;
use crate::parser::span_utils::shift_errors;
use crate::{Span, SyntaxKind};

/// Classify a raw rule-body literal into the structured rule-body term used by
/// semantic consumers.
///
/// The caller provides the literal span so diagnostics can point back to the
/// CST node that supplied the raw text. `first_aggregation_span` is shared
/// across the surrounding rule body, allowing this helper to reject multiple
/// aggregations while still parsing each literal independently.
pub(super) fn parse_rule_body_term(
    raw: &str,
    literal_span: Span,
    first_aggregation_span: &mut Option<Span>,
    errors: &mut Vec<Simple<SyntaxKind>>,
) -> Option<RuleBodyTerm> {
    if let Some(parts) = split_assignment(raw) {
        match parse_assignment(&parts, &literal_span) {
            Ok(term) => return term,
            Err(mut errs) => {
                errors.append(&mut errs);
                return None;
            }
        }
    }

    match parse_expression(raw.trim()) {
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

    if parts.value.trim().is_empty() {
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
            ForLoopComponents {
                pattern,
                iterable: *iterable,
                guard: guard.map(|guard| *guard),
                body: *body,
            },
            ctx,
        )),
        Expr::Apply { callee, args } => {
            if let Some(source) = invocation_aggregation_source(&callee) {
                return classify_aggregation_with_tracking(args, source, ctx);
            }
            Some(RuleBodyTerm::Expression(Expr::Apply { callee, args }))
        }
        Expr::Call { callee, args } => {
            if let Some(source) = invocation_aggregation_source(&callee) {
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

fn invocation_aggregation_source(callee: &Expr) -> Option<AggregationSource> {
    if let Expr::Variable(name) = callee {
        aggregation_source_for(name.as_str())
    } else {
        None
    }
}

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

    if validate_aggregation(
        &term,
        ctx.literal_span,
        ctx.first_aggregation_span,
        ctx.errors,
    ) {
        None
    } else {
        Some(term)
    }
}

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
                return true;
            }
            None => {
                *first_aggregation_span = Some(literal_span.clone());
            }
        }
    }
    false
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

struct ForLoopComponents {
    pattern: Pattern,
    iterable: Expr,
    guard: Option<Expr>,
    body: Expr,
}

fn classify_for_loop(
    components: ForLoopComponents,
    ctx: &mut ClassificationContext<'_>,
) -> RuleBodyTerm {
    let body_terms = classify_for_body_with_aggregation_tracking(components.body, ctx);

    RuleBodyTerm::ForLoop(RuleForLoop {
        pattern: components.pattern,
        iterable: components.iterable,
        guard: components.guard,
        body_terms,
    })
}

fn classify_for_body_with_aggregation_tracking(
    body: Expr,
    ctx: &mut ClassificationContext<'_>,
) -> Vec<RuleBodyTerm> {
    match body {
        Expr::Binary {
            op: BinaryOp::Seq,
            lhs,
            rhs,
        } => {
            let mut terms = classify_for_body_with_aggregation_tracking(*lhs, ctx);
            terms.extend(classify_for_body_with_aggregation_tracking(*rhs, ctx));
            terms
        }
        Expr::ForLoop {
            pattern,
            iterable,
            guard,
            body,
        } => {
            vec![classify_for_loop(
                ForLoopComponents {
                    pattern,
                    iterable: *iterable,
                    guard: guard.map(|guard| *guard),
                    body: *body,
                },
                ctx,
            )]
        }
        Expr::Group(inner) => classify_for_body_with_aggregation_tracking(*inner, ctx),
        other => classify_expression(other, ctx).into_iter().collect(),
    }
}

fn multiple_aggregations_error(_first_span: &Span, second_span: &Span) -> Simple<SyntaxKind> {
    Simple::custom(
        second_span.clone(),
        "at most one aggregation (group_by or Aggregate) is permitted per rule body".to_string(),
    )
}

#[cfg(test)]
mod tests {
    #![expect(clippy::expect_used, reason = "tests assert exact parser output")]

    use super::*;
    use chumsky::error::SimpleReason;

    fn span_for(src: &str) -> Span {
        0..src.len()
    }

    #[test]
    fn assignment_literal_becomes_assignment_term() {
        let src = "var item = FlatMap(items)";
        let mut first_aggregation_span = None;
        let mut errors = Vec::new();

        let term =
            parse_rule_body_term(src, span_for(src), &mut first_aggregation_span, &mut errors)
                .expect("assignment should parse");

        let RuleBodyTerm::Assignment(assignment) = term else {
            panic!("expected assignment term");
        };
        assert_eq!(assignment.pattern.to_source(), "var item");
        assert_eq!(assignment.value.to_sexpr(), "(call FlatMap items)");
        assert!(errors.is_empty());
    }

    #[test]
    fn second_aggregation_reports_error() {
        let src = "group_by(count(), key)";
        let mut first_aggregation_span = Some(0..4);
        let mut errors = Vec::new();

        let term =
            parse_rule_body_term(src, span_for(src), &mut first_aggregation_span, &mut errors);

        assert!(term.is_none());
        assert_eq!(errors.len(), 1);
        let error = errors.first().expect("aggregation error missing");
        assert!(matches!(
            error.reason(),
            SimpleReason::Custom(message)
                if message == "at most one aggregation (group_by or Aggregate) is permitted per rule body"
        ));
    }
}
