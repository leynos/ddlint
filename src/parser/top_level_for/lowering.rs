//! Lowering helpers for top-level `for` statements.

use chumsky::error::Simple;

use crate::parser::ast::{
    BinaryOp, Expr, Pattern, SemanticRule, SemanticRuleOrigin, SemanticRuleSpec,
};
use crate::{Span, SyntaxKind};

/// Diagnostic for top-level `for` statements whose bodies cannot lower.
pub(crate) const UNSUPPORTED_TOP_LEVEL_FOR_STATEMENT: &str = concat!(
    "top-level `for` body must end in an atom-like expression ",
    "(for example `Rel(args)`)"
);

pub(crate) fn lower_top_level_for(
    expression: Expr,
    statement_span: Span,
    errors: &mut Vec<Simple<SyntaxKind>>,
) -> Option<SemanticRule> {
    let mut body = Vec::new();
    let mut patterns = Vec::new();
    let Some(head) = collect_lowered_terms(expression, &mut patterns, &mut body) else {
        errors.push(Simple::custom(
            statement_span.clone(),
            UNSUPPORTED_TOP_LEVEL_FOR_STATEMENT,
        ));
        return None;
    };
    if !is_supported_head_expression(&head) {
        errors.push(Simple::custom(
            statement_span.clone(),
            UNSUPPORTED_TOP_LEVEL_FOR_STATEMENT,
        ));
        return None;
    }

    Some(SemanticRule::new(SemanticRuleSpec {
        origin: SemanticRuleOrigin::TopLevelFor,
        source_span: statement_span,
        patterns,
        head,
        body,
    }))
}

fn collect_lowered_terms(
    expression: Expr,
    patterns: &mut Vec<Pattern>,
    body_terms: &mut Vec<Expr>,
) -> Option<Expr> {
    match expression {
        Expr::ForLoop {
            pattern,
            iterable,
            guard,
            body,
        } => {
            patterns.push(pattern);
            body_terms.push(*iterable);
            if let Some(guard_expr) = guard {
                body_terms.push(*guard_expr);
            }
            collect_lowered_terms(*body, patterns, body_terms)
        }
        Expr::IfElse { .. }
        | Expr::Match { .. }
        | Expr::Break
        | Expr::Continue
        | Expr::Return { .. }
        | Expr::Binary {
            op: BinaryOp::Seq, ..
        } => None,
        other => Some(other),
    }
}

fn is_supported_head_expression(expr: &Expr) -> bool {
    match expr {
        Expr::Call { .. } | Expr::Apply { .. } => true,
        Expr::AtomDiff { expr } | Expr::AtomDelay { expr, .. } => {
            is_supported_head_expression(expr)
        }
        _ => false,
    }
}
