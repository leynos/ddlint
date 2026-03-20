//! Lowering helpers for top-level `for` statements.
//!
//! "Lowering" here means translating the parser's high-level top-level `for`
//! surface syntax into the desugared [`SemanticRule`] form consumed by later
//! semantic analysis and linting. The helpers in this module flatten nested
//! top-level loops into a canonical head-plus-body representation, preserve the
//! original source span for diagnostics, and collect the loop patterns that
//! introduce bindings for the lowered rule scope.
//!
//! Consumers should expect two main invariants from this module:
//! - successful lowering yields a `SemanticRule` whose body terms are emitted
//!   in evaluation order and whose head is atom-like;
//! - unsupported control-flow constructs in the lowered body, or a non
//!   atom-like head, are rejected with targeted diagnostics instead of
//!   producing a partial rule.
//!
//! This module therefore owns the parser-side binding and scope hand-off for
//! top-level `for` desugaring. Related helpers for interpreting those patterns
//! and bindings live in the semantic model builder.

use chumsky::error::Simple;

use crate::parser::ast::{
    BinaryOp, Expr, Pattern, SemanticRule, SemanticRuleOrigin, SemanticRuleSpec,
};
use crate::{Span, SyntaxKind};

/// Diagnostic for top-level `for` statements whose bodies cannot lower.
pub(crate) const UNSUPPORTED_TOP_LEVEL_FOR_BODY: &str = concat!(
    "top-level `for` body contains unsupported control flow before the head ",
    "(for example `if`, `match`, `break`, `continue`, `return`, or sequencing)"
);

/// Diagnostic for top-level `for` statements whose lowered heads are not atom-like.
pub(crate) const UNSUPPORTED_TOP_LEVEL_FOR_HEAD: &str = concat!(
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
            UNSUPPORTED_TOP_LEVEL_FOR_BODY,
        ));
        return None;
    };
    if !is_supported_head_expression(&head) {
        errors.push(Simple::custom(
            statement_span.clone(),
            UNSUPPORTED_TOP_LEVEL_FOR_HEAD,
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
