//! Diff-marker postfix helpers for the Pratt expression parser.
//!
//! Tracks a pending `'` diff-marker span across the postfix chain and wraps
//! completed postfix expressions in [`Expr::AtomDiff`] via `apply_pending_diff`.

use crate::parser::ast::Expr;
use crate::{Span, SyntaxKind};

use super::Pratt;

impl<I> Pratt<'_, I>
where
    I: Iterator<Item = (SyntaxKind, Span)> + Clone,
{
    pub(super) fn handle_diff_marker(&mut self, pending: &mut Option<Span>) {
        let (_, span) = self.ts.next_tok().unwrap_or_else(|| {
            let span = self.ts.eof_span();
            (SyntaxKind::N_ERROR, span)
        });
        if pending.is_some() {
            self.ts.push_error(span, "duplicate diff marker");
        } else {
            *pending = Some(span);
        }
    }

    pub(super) fn apply_pending_diff(expr: Expr, pending: &mut Option<Span>) -> Expr {
        if pending.take().is_some() {
            Expr::AtomDiff {
                expr: Box::new(expr),
            }
        } else {
            expr
        }
    }

    pub(super) fn emit_error_if_diff_pending(&mut self, pending: &mut Option<Span>) {
        if let Some(diff_span) = pending.take() {
            self.ts
                .push_error(diff_span, "diff marker must apply to an atom");
        }
    }

    pub(super) fn validate_no_pending_diff(&mut self, pending: &mut Option<Span>) -> Option<()> {
        if let Some(span) = pending.take() {
            self.ts
                .push_error(span, "diff marker must be followed by atom arguments");
            return None;
        }
        Some(())
    }
}
