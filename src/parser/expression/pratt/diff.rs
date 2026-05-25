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
    /// Consume a diff marker and remember that the next completed postfix atom
    /// should be wrapped in [`Expr::AtomDiff`].
    pub(super) fn handle_diff_marker(&mut self, pending: &mut Option<Span>) -> Option<()> {
        let (_, span) = self.ts.next_tok()?;
        if pending.is_some() {
            self.ts.push_error(span, "duplicate diff marker");
        } else {
            *pending = Some(span);
        }
        Some(())
    }

    /// Wrap `expr` in [`Expr::AtomDiff`] when a preceding diff marker is
    /// pending, clearing that marker as part of the operation.
    pub(super) fn apply_pending_diff(expr: Expr, pending: &mut Option<Span>) -> Expr {
        if pending.take().is_some() {
            Expr::AtomDiff {
                expr: Box::new(expr),
            }
        } else {
            expr
        }
    }

    /// Emit the diagnostic used when a pending diff marker cannot legally bind
    /// to the postfix form about to be parsed.
    pub(super) fn emit_error_if_diff_pending(&mut self, pending: &mut Option<Span>) {
        if let Some(diff_span) = pending.take() {
            self.ts
                .push_error(diff_span, "diff marker must apply to an atom");
        }
    }

    /// Ensure the postfix chain did not end with an unbound diff marker.
    pub(super) fn validate_no_pending_diff(&mut self, pending: &mut Option<Span>) -> Option<()> {
        if let Some(span) = pending.take() {
            self.ts
                .push_error(span, "diff marker must be followed by atom arguments");
            return None;
        }
        Some(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn apply_pending_diff_wraps_and_clears_marker() {
        let mut pending = Some(0..1);
        let expr = Pratt::<'_, std::vec::IntoIter<(SyntaxKind, Span)>>::apply_pending_diff(
            Expr::Variable("atom".into()),
            &mut pending,
        );

        assert_eq!(
            expr,
            Expr::AtomDiff {
                expr: Box::new(Expr::Variable("atom".into()))
            }
        );
        assert!(pending.is_none());
    }

    #[test]
    fn apply_pending_diff_leaves_expression_when_no_marker_is_pending() {
        let mut pending = None;
        let expr = Pratt::<'_, std::vec::IntoIter<(SyntaxKind, Span)>>::apply_pending_diff(
            Expr::Variable("atom".into()),
            &mut pending,
        );

        assert_eq!(expr, Expr::Variable("atom".into()));
    }
}
