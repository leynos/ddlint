//! Delay-postfix parsing for the Pratt expression parser.
//!
//! Handles the `expr -<N>` construct, consuming the `-<` token pair and
//! a decimal delay value to produce [`Expr::AtomDelay`].

use crate::parser::ast::Expr;
use crate::parser::span_utils::parse_u32_decimal;
use crate::{Span, SyntaxKind};

use super::Pratt;

impl<I> Pratt<'_, I>
where
    I: Iterator<Item = (SyntaxKind, Span)> + Clone,
{
    pub(super) fn parse_delay_postfix(&mut self, lhs: Expr) -> Option<Expr> {
        let (minus_kind, _) = self.ts.next_tok()?;
        debug_assert_eq!(
            minus_kind,
            SyntaxKind::T_MINUS,
            "parse_delay_postfix must be called with a leading '-' token"
        );
        if !self.ts.expect(SyntaxKind::T_LT) {
            return None;
        }
        let (kind, number_span) = self.ts.next_tok().unwrap_or_else(|| {
            let span = self.ts.eof_span();
            (SyntaxKind::N_ERROR, span)
        });
        if kind != SyntaxKind::T_NUMBER {
            self.ts.push_error(number_span, "expected delay value");
            return None;
        }
        let raw = self.ts.slice(&number_span);
        let delay = match parse_u32_decimal(&raw) {
            Ok(delay) => delay,
            Err(message) => {
                self.ts.push_error(number_span, message);
                return None;
            }
        };
        if !self.ts.expect(SyntaxKind::T_GT) {
            return None;
        }
        Some(Expr::AtomDelay {
            delay,
            expr: Box::new(lhs),
        })
    }
}
