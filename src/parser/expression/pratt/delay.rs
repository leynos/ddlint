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
    /// Parse a `-<N>` delay postfix, returning an [`Expr::AtomDelay`] around
    /// the expression parsed before the postfix marker.
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

#[cfg(test)]
mod tests {
    #![expect(clippy::expect_used, reason = "tests assert exact parser output")]

    use chumsky::error::SimpleReason;

    use crate::parser::expression::parse_expression;

    #[test]
    fn parses_atom_delay_postfix() {
        let expr = parse_expression("signal-<3>").expect("delay expression should parse");

        assert_eq!(expr.to_sexpr(), "(delay 3 signal)");
    }

    #[test]
    fn rejects_non_numeric_delay_value() {
        let errors =
            parse_expression("signal -< delay").expect_err("non-numeric delay should fail");

        assert!(errors.iter().any(|error| matches!(
            error.reason(),
            SimpleReason::Custom(message) if message == "expected delay value"
        )));
    }
}
