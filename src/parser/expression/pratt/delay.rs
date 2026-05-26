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
    use chumsky::error::SimpleReason;

    use crate::parser::ast::Expr;
    use crate::parser::expression::parse_expression;
    use crate::tokenize_without_trivia;
    use crate::{Span, SyntaxKind};

    #[test]
    #[expect(clippy::expect_used, reason = "test asserts exact parser output")]
    fn parses_atom_delay_postfix() {
        let expr = parse_expression("signal-<3>").expect("delay expression should parse");

        assert_eq!(expr.to_sexpr(), "(delay 3 signal)");
    }

    #[test]
    #[expect(clippy::expect_used, reason = "test asserts exact parser error")]
    fn rejects_non_numeric_delay_value() {
        let errors =
            parse_expression("signal -< delay").expect_err("non-numeric delay should fail");

        assert!(errors.iter().any(|error| matches!(
            error.reason(),
            SimpleReason::Custom(message) if message == "expected delay value"
        )));
    }

    #[test]
    #[expect(clippy::expect_used, reason = "test asserts exact parser output")]
    fn parse_delay_postfix_accepts_max_u32_delay() {
        let mut parser = delay_parser("-<4294967295>");

        let expr = parser
            .parse_delay_postfix(Expr::Variable("signal".into()))
            .expect("u32 max delay should parse");

        assert_eq!(expr.to_sexpr(), "(delay 4294967295 signal)");
    }

    #[test]
    fn parse_delay_postfix_rejects_delay_above_u32_max() {
        let mut parser = delay_parser("-<4294967296>");

        let expr = parser.parse_delay_postfix(Expr::Variable("signal".into()));

        assert!(expr.is_none());
        let errors = parser.ts.take_errors();
        assert!(has_custom_error(&errors, "delay must fit u32"));
    }

    #[test]
    fn parse_delay_postfix_rejects_empty_delay_value() {
        let mut parser = delay_parser("-<>");

        let expr = parser.parse_delay_postfix(Expr::Variable("signal".into()));

        assert!(expr.is_none());
        let errors = parser.ts.take_errors();
        assert!(has_custom_error(&errors, "expected delay value"));
    }

    #[test]
    fn parse_delay_postfix_rejects_negative_delay_value() {
        let mut parser = delay_parser("-<-1>");

        let expr = parser.parse_delay_postfix(Expr::Variable("signal".into()));

        assert!(expr.is_none());
        let errors = parser.ts.take_errors();
        assert!(has_custom_error(&errors, "expected delay value"));
    }

    fn delay_parser(src: &str) -> super::Pratt<'_, std::vec::IntoIter<(SyntaxKind, Span)>> {
        super::Pratt::new(tokenize_without_trivia(src).into_iter(), src)
    }

    fn has_custom_error(errors: &[chumsky::error::Simple<SyntaxKind>], expected: &str) -> bool {
        errors.iter().any(|error| {
            matches!(
                error.reason(),
                SimpleReason::Custom(message) if message == expected
            )
        })
    }
}
