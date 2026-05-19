//! Postfix-operator parsing for the Pratt expression parser.
//!
//! `parse_postfix` dispatches `(`, `[`, `.`, `-<`, and `'` postfix forms to
//! dedicated helpers, coordinating diff-marker tracking across the full chain.

use crate::parser::ast::Expr;
use crate::parser::expression::qualified::is_qualified_function_callee;
use crate::{Span, SyntaxKind};

use super::Pratt;

impl<I> Pratt<'_, I>
where
    I: Iterator<Item = (SyntaxKind, Span)> + Clone,
{
    /// Parse highest-precedence postfix operators.
    pub(super) fn parse_postfix(&mut self, mut lhs: Expr) -> Option<Expr> {
        let mut pending_diff_span: Option<Span> = None;
        loop {
            match self.ts.peek_kind() {
                Some(SyntaxKind::T_APOSTROPHE) => {
                    self.handle_diff_marker(&mut pending_diff_span);
                    continue;
                }
                Some(SyntaxKind::T_LPAREN) => {
                    lhs = self.parse_function_call_postfix(lhs)?;
                }
                Some(SyntaxKind::T_LBRACKET) => {
                    lhs = self.parse_bit_slice_postfix(lhs)?;
                }
                Some(SyntaxKind::T_DOT) => {
                    lhs = self.parse_dot_access_postfix(lhs)?;
                }
                Some(SyntaxKind::T_MINUS) if self.ts.peek_nth_kind(1) == Some(SyntaxKind::T_LT) => {
                    self.emit_error_if_diff_pending(&mut pending_diff_span);
                    lhs = self.parse_delay_postfix(lhs)?;
                }
                _ => break,
            }

            lhs = Self::apply_pending_diff(lhs, &mut pending_diff_span);
        }

        self.validate_no_pending_diff(&mut pending_diff_span)?;
        Some(lhs)
    }

    fn parse_function_call_postfix(&mut self, lhs: Expr) -> Option<Expr> {
        let args = self.parse_parenthesized_args()?;
        if is_qualified_function_callee(&lhs) {
            Some(Expr::Call {
                callee: Box::new(lhs),
                args,
            })
        } else {
            Some(Expr::Apply {
                callee: Box::new(lhs),
                args,
            })
        }
    }

    fn parse_bit_slice_postfix(&mut self, lhs: Expr) -> Option<Expr> {
        let _ = self.ts.next_tok();
        self.with_struct_literals_suspended(move |this| {
            let hi = this.parse_expr(0)?;
            if !this.ts.expect(SyntaxKind::T_COMMA) {
                return None;
            }
            let lo = this.parse_expr(0)?;
            if !this.ts.expect(SyntaxKind::T_RBRACKET) {
                return None;
            }
            Some(Expr::BitSlice {
                expr: Box::new(lhs),
                hi: Box::new(hi),
                lo: Box::new(lo),
            })
        })
    }

    fn parse_dot_access_postfix(&mut self, lhs: Expr) -> Option<Expr> {
        // token is consumed to advance past '.'
        let _ = self.ts.next_tok();
        match self.ts.next_tok() {
            Some((SyntaxKind::T_IDENT, span)) => {
                let name = self.ts.slice(&span);
                if matches!(self.ts.peek_kind(), Some(SyntaxKind::T_LPAREN)) {
                    self.parse_method_call(lhs, name)
                } else {
                    Some(Expr::FieldAccess {
                        expr: Box::new(lhs),
                        field: name,
                    })
                }
            }
            Some((SyntaxKind::T_NUMBER, span)) => {
                let index = self.ts.slice(&span);
                Some(Expr::TupleIndex {
                    expr: Box::new(lhs),
                    index,
                })
            }
            other => {
                let span = other.map_or_else(|| self.ts.eof_span(), |(_, span)| span);
                self.ts
                    .push_error(span, "expected identifier or tuple index after '.'");
                None
            }
        }
    }

    fn parse_method_call(&mut self, lhs: Expr, name: String) -> Option<Expr> {
        let args = self.parse_parenthesized_args()?;
        Some(Expr::MethodCall {
            recv: Box::new(lhs),
            name,
            args,
        })
    }

    fn parse_parenthesized_args(&mut self) -> Option<Vec<Expr>> {
        // token is consumed to advance past '('
        let _ = self.ts.next_tok();
        self.with_struct_literals_suspended(|this| {
            let args = this.parse_args()?;
            if !this.ts.expect(SyntaxKind::T_RPAREN) {
                return None;
            }
            Some(args)
        })
    }

    /// Parse a comma-separated argument list after the opening parenthesis has
    /// already been consumed.
    pub(super) fn parse_args(&mut self) -> Option<Vec<Expr>> {
        let mut args = Vec::new();
        if matches!(self.ts.peek_kind(), Some(SyntaxKind::T_RPAREN)) {
            return Some(args);
        }
        loop {
            if self.ts.peek_kind().is_none() {
                self.ts.expect(SyntaxKind::T_RPAREN);
                return None;
            }
            let expr = self.parse_expr(0)?;
            args.push(expr);
            if !matches!(self.ts.peek_kind(), Some(SyntaxKind::T_COMMA)) {
                break;
            }
            let _ = self.ts.next_tok();
            if matches!(self.ts.peek_kind(), Some(SyntaxKind::T_RPAREN)) {
                let span = self.ts.peek_span().unwrap_or_else(|| self.ts.eof_span());
                self.ts
                    .push_error(span, "unexpected trailing comma in argument list");
                return None;
            }
        }
        Some(args)
    }
}

#[cfg(test)]
mod tests {
    #![expect(clippy::expect_used, reason = "tests assert exact parser output")]

    use chumsky::error::SimpleReason;

    use crate::parser::expression::parse_expression;

    #[test]
    fn qualified_callee_uses_call_postfix() {
        let expr =
            parse_expression("std::map(value)").expect("qualified function call should parse");

        assert_eq!(expr.to_sexpr(), "(call std::map value)");
    }

    #[test]
    fn trailing_argument_comma_is_rejected() {
        let errors = parse_expression("f(1,)").expect_err("trailing comma should fail");

        assert!(errors.iter().any(|error| matches!(
            error.reason(),
            SimpleReason::Custom(message)
                if message == "unexpected trailing comma in argument list"
        )));
    }
}
