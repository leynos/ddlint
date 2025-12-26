//! Infix operator handling for the Pratt parser.

use crate::parser::ast::{Expr, infix_binding_power};
use crate::{Span, SyntaxKind};

use super::pratt::Pratt;

impl<I> Pratt<'_, I>
where
    I: Iterator<Item = (SyntaxKind, Span)> + Clone,
{
    pub(super) fn parse_infix(&mut self, lhs: Expr, min_bp: u8) -> Option<Expr> {
        self.parse_infix_with_colon_mode(lhs, min_bp, false)
    }

    /// Parse infix operators, optionally treating `:` as a terminator.
    ///
    /// When `colon_terminates` is true, the `:` operator is not consumed as type
    /// ascription but instead terminates parsing. This is used when parsing map
    /// keys where `:` separates key from value rather than being type ascription.
    pub(super) fn parse_infix_with_colon_mode(
        &mut self,
        mut lhs: Expr,
        min_bp: u8,
        colon_terminates: bool,
    ) -> Option<Expr> {
        while let Some(op_kind) = self.ts.peek_kind() {
            // When parsing map keys, treat `:` as a terminator
            if colon_terminates && op_kind == SyntaxKind::T_COLON {
                break;
            }

            let Some((l_bp, r_bp, op)) = infix_binding_power(op_kind) else {
                break;
            };
            if l_bp < min_bp {
                break;
            }
            let Some((_, op_span)) = self.ts.next_tok() else {
                unreachable!("peeked operator");
            };
            let rhs = match op_kind {
                SyntaxKind::T_COLON | SyntaxKind::K_AS => self.parse_type(),
                _ => self.parse_expr_for_map_key(r_bp, colon_terminates),
            };
            let Some(rhs) = rhs else {
                self.ts
                    .push_error(op_span, "missing right-hand side for operator");
                return None;
            };
            lhs = Expr::Binary {
                op,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            };
        }
        Some(lhs)
    }

    /// Parse an expression, optionally in map-key mode where `:` terminates.
    fn parse_expr_for_map_key(&mut self, min_bp: u8, colon_terminates: bool) -> Option<Expr> {
        if colon_terminates {
            self.parse_map_key_expr(min_bp)
        } else {
            self.parse_expr(min_bp)
        }
    }
}
