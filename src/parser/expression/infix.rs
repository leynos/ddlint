//! Infix operator handling for the Pratt parser.

use crate::parser::ast::{Expr, infix_binding_power};
use crate::{Span, SyntaxKind};

use super::Pratt;

impl<I> Pratt<'_, I>
where
    I: Iterator<Item = (SyntaxKind, Span)>,
{
    pub(super) fn parse_infix(&mut self, mut lhs: Expr, min_bp: u8) -> Option<Expr> {
        while let Some(op_kind) = self.peek() {
            let Some((l_bp, r_bp, op)) = infix_binding_power(op_kind) else {
                break;
            };
            if l_bp < min_bp {
                break;
            }
            self.next();
            let rhs = self.parse_expr(r_bp)?;
            lhs = Expr::Binary {
                op,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            };
        }
        Some(lhs)
    }
}
