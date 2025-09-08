//! Infix operator handling for the Pratt parser.

use crate::parser::ast::{Expr, infix_binding_power};
use crate::{Span, SyntaxKind};

use super::pratt::Pratt;

impl<I> Pratt<'_, I>
where
    I: Iterator<Item = (SyntaxKind, Span)>,
{
    pub(super) fn parse_infix(&mut self, mut lhs: Expr, min_bp: u8) -> Option<Expr> {
        while let Some(op_kind) = self.ts.peek_kind() {
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
                _ => self.parse_expr(r_bp),
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
}
