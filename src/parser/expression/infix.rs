//! Infix operator handling for the Pratt parser.

use super::pratt::Pratt;
use crate::{
    Span,
    SyntaxKind,
    parser::{
        ast::{Expr, infix_binding_power},
        reserved_tokens::rejection_for,
    },
};

impl<I> Pratt<'_, I>
where
    I: Iterator<Item = (SyntaxKind, Span)> + Clone,
{
    /// Fold infix operators into `lhs` until the next token binds no tighter
    /// than `min_bp`.
    ///
    /// While a block item is being parsed, `;` terminates the item instead of
    /// forming a [`crate::parser::ast::BinaryOp::Seq`] node; the block parser
    /// then consumes it as the item separator.
    pub(super) fn parse_infix(&mut self, mut lhs: Expr, min_bp: u8) -> Option<Expr> {
        while let Some(op_kind) = self.ts.peek_kind() {
            if self.semicolon_terminates() && op_kind == SyntaxKind::T_SEMI {
                break;
            }

            if let Some(message) = rejection_for(op_kind) {
                let (_, op_span) = self.ts.next_tok()?;
                self.ts.push_reserved_error(op_span, message);
                return None;
            }

            let Some((l_bp, r_bp, op)) = infix_binding_power(op_kind) else {
                break;
            };
            if l_bp < min_bp {
                break;
            }
            let (_, op_span) = self.ts.next_tok()?;
            let Some(rhs) = (match op_kind {
                SyntaxKind::T_COLON | SyntaxKind::K_AS => self.parse_type(),
                _ => self.parse_expr(r_bp),
            }) else {
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
