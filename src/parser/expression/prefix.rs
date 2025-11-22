//! Parsing of prefix expressions for the Pratt parser.

use crate::parser::ast::{Expr, prefix_binding_power};
use crate::{Span, SyntaxKind};

use super::pratt::Pratt;

impl<I> Pratt<'_, I>
where
    I: Iterator<Item = (SyntaxKind, Span)> + Clone,
{
    pub(super) fn parse_prefix(&mut self) -> Option<Expr> {
        let (kind, span) = self.ts.next_tok()?;
        if let Some(lit) = self.parse_literal(kind, &span) {
            return Some(lit);
        }
        match kind {
            SyntaxKind::T_IDENT => self.parse_identifier_or_struct(&span),
            SyntaxKind::K_FLATMAP | SyntaxKind::K_AGGREGATE => {
                let name = self.ts.slice(&span);
                self.parse_ident_expression(name, &span)
            }
            SyntaxKind::K_UNDERSCORE => Some(Expr::Variable("_".to_string())),
            SyntaxKind::T_LPAREN => self.parse_parenthesized_expr(),
            SyntaxKind::T_PIPE => self.parse_closure_literal(),
            SyntaxKind::T_LBRACE => self.parse_brace_group(),
            SyntaxKind::K_IF => self.parse_if_expression(),
            SyntaxKind::K_MATCH => self.parse_match_expression(),
            SyntaxKind::K_FOR => self.parse_for_expression(),
            SyntaxKind::K_BREAK => Some(Self::parse_break_expression()),
            SyntaxKind::K_CONTINUE => Some(Self::parse_continue_expression()),
            SyntaxKind::K_RETURN => self.parse_return_expression(),
            k => {
                let Some((bp, op)) = prefix_binding_power(k) else {
                    self.ts
                        .push_error(span.clone(), format!("unexpected token: {k:?}"));
                    return None;
                };
                let rhs = self.parse_expr(bp)?;
                Some(Expr::Unary {
                    op,
                    expr: Box::new(rhs),
                })
            }
        }
    }
}
