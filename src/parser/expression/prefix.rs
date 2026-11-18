//! Parsing of prefix expressions for the Pratt parser.

use super::pratt::Pratt;
use crate::{
    Span,
    SyntaxKind,
    parser::{
        ast::{Expr, prefix_binding_power},
        reserved_tokens::rejection_for,
    },
};

impl<I> Pratt<'_, I>
where
    I: Iterator<Item = (SyntaxKind, Span)> + Clone,
{
    /// Parse the next token as a prefix expression, literal, or prefix operator.
    ///
    /// Unsupported reserved tokens emit their shared policy diagnostic and
    /// return no expression; ordinary unexpected tokens retain parser errors.
    pub(super) fn parse_prefix(&mut self) -> Option<Expr> {
        let (kind, span) = self.ts.next_tok()?;
        if let Some(lit) = self.parse_literal(kind, &span) {
            return Some(lit);
        }
        if matches!(kind, SyntaxKind::T_NUMBER | SyntaxKind::T_STRING) {
            // Literal parsing already emitted a precise diagnostic; avoid a
            // follow-up "unexpected token" error for the same span.
            return None;
        }
        self.parse_prefix_by_kind(kind, span)
    }

    /// Dispatch a non-literal prefix token to its parser.
    ///
    /// `kind` and `span` come from the token that `parse_prefix` consumed.
    /// Scope: called only from `parse_prefix`.
    fn parse_prefix_by_kind(&mut self, kind: SyntaxKind, span: Span) -> Option<Expr> {
        match kind {
            SyntaxKind::T_IDENT => self.parse_identifier_or_struct(&span),
            SyntaxKind::K_FLATMAP | SyntaxKind::K_AGGREGATE => {
                let name = self.ts.slice(&span);
                self.parse_ident_expression(name, &span)
            }
            SyntaxKind::K_UNDERSCORE => Some(Expr::Variable("_".to_owned())),
            SyntaxKind::T_LPAREN => self.parse_parenthesized_expr(),
            SyntaxKind::T_PIPE => self.parse_closure_literal(),
            SyntaxKind::T_LBRACKET => self.parse_vector_literal(),
            SyntaxKind::T_LBRACE => self.parse_block(),
            SyntaxKind::K_IF => self.parse_if_expression(),
            SyntaxKind::K_MATCH => self.parse_match_expression(),
            SyntaxKind::K_FOR => self.parse_for_expression(),
            SyntaxKind::K_BREAK => Some(Self::parse_break_expression()),
            SyntaxKind::K_CONTINUE => Some(Self::parse_continue_expression()),
            SyntaxKind::K_RETURN => self.parse_return_expression(),
            k => self.parse_operator_prefix(k, span),
        }
    }

    /// Parse a prefix operator application, or reject the token.
    ///
    /// Reserved tokens emit their shared policy diagnostic; tokens with no
    /// prefix binding power emit an "unexpected token" error. Either way the
    /// result is `None`. Scope: called only from `parse_prefix_by_kind`.
    fn parse_operator_prefix(&mut self, kind: SyntaxKind, span: Span) -> Option<Expr> {
        if let Some(message) = rejection_for(kind) {
            self.ts.push_reserved_error(span, message);
            return None;
        }
        let Some((bp, op)) = prefix_binding_power(kind) else {
            self.ts
                .push_error(span, format!("unexpected token: {kind:?}"));
            return None;
        };
        let rhs = self.parse_expr(bp)?;
        Some(Expr::Unary {
            op,
            expr: Box::new(rhs),
        })
    }
}
