//! Parsing of prefix expressions and literals for the Pratt parser.

use crate::parser::ast::{Expr, Literal, prefix_binding_power};
use crate::{Span, SyntaxKind};

use super::Pratt;

impl<I> Pratt<'_, I>
where
    I: Iterator<Item = (SyntaxKind, Span)>,
{
    pub(super) fn parse_prefix(&mut self) -> Option<Expr> {
        let (kind, span) = self.ts.next_tok()?;
        if let Some(lit) = self.parse_literal(kind, &span) {
            return Some(lit);
        }
        match kind {
            SyntaxKind::T_IDENT => self.parse_identifier_or_struct(&span),
            SyntaxKind::T_LPAREN => self.parse_parenthesized_expr(),
            SyntaxKind::T_PIPE => self.parse_closure_literal(),
            k => {
                let Some((bp, op)) = prefix_binding_power(k) else {
                    self.ts.push_error(span, "unexpected token");
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

    fn parse_identifier_or_struct(&mut self, span: &Span) -> Option<Expr> {
        let name = self.ts.slice(span);
        self.parse_ident_expression(name)
    }

    fn parse_ident_expression(&mut self, name: String) -> Option<Expr> {
        if matches!(self.ts.peek_kind(), Some(SyntaxKind::T_LBRACE)) {
            self.ts.next_tok();
            let fields = self.parse_struct_fields()?;
            if !self.ts.expect(SyntaxKind::T_RBRACE) {
                return None;
            }
            Some(Expr::Struct { name, fields })
        } else {
            Some(Expr::Variable(name))
        }
    }

    fn parse_parenthesized_expr(&mut self) -> Option<Expr> {
        if matches!(self.ts.peek_kind(), Some(SyntaxKind::T_RPAREN)) {
            self.ts.next_tok();
            return Some(Expr::Tuple(Vec::new()));
        }
        let first = self.parse_expr(0)?;
        let mut items = vec![first];
        let mut is_tuple = false;
        while matches!(self.ts.peek_kind(), Some(SyntaxKind::T_COMMA)) {
            is_tuple = true;
            self.ts.next_tok();
            if matches!(self.ts.peek_kind(), Some(SyntaxKind::T_RPAREN)) {
                break;
            }
            items.push(self.parse_expr(0)?);
        }
        if !self.ts.expect(SyntaxKind::T_RPAREN) {
            return None;
        }
        if is_tuple {
            Some(Expr::Tuple(items))
        } else {
            #[expect(clippy::expect_used, reason = "tuple contains one element")]
            let item = items.pop().expect("group expression missing item");
            Some(Expr::Group(Box::new(item)))
        }
    }

    fn parse_closure_literal(&mut self) -> Option<Expr> {
        let params = self.parse_closure_params()?;
        if !matches!(self.ts.peek_kind(), Some(SyntaxKind::T_PIPE)) {
            let span = self.ts.peek_span().unwrap_or_else(|| self.ts.eof_span());
            self.ts.push_error(span, "expected `|`");
            return None;
        }
        self.ts.next_tok();
        let body = self.parse_expr(0)?;
        Some(Expr::Closure {
            params,
            body: Box::new(body),
        })
    }

    fn parse_comma_separated_identifiers<T>(
        &mut self,
        terminator: SyntaxKind,
        err_msg: &'static str,
        mut process_item: impl FnMut(&mut Self, String) -> Option<T>,
        allow_trailing_comma: bool,
    ) -> Option<Vec<T>> {
        let mut items = Vec::new();
        if matches!(self.ts.peek_kind(), Some(k) if k == terminator) {
            return Some(items);
        }
        loop {
            let (k, sp) = self.ts.next_tok()?;
            if k != SyntaxKind::T_IDENT {
                self.ts.push_error(sp, err_msg);
                return None;
            }
            let name = self.ts.slice(&sp);
            items.push(process_item(self, name)?);
            if !matches!(self.ts.peek_kind(), Some(SyntaxKind::T_COMMA)) {
                break;
            }
            self.ts.next_tok();
            if allow_trailing_comma && matches!(self.ts.peek_kind(), Some(k) if k == terminator) {
                break;
            }
        }
        Some(items)
    }

    fn parse_struct_fields(&mut self) -> Option<Vec<(String, Expr)>> {
        self.parse_comma_separated_identifiers(
            SyntaxKind::T_RBRACE,
            "expected field name",
            |parser, name| {
                if !parser.ts.expect(SyntaxKind::T_COLON) {
                    return None;
                }
                let value = parser.parse_expr(0)?;
                Some((name, value))
            },
            true,
        )
    }

    fn parse_closure_params(&mut self) -> Option<Vec<String>> {
        self.parse_comma_separated_identifiers(
            SyntaxKind::T_PIPE,
            "expected parameter name",
            |_parser, name| Some(name),
            true,
        )
    }

    fn parse_literal(&self, kind: SyntaxKind, span: &Span) -> Option<Expr> {
        match kind {
            SyntaxKind::T_NUMBER => Some(Expr::Literal(Literal::Number(self.ts.slice(span)))),
            SyntaxKind::T_STRING => {
                let raw = self.ts.slice(span);
                let value = raw
                    .strip_prefix('"')
                    .and_then(|s| s.strip_suffix('"'))
                    .unwrap_or(&raw)
                    .to_string();
                Some(Expr::Literal(Literal::String(value)))
            }
            SyntaxKind::K_TRUE => Some(Expr::Literal(Literal::Bool(true))),
            SyntaxKind::K_FALSE => Some(Expr::Literal(Literal::Bool(false))),
            _ => None,
        }
    }
}
