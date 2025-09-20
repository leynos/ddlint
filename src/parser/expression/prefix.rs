//! Parsing of prefix expressions and literals for the Pratt parser.

use crate::parser::ast::{Expr, Literal, prefix_binding_power};
use crate::{Span, SyntaxKind};

use super::pratt::Pratt;

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
            SyntaxKind::T_LBRACE => self.parse_brace_group(),
            SyntaxKind::K_IF => self.parse_if_expression(),
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

    fn parse_identifier_or_struct(&mut self, span: &Span) -> Option<Expr> {
        let name = self.ts.slice(span);
        self.parse_ident_expression(name)
    }

    fn parse_ident_expression(&mut self, name: String) -> Option<Expr> {
        if matches!(self.ts.peek_kind(), Some(SyntaxKind::T_LBRACE)) {
            if !self
                .struct_literal_guard
                .should_parse_struct_literal(self.expr_depth)
            {
                return Some(Expr::Variable(name));
            }
            self.ts.next_tok();
            let fields = self.parse_struct_fields()?;
            if !self.ts.expect(SyntaxKind::T_RBRACE) {
                return None;
            }
            Some(Expr::Struct { name, fields })
        } else {
            self.struct_literal_guard
                .consume_without_struct(self.expr_depth);
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
            #[expect(
                clippy::expect_used,
                reason = "parser invariant: group contains exactly one item"
            )]
            let item = items.pop().expect("expected one item in group");
            Some(Expr::Group(Box::new(item)))
        }
    }

    fn parse_brace_group(&mut self) -> Option<Expr> {
        // `{ expr }` – alternative grouping (not a struct literal; those are parsed after an identifier).
        if matches!(self.ts.peek_kind(), Some(SyntaxKind::T_RBRACE)) {
            let sp = self.ts.peek_span().unwrap_or_else(|| self.ts.eof_span());
            self.ts.push_error(sp, "expected expression");
            return None;
        }
        let inner = self.parse_expr(0)?;
        if !self.ts.expect(SyntaxKind::T_RBRACE) {
            return None;
        }
        Some(Expr::Group(Box::new(inner)))
    }

    fn parse_closure_literal(&mut self) -> Option<Expr> {
        let params = self.parse_closure_params()?;
        if !self.ts.expect(SyntaxKind::T_PIPE) {
            return None;
        }
        let body = self.parse_expr(0)?;
        Some(Expr::Closure {
            params,
            body: Box::new(body),
        })
    }

    fn parse_if_expression(&mut self) -> Option<Expr> {
        let condition = self.parse_if_condition()?;
        let then_branch =
            self.parse_if_clause("expected expression for 'then' branch of 'if'", None)?;
        let else_branch = if matches!(self.ts.peek_kind(), Some(SyntaxKind::K_ELSE)) {
            let else_span = self
                .ts
                .next_tok()
                .map_or_else(|| self.ts.eof_span(), |(_, span)| span);
            self.parse_if_clause(
                "expected expression for 'else' branch of 'if'",
                Some(else_span),
            )?
        } else {
            Expr::Tuple(Vec::new())
        };
        Some(Expr::IfElse {
            condition: Box::new(condition),
            then_branch: Box::new(then_branch),
            else_branch: Box::new(else_branch),
        })
    }

    fn parse_if_condition(&mut self) -> Option<Expr> {
        self.struct_literal_guard
            .disallow_at_depth(self.expr_depth + 1);
        let result = self.parse_if_clause("expected condition expression after 'if'", None);
        self.struct_literal_guard.reset();
        result
    }

    fn parse_if_clause(&mut self, expectation: &str, fallback: Option<Span>) -> Option<Expr> {
        if matches!(self.ts.peek_kind(), Some(SyntaxKind::K_ELSE)) {
            let span = self.ts.peek_span().unwrap_or_else(|| self.ts.eof_span());
            self.ts.push_error(span, expectation.to_string());
            return None;
        }
        let error_count = self.ts.errors.len();
        let expr = self.parse_expr(0);
        if expr.is_none() && self.ts.errors.len() == error_count {
            let span = self
                .ts
                .peek_span()
                .or(fallback)
                .unwrap_or_else(|| self.ts.eof_span());
            self.ts.push_error(span, expectation.to_string());
        }
        expr
    }

    /// Parse a comma-separated list of identifiers up to (but not consuming) `terminator`.
    /// Returns an empty vector if the next token is `terminator`. Callers must consume `terminator`.
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
                    .unwrap_or(raw.as_str())
                    .to_string();
                Some(Expr::Literal(Literal::String(value)))
            }
            SyntaxKind::K_TRUE => Some(Expr::Literal(Literal::Bool(true))),
            SyntaxKind::K_FALSE => Some(Expr::Literal(Literal::Bool(false))),
            _ => None,
        }
    }
}
