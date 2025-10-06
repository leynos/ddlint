//! Parsing of prefix expressions and literals for the Pratt parser.

use crate::parser::ast::{Expr, Literal, prefix_binding_power};
use crate::{Span, SyntaxKind};

use super::pratt::Pratt;

impl<I> Pratt<'_, I>
where
    I: Iterator<Item = (SyntaxKind, Span)> + Clone,
{
    fn is_at_top_level(paren_depth: usize, brace_depth: usize, bracket_depth: usize) -> bool {
        paren_depth == 0 && brace_depth == 0 && bracket_depth == 0
    }

    fn handle_open_delimiter(
        start: &mut Option<usize>,
        end: &mut Option<usize>,
        depth: &mut usize,
        span: &Span,
    ) {
        *depth += 1;
        start.get_or_insert(span.start);
        *end = Some(span.end);
    }

    fn handle_close_delimiter(
        &mut self,
        span: &Span,
        depth: &mut usize,
        unexpected_msg: &'static str,
    ) -> Option<usize> {
        if *depth == 0 {
            self.ts.push_error(span.clone(), unexpected_msg);
            return None;
        }
        *depth -= 1;
        Some(span.end)
    }

    fn handle_close_paren(
        &mut self,
        span: &Span,
        paren_depth: &mut usize,
        brace_depth: usize,
        bracket_depth: usize,
        end: &mut Option<usize>,
    ) -> Option<()> {
        if *paren_depth > 0 {
            *paren_depth -= 1;
            *end = Some(span.end);
            return Some(());
        }

        if Self::is_at_top_level(*paren_depth, brace_depth, bracket_depth) {
            self.ts
                .push_error(span.clone(), "expected 'in' before ')' in for-loop header");
        } else {
            self.ts.push_error(
                span.clone(),
                "unmatched closing parenthesis in for-loop pattern",
            );
        }

        None
    }

    fn validate_delimiter_balance(
        &mut self,
        paren_depth: usize,
        brace_depth: usize,
        bracket_depth: usize,
        last_span: Option<Span>,
    ) -> bool {
        if paren_depth > 0 {
            let span = last_span.unwrap_or_else(|| self.ts.eof_span());
            self.ts.push_error(
                span,
                format!(
                    "unmatched opening parenthesis in for-loop pattern: {paren_depth} unclosed"
                ),
            );
            return false;
        }
        if brace_depth > 0 {
            let span = last_span.unwrap_or_else(|| self.ts.eof_span());
            self.ts.push_error(
                span,
                format!("unmatched opening brace in for-loop pattern: {brace_depth} unclosed"),
            );
            return false;
        }
        if bracket_depth > 0 {
            let span = last_span.unwrap_or_else(|| self.ts.eof_span());
            self.ts.push_error(
                span,
                format!("unmatched opening bracket in for-loop pattern: {bracket_depth} unclosed"),
            );
            return false;
        }
        true
    }

    fn extract_pattern_text(
        &mut self,
        start: Option<usize>,
        end: Option<usize>,
        in_span: Span,
    ) -> Option<(String, Span)> {
        let (Some(s), Some(e)) = (start, end) else {
            self.ts.push_error(
                in_span.clone(),
                "expected binding before 'in' in for-loop header",
            );
            return None;
        };

        let span = s..e;
        let text = self.ts.slice(&span);
        let trimmed = text.trim();
        if trimmed.is_empty() {
            self.ts.push_error(
                span.clone(),
                "expected binding before 'in' in for-loop header",
            );
            return None;
        }

        Some((trimmed.to_string(), span))
    }

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
            SyntaxKind::K_FOR => self.parse_for_expression(),
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
        self.parse_ident_expression(name, span)
    }

    fn parse_ident_expression(&mut self, name: String, span: &Span) -> Option<Expr> {
        if !matches!(self.ts.peek_kind(), Some(SyntaxKind::T_LBRACE)) {
            return Some(Expr::Variable(name));
        }

        if !self.allows_struct_literals() {
            let next_kind = self.ts.peek_nth_kind(1);
            let colon_kind = self.ts.peek_nth_kind(2);
            let looks_like_struct = matches!(next_kind, Some(SyntaxKind::T_RBRACE))
                || matches!(colon_kind, Some(SyntaxKind::T_COLON));
            if looks_like_struct {
                self.ts.push_error(
                    span.clone(),
                    "struct literal syntax is not allowed in this context",
                );
            }
            return Some(Expr::Variable(name));
        }

        self.ts.next_tok();
        let fields = self.parse_struct_fields()?;
        if !self.ts.expect(SyntaxKind::T_RBRACE) {
            return None;
        }
        Some(Expr::Struct { name, fields })
    }

    fn parse_parenthesized_expr(&mut self) -> Option<Expr> {
        if matches!(self.ts.peek_kind(), Some(SyntaxKind::T_RPAREN)) {
            self.ts.next_tok();
            return Some(Expr::Tuple(Vec::new()));
        }
        self.with_struct_literals_suspended(|this| {
            let first = this.parse_expr(0)?;
            let mut items = vec![first];
            let mut is_tuple = false;
            while matches!(this.ts.peek_kind(), Some(SyntaxKind::T_COMMA)) {
                is_tuple = true;
                this.ts.next_tok();
                if matches!(this.ts.peek_kind(), Some(SyntaxKind::T_RPAREN)) {
                    break;
                }
                items.push(this.parse_expr(0)?);
            }
            if !this.ts.expect(SyntaxKind::T_RPAREN) {
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
        })
    }

    fn parse_brace_group(&mut self) -> Option<Expr> {
        // `{ expr }` – alternative grouping (not a struct literal; those are parsed after an identifier).
        if matches!(self.ts.peek_kind(), Some(SyntaxKind::T_RBRACE)) {
            let sp = self.ts.peek_span().unwrap_or_else(|| self.ts.eof_span());
            self.ts.push_error(sp, "expected expression");
            return None;
        }
        self.with_struct_literals_suspended(|this| {
            let inner = this.parse_expr(0)?;
            if !this.ts.expect(SyntaxKind::T_RBRACE) {
                return None;
            }
            Some(Expr::Group(Box::new(inner)))
        })
    }

    fn parse_closure_literal(&mut self) -> Option<Expr> {
        let params = self.parse_closure_params()?;
        if !self.ts.expect(SyntaxKind::T_PIPE) {
            return None;
        }
        let body = self.with_struct_literals_suspended(|this| this.parse_expr(0))?;
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

    fn parse_for_expression(&mut self) -> Option<Expr> {
        if !self.ts.expect(SyntaxKind::T_LPAREN) {
            return None;
        }

        let (pattern, pattern_span) = self.collect_for_pattern()?;
        if pattern.is_empty() {
            self.ts.push_error(
                pattern_span,
                "expected binding before 'in' in for-loop header",
            );
            return None;
        }

        let iterable = self.with_struct_literals_suspended(|this| this.parse_expr(0))?;

        let guard = if matches!(self.ts.peek_kind(), Some(SyntaxKind::K_IF)) {
            let (_, if_span) = self
                .ts
                .next_tok()
                .unwrap_or_else(|| (SyntaxKind::K_IF, self.ts.eof_span()));
            let guard_expr = self.with_struct_literal_activation(|this| {
                this.parse_if_clause(
                    "expected guard expression after 'if' in for-loop header",
                    Some(if_span.clone()),
                )
            })?;
            Some(Box::new(guard_expr))
        } else {
            None
        };

        if !self.ts.expect(SyntaxKind::T_RPAREN) {
            return None;
        }

        let body = self.with_struct_literals_suspended(|this| this.parse_expr(0))?;

        Some(Expr::ForLoop {
            pattern,
            iterable: Box::new(iterable),
            guard,
            body: Box::new(body),
        })
    }

    fn parse_if_condition(&mut self) -> Option<Expr> {
        self.with_struct_literal_activation(|this| {
            this.parse_if_clause("expected condition expression after 'if'", None)
        })
    }

    fn parse_if_clause(&mut self, expectation: &str, fallback: Option<Span>) -> Option<Expr> {
        if matches!(self.ts.peek_kind(), Some(SyntaxKind::K_ELSE)) {
            let span = self.ts.peek_span().unwrap_or_else(|| self.ts.eof_span());
            self.ts.push_error(span, expectation.to_string());
            return None;
        }
        let error_count = self.ts.error_count();
        let expr = self.parse_expr(0);
        if expr.is_none() && self.ts.error_count() == error_count {
            let span = self
                .ts
                .peek_span()
                .or(fallback)
                .unwrap_or_else(|| self.ts.eof_span());
            self.ts.push_error(span, expectation.to_string());
        }
        expr
    }

    fn collect_for_pattern(&mut self) -> Option<(String, Span)> {
        let mut paren_depth = 0usize;
        let mut brace_depth = 0usize;
        let mut bracket_depth = 0usize;
        let mut start = None;
        let mut end = None;
        let mut last_span: Option<Span> = None;

        while let Some((kind, span)) = self.ts.next_tok() {
            last_span = Some(span.clone());
            match kind {
                SyntaxKind::K_IN
                    if Self::is_at_top_level(paren_depth, brace_depth, bracket_depth) =>
                {
                    return self.extract_pattern_text(start, end, span);
                }
                SyntaxKind::T_LPAREN => {
                    Self::handle_open_delimiter(&mut start, &mut end, &mut paren_depth, &span);
                }
                SyntaxKind::T_RPAREN => {
                    self.handle_close_paren(
                        &span,
                        &mut paren_depth,
                        brace_depth,
                        bracket_depth,
                        &mut end,
                    )?;
                }
                SyntaxKind::T_LBRACE => {
                    Self::handle_open_delimiter(&mut start, &mut end, &mut brace_depth, &span);
                }
                SyntaxKind::T_RBRACE => {
                    end = Some(self.handle_close_delimiter(
                        &span,
                        &mut brace_depth,
                        "unmatched closing brace in for-loop pattern",
                    )?);
                }
                SyntaxKind::T_LBRACKET => {
                    Self::handle_open_delimiter(&mut start, &mut end, &mut bracket_depth, &span);
                }
                SyntaxKind::T_RBRACKET => {
                    end = Some(self.handle_close_delimiter(
                        &span,
                        &mut bracket_depth,
                        "unmatched closing bracket in for-loop pattern",
                    )?);
                }
                _ => {
                    start.get_or_insert(span.start);
                    end = Some(span.end);
                }
            }
        }

        if !self.validate_delimiter_balance(paren_depth, brace_depth, bracket_depth, last_span) {
            return None;
        }

        self.ts
            .push_error(self.ts.eof_span(), "expected 'in' in for-loop header");
        None
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
