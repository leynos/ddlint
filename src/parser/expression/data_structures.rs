//! Parsing helpers for identifiers, struct literals, closures, groupings, and
//! collection literals.

use crate::parser::ast::Expr;
use crate::{Span, SyntaxKind};

use super::pratt::Pratt;

impl<I> Pratt<'_, I>
where
    I: Iterator<Item = (SyntaxKind, Span)> + Clone,
{
    pub(super) fn parse_identifier_or_struct(&mut self, span: &Span) -> Option<Expr> {
        let name = self.ts.slice(span);
        self.parse_ident_expression(name, span)
    }

    pub(super) fn parse_ident_expression(&mut self, name: String, span: &Span) -> Option<Expr> {
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

    pub(super) fn parse_parenthesized_expr(&mut self) -> Option<Expr> {
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

    /// Parse a vector literal `[e1, e2, ...]`.
    ///
    /// Supports trailing commas and empty vectors. The opening bracket has
    /// already been consumed by the prefix dispatcher.
    pub(super) fn parse_vector_literal(&mut self) -> Option<Expr> {
        if matches!(self.ts.peek_kind(), Some(SyntaxKind::T_RBRACKET)) {
            self.ts.next_tok();
            return Some(Expr::VecLit(Vec::new()));
        }

        self.with_struct_literals_suspended(|this| {
            let items = this.parse_comma_separated_expressions(SyntaxKind::T_RBRACKET)?;
            if !this.ts.expect(SyntaxKind::T_RBRACKET) {
                return None;
            }
            Some(Expr::VecLit(items))
        })
    }

    /// Parse either a map literal `{k: v, ...}` or a brace group `{ expr }`.
    ///
    /// Disambiguation:
    /// - `{}` → empty map
    /// - `{ expr }` → brace group (backward compatible)
    /// - `{ expr: expr, ... }` → map literal
    ///
    /// The opening brace has already been consumed by the prefix dispatcher.
    ///
    /// For map literal keys, we parse with a binding power that prevents the `:`
    /// operator from being consumed as type ascription. The `:` operator has
    /// binding power 50, so we use 51 for keys.
    pub(super) fn parse_brace_or_map_literal(&mut self) -> Option<Expr> {
        // Binding power just above `:` (50) to prevent type ascription
        const MAP_KEY_BP: u8 = 51;

        // Handle empty map `{}`
        if matches!(self.ts.peek_kind(), Some(SyntaxKind::T_RBRACE)) {
            self.ts.next_tok();
            return Some(Expr::MapLit(Vec::new()));
        }

        self.with_struct_literals_suspended(|this| {
            // Parse first key/expression with high BP to stop before `:`
            let first_key = this.parse_expr(MAP_KEY_BP)?;

            match this.ts.peek_kind() {
                // Colon after first expression → map literal
                Some(SyntaxKind::T_COLON) => this.parse_map_entries(first_key, MAP_KEY_BP),

                // Single expression followed by `}` → brace group
                Some(SyntaxKind::T_RBRACE) => {
                    this.ts.next_tok();
                    Some(Expr::Group(Box::new(first_key)))
                }

                // Comma without colon → error (looks like malformed map literal)
                Some(SyntaxKind::T_COMMA) => {
                    let sp = this.ts.peek_span().unwrap_or_else(|| this.ts.eof_span());
                    this.ts
                        .push_error(sp, "expected ':' for map literal or '}' for brace group");
                    None
                }

                // Some operator after the first expression → continue as brace group
                _ => this.parse_brace_group_continuation(first_key),
            }
        })
    }

    /// Parse remaining map entries after detecting a colon following the first key.
    ///
    /// The first key has already been parsed. The caller has peeked a colon but
    /// not yet consumed it.
    fn parse_map_entries(&mut self, first_key: Expr, key_bp: u8) -> Option<Expr> {
        self.ts.next_tok(); // consume ':'
        let first_value = self.parse_expr(0)?;
        let mut entries = vec![(first_key, first_value)];

        while matches!(self.ts.peek_kind(), Some(SyntaxKind::T_COMMA)) {
            self.ts.next_tok();

            // Handle trailing comma
            if matches!(self.ts.peek_kind(), Some(SyntaxKind::T_RBRACE)) {
                break;
            }

            // Parse key with high BP to stop before `:`
            let key = self.parse_expr(key_bp)?;
            if !self.ts.expect(SyntaxKind::T_COLON) {
                return None;
            }
            let value = self.parse_expr(0)?;
            entries.push((key, value));
        }

        if !self.ts.expect(SyntaxKind::T_RBRACE) {
            return None;
        }
        Some(Expr::MapLit(entries))
    }

    /// Continue parsing a brace group when there are infix operators remaining.
    ///
    /// We parsed the first expression with high binding power (to stop before
    /// `:`), so there may be more operators to process before the closing brace.
    fn parse_brace_group_continuation(&mut self, first_expr: Expr) -> Option<Expr> {
        let full_expr = self.parse_infix(first_expr, 0)?;
        if !self.ts.expect(SyntaxKind::T_RBRACE) {
            return None;
        }
        Some(Expr::Group(Box::new(full_expr)))
    }

    /// Parse a comma-separated list of expressions up to (but not consuming)
    /// `terminator`. Supports trailing commas.
    fn parse_comma_separated_expressions(&mut self, terminator: SyntaxKind) -> Option<Vec<Expr>> {
        let mut items = Vec::new();

        if matches!(self.ts.peek_kind(), Some(k) if k == terminator) {
            return Some(items);
        }

        loop {
            let expr = self.parse_expr(0)?;
            items.push(expr);

            if !matches!(self.ts.peek_kind(), Some(SyntaxKind::T_COMMA)) {
                break;
            }
            self.ts.next_tok();

            // Handle trailing comma
            if matches!(self.ts.peek_kind(), Some(k) if k == terminator) {
                break;
            }
        }

        Some(items)
    }

    pub(super) fn parse_closure_literal(&mut self) -> Option<Expr> {
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

    fn parse_closure_params(&mut self) -> Option<Vec<String>> {
        self.parse_comma_separated_identifiers(
            SyntaxKind::T_PIPE,
            "expected parameter name",
            |_parser, name| Some(name),
            true,
        )
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
}
