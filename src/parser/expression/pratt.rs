//! Core Pratt parser implementation and entry point.
//!
//! This module defines the [`Pratt`] struct and the public
//! [`parse_expression`] function which tokenizes the source and builds
//! expression trees.

use std::collections::HashMap;

use chumsky::error::Simple;

use crate::parser::ast::{Expr, StringLiteral};
use crate::parser::span_utils::parse_u32_decimal;
use crate::{Span, SyntaxKind, tokenize_without_trivia};

use super::token_stream::TokenStream;

const MAX_EXPR_DEPTH: usize = 256;

pub(super) struct Pratt<'a, I>
where
    I: Iterator<Item = (SyntaxKind, Span)> + Clone,
{
    pub(super) ts: TokenStream<'a, I>,
    /// Tracks recursion depth to reject pathologically nested expressions.
    expr_depth: usize,
    /// Records contexts where bare struct literals should be disallowed or temporarily re-enabled.
    struct_literals: StructLiteralState,
    /// Cache of parsed string literals keyed by start offset to avoid re-parsing.
    pub(super) string_literal_cache: HashMap<usize, StringLiteral>,
}

#[derive(Default)]
struct StructLiteralState {
    /// Active contexts where bare struct literals must be disabled (for example `if` conditions).
    active: usize,
    /// Nesting depth that temporarily suspends the guard for disambiguated sub-expressions.
    suspension: usize,
}

impl StructLiteralState {
    fn activate(&mut self) {
        self.active += 1;
    }

    fn deactivate(&mut self) {
        #[expect(
            clippy::expect_used,
            reason = "struct literal guard depth must not underflow"
        )]
        let remaining = self
            .active
            .checked_sub(1)
            .expect("struct literal guard underflow");
        self.active = remaining;
        if remaining == 0 {
            self.suspension = 0;
        }
    }

    fn suspend(&mut self) -> bool {
        if self.active == 0 {
            return false;
        }
        self.suspension += 1;
        true
    }

    fn resume(&mut self) {
        #[expect(
            clippy::expect_used,
            reason = "struct literal guard suspension depth must not underflow"
        )]
        let remaining = self
            .suspension
            .checked_sub(1)
            .expect("struct literal guard suspension underflow");
        self.suspension = remaining;
    }

    fn allows_struct_literal(&self) -> bool {
        self.active == 0 || self.suspension > 0
    }
}

/// Parse a source string into an [`Expr`].
///
/// This tokenizes the input and runs the Pratt parser, collecting any syntax
/// errors for the caller to handle.
///
/// # Errors
/// Returns a vector of [`Simple`] errors when parsing fails.
#[must_use = "discarding the Result will ignore parse errors"]
pub fn parse_expression(src: &str) -> Result<Expr, Vec<Simple<SyntaxKind>>> {
    let mut parser = Pratt::new(tokenize_without_trivia(src).into_iter(), src);
    let expr = parser.parse_expr(0);
    if let Some(expr_val) = expr {
        for (kind, sp) in parser.ts.drain_unexpected_tokens() {
            parser
                .ts
                .push_error(sp, format!("unexpected token: {kind:?}"));
        }
        if !parser.ts.has_errors() {
            return Ok(expr_val);
        }
    } else if !parser.ts.has_errors() {
        parser
            .ts
            .push_error(parser.ts.eof_span(), "invalid expression");
    }
    Err(parser.ts.take_errors())
}

impl<I> Pratt<'_, I>
where
    I: Iterator<Item = (SyntaxKind, Span)> + Clone,
{
    #[must_use]
    pub(super) fn new(tokens: I, src: &str) -> Pratt<'_, I> {
        Pratt {
            ts: TokenStream::new(tokens, src),
            expr_depth: 0,
            struct_literals: StructLiteralState::default(),
            string_literal_cache: HashMap::new(),
        }
    }

    pub(super) fn allows_struct_literals(&self) -> bool {
        self.struct_literals.allows_struct_literal()
    }

    pub(super) fn with_struct_literals_suspended<R>(
        &mut self,
        f: impl FnOnce(&mut Self) -> R,
    ) -> R {
        if !self.struct_literals.suspend() {
            return f(self);
        }
        let result = f(self);
        self.struct_literals.resume();
        result
    }

    pub(super) fn with_struct_literal_activation<R>(
        &mut self,
        f: impl FnOnce(&mut Self) -> R,
    ) -> R {
        self.struct_literals.activate();
        let result = f(self);
        self.struct_literals.deactivate();
        result
    }

    pub(super) fn parse_expr(&mut self, min_bp: u8) -> Option<Expr> {
        if self.expr_depth >= MAX_EXPR_DEPTH {
            let sp = self.ts.peek_span().unwrap_or_else(|| self.ts.eof_span());
            self.ts.push_error(sp, "expression nesting too deep");
            return None;
        }
        self.expr_depth += 1;
        let result = self.parse_expr_inner(min_bp);
        self.expr_depth -= 1;
        result
    }

    fn parse_expr_inner(&mut self, min_bp: u8) -> Option<Expr> {
        let lhs = self.parse_prefix()?;
        let lhs = self.parse_postfix(lhs)?;
        self.parse_infix(lhs, min_bp)
    }

    /// Parse an expression in map-key mode where `:` terminates parsing.
    ///
    /// This is used for map literal keys where `:` separates key from value
    /// rather than being consumed as type ascription.
    pub(super) fn parse_map_key_expr(&mut self, min_bp: u8) -> Option<Expr> {
        if self.expr_depth >= MAX_EXPR_DEPTH {
            let sp = self.ts.peek_span().unwrap_or_else(|| self.ts.eof_span());
            self.ts.push_error(sp, "expression nesting too deep");
            return None;
        }
        self.expr_depth += 1;
        let lhs = self.parse_prefix()?;
        let lhs = self.parse_postfix(lhs)?;
        let result = self.parse_infix_with_colon_mode(lhs, min_bp, true);
        self.expr_depth -= 1;
        result
    }

    // Parse highest-precedence postfix operators, delegating each operation to a dedicated helper.
    pub(super) fn parse_postfix(&mut self, mut lhs: Expr) -> Option<Expr> {
        let mut pending_diff_span: Option<Span> = None;
        loop {
            match self.ts.peek_kind() {
                Some(SyntaxKind::T_APOSTROPHE) => {
                    self.handle_diff_marker(&mut pending_diff_span)?;
                    continue;
                }
                Some(SyntaxKind::T_LPAREN) => {
                    lhs = self.parse_function_call_postfix(lhs)?;
                }
                Some(SyntaxKind::T_LBRACKET) => {
                    lhs = self.parse_bit_slice_postfix(lhs)?;
                }
                Some(SyntaxKind::T_DOT) => {
                    lhs = self.parse_dot_access_postfix(lhs)?;
                }
                Some(SyntaxKind::T_MINUS) if self.ts.peek_nth_kind(1) == Some(SyntaxKind::T_LT) => {
                    self.validate_diff_not_pending(&mut pending_diff_span);
                    lhs = self.parse_delay_postfix(lhs)?;
                }
                _ => break,
            }

            lhs = Self::apply_pending_diff(lhs, &mut pending_diff_span);
        }

        self.validate_no_pending_diff(&mut pending_diff_span)?;

        Some(lhs)
    }

    fn handle_diff_marker(&mut self, pending: &mut Option<Span>) -> Option<()> {
        let (_, sp) = self.ts.next_tok()?;
        if pending.is_some() {
            self.ts.push_error(sp, "duplicate diff marker");
        } else {
            *pending = Some(sp);
        }

        if !matches!(
            self.ts.peek_kind(),
            Some(SyntaxKind::T_LPAREN | SyntaxKind::T_LBRACKET)
        ) {
            let diff_span = pending.take().unwrap_or_else(|| self.ts.eof_span());
            self.ts
                .push_error(diff_span, "diff marker must precede atom arguments");
            return None;
        }

        Some(())
    }

    fn apply_pending_diff(expr: Expr, pending: &mut Option<Span>) -> Expr {
        if pending.take().is_some() {
            Expr::AtomDiff {
                expr: Box::new(expr),
            }
        } else {
            expr
        }
    }

    fn validate_diff_not_pending(&mut self, pending: &mut Option<Span>) {
        if let Some(diff_span) = pending.take() {
            self.ts
                .push_error(diff_span, "diff marker must apply to an atom");
        }
    }

    fn validate_no_pending_diff(&mut self, pending: &mut Option<Span>) -> Option<()> {
        if let Some(span) = pending.take() {
            self.ts
                .push_error(span, "diff marker must be followed by atom arguments");
            return None;
        }
        Some(())
    }

    fn parse_delay_postfix(&mut self, lhs: Expr) -> Option<Expr> {
        let (minus_kind, _) = self.ts.next_tok()?; // '-'
        debug_assert_eq!(
            minus_kind,
            SyntaxKind::T_MINUS,
            "parse_delay_postfix must be called with a leading '-' token"
        );
        if !self.ts.expect(SyntaxKind::T_LT) {
            return None;
        }
        let (kind, number_span) = self.ts.next_tok().unwrap_or_else(|| {
            let sp = self.ts.eof_span();
            (SyntaxKind::N_ERROR, sp)
        });
        if kind != SyntaxKind::T_NUMBER {
            self.ts.push_error(number_span, "expected delay value");
            return None;
        }
        let raw = self.ts.slice(&number_span);
        let delay = match parse_u32_decimal(&raw) {
            Ok(delay) => delay,
            Err(msg) => {
                self.ts.push_error(number_span, msg);
                return None;
            }
        };
        if !self.ts.expect(SyntaxKind::T_GT) {
            return None;
        }
        Some(Expr::AtomDelay {
            delay,
            expr: Box::new(lhs),
        })
    }

    fn parse_function_call_postfix(&mut self, lhs: Expr) -> Option<Expr> {
        let args = self.parse_parenthesized_args()?;
        Some(Expr::Call {
            callee: Box::new(lhs),
            args,
        })
    }

    fn parse_bit_slice_postfix(&mut self, lhs: Expr) -> Option<Expr> {
        self.ts.next_tok(); // opening bracket already peeked
        self.with_struct_literals_suspended(move |this| {
            let hi = this.parse_expr(0)?;
            if !this.ts.expect(SyntaxKind::T_COMMA) {
                return None;
            }
            let lo = this.parse_expr(0)?;
            if !this.ts.expect(SyntaxKind::T_RBRACKET) {
                return None;
            }
            Some(Expr::BitSlice {
                expr: Box::new(lhs),
                hi: Box::new(hi),
                lo: Box::new(lo),
            })
        })
    }

    fn parse_dot_access_postfix(&mut self, lhs: Expr) -> Option<Expr> {
        self.ts.next_tok(); // '.'
        match self.ts.next_tok() {
            Some((SyntaxKind::T_IDENT, sp)) => {
                let name = self.ts.slice(&sp);
                if matches!(self.ts.peek_kind(), Some(SyntaxKind::T_LPAREN)) {
                    self.parse_method_call(lhs, name)
                } else {
                    Some(Expr::FieldAccess {
                        expr: Box::new(lhs),
                        field: name,
                    })
                }
            }
            Some((SyntaxKind::T_NUMBER, sp)) => {
                let idx = self.ts.slice(&sp);
                Some(Expr::TupleIndex {
                    expr: Box::new(lhs),
                    index: idx,
                })
            }
            other => {
                let sp = other.map_or_else(|| self.ts.eof_span(), |(_, s)| s);
                self.ts
                    .push_error(sp, "expected identifier or tuple index after '.'");
                None
            }
        }
    }

    fn parse_method_call(&mut self, lhs: Expr, name: String) -> Option<Expr> {
        let args = self.parse_parenthesized_args()?;
        Some(Expr::MethodCall {
            recv: Box::new(lhs),
            name,
            args,
        })
    }

    fn parse_parenthesized_args(&mut self) -> Option<Vec<Expr>> {
        self.ts.next_tok(); // opening parenthesis already peeked
        self.with_struct_literals_suspended(|this| {
            let args = this.parse_args()?;
            if !this.ts.expect(SyntaxKind::T_RPAREN) {
                return None;
            }
            Some(args)
        })
    }

    pub(super) fn parse_args(&mut self) -> Option<Vec<Expr>> {
        let mut args = Vec::new();
        if matches!(self.ts.peek_kind(), Some(SyntaxKind::T_RPAREN)) {
            return Some(args);
        }
        loop {
            if self.ts.peek_kind().is_none() {
                self.ts.expect(SyntaxKind::T_RPAREN);
                return None;
            }
            let expr = self.parse_expr(0)?;
            args.push(expr);
            if !matches!(self.ts.peek_kind(), Some(SyntaxKind::T_COMMA)) {
                break;
            }
            self.ts.next_tok();
            if matches!(self.ts.peek_kind(), Some(SyntaxKind::T_RPAREN)) {
                let sp = self.ts.peek_span().unwrap_or_else(|| self.ts.eof_span());
                self.ts
                    .push_error(sp, "unexpected trailing comma in argument list");
                return None;
            }
        }
        Some(args)
    }

    /// Parse a type expression following `:` or `as`.
    ///
    /// The RHS of ascriptions, and casts, should not consume infix operators
    /// belonging to the outer expression. Parsing the type with the highest
    /// possible binding power ensures only atomic type expressions are captured.
    pub(super) fn parse_type(&mut self) -> Option<Expr> {
        let ty = self.parse_expr(u8::MAX)?;
        if matches!(
            self.ts.peek_kind(),
            Some(SyntaxKind::T_COLON | SyntaxKind::K_AS)
        ) {
            let sp = self.ts.peek_span().unwrap_or_else(|| self.ts.eof_span());
            self.ts
                .push_error(sp, "chained type operators are not allowed");
        }
        Some(ty)
    }
}
