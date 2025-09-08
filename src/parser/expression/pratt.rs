//! Core Pratt parser implementation and entry point.
//!
//! This module defines the [`Pratt`] struct and the public
//! [`parse_expression`] function which tokenises the source and builds
//! expression trees.

use chumsky::error::Simple;

use crate::parser::ast::Expr;
use crate::{Span, SyntaxKind, tokenize_no_trivia};

use super::token_stream::TokenStream;

pub(super) struct Pratt<'a, I>
where
    I: Iterator<Item = (SyntaxKind, Span)>,
{
    pub(super) ts: TokenStream<'a, I>,
}

/// Parse a source string into an [`Expr`].
///
/// This tokenises the input and runs the Pratt parser, collecting any syntax
/// errors for the caller to handle.
///
/// # Errors
/// Returns a vector of [`Simple`] errors when parsing fails.
#[must_use = "discarding the Result will ignore parse errors"]
pub fn parse_expression(src: &str) -> Result<Expr, Vec<Simple<SyntaxKind>>> {
    let mut parser = Pratt::new(tokenize_no_trivia(src).into_iter(), src);
    let expr = parser.parse_expr(0);
    if let Some(expr_val) = expr {
        for (kind, sp) in parser.ts.drain_unexpected_tokens() {
            parser
                .ts
                .push_error(sp, format!("unexpected token: {kind:?}"));
        }
        if parser.ts.errors.is_empty() {
            return Ok(expr_val);
        }
    } else if parser.ts.errors.is_empty() {
        parser
            .ts
            .push_error(parser.ts.eof_span(), "invalid expression");
    }
    Err(parser.ts.errors)
}

impl<I> Pratt<'_, I>
where
    I: Iterator<Item = (SyntaxKind, Span)>,
{
    #[must_use]
    pub(super) fn new(tokens: I, src: &str) -> Pratt<'_, I> {
        Pratt {
            ts: TokenStream::new(tokens, src),
        }
    }

    pub(super) fn parse_expr(&mut self, min_bp: u8) -> Option<Expr> {
        let lhs = self.parse_prefix()?;
        let lhs = self.parse_postfix(lhs)?;
        self.parse_infix(lhs, min_bp)
    }

    // Parse highest-precedence postfix operators, delegating each operation to a dedicated helper.
    pub(super) fn parse_postfix(&mut self, mut lhs: Expr) -> Option<Expr> {
        loop {
            lhs = match self.ts.peek_kind() {
                Some(SyntaxKind::T_LPAREN) => self.parse_function_call_postfix(lhs)?,
                Some(SyntaxKind::T_LBRACKET) => self.parse_bit_slice_postfix(lhs)?,
                Some(SyntaxKind::T_DOT) => self.parse_dot_access_postfix(lhs)?,
                _ => break,
            };
        }
        Some(lhs)
    }

    fn parse_function_call_postfix(&mut self, lhs: Expr) -> Option<Expr> {
        let args = self.parse_parenthesized_args()?;
        Some(Expr::Call {
            callee: Box::new(lhs),
            args,
        })
    }

    fn parse_bit_slice_postfix(&mut self, lhs: Expr) -> Option<Expr> {
        self.ts.next_tok(); // '[' already peeked
        let hi = self.parse_expr(0)?;
        if !self.ts.expect(SyntaxKind::T_COMMA) {
            return None;
        }
        let lo = self.parse_expr(0)?;
        if !self.ts.expect(SyntaxKind::T_RBRACKET) {
            return None;
        }
        Some(Expr::BitSlice {
            expr: Box::new(lhs),
            hi: Box::new(hi),
            lo: Box::new(lo),
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
        self.ts.next_tok(); // '(' already peeked
        let args = self.parse_args()?;
        if !self.ts.expect(SyntaxKind::T_RPAREN) {
            return None;
        }
        Some(args)
    }

    pub(super) fn parse_args(&mut self) -> Option<Vec<Expr>> {
        let mut args = Vec::new();
        if matches!(self.ts.peek_kind(), Some(SyntaxKind::T_RPAREN)) {
            return Some(args);
        }
        loop {
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
    /// The RHS of ascriptions and casts should not consume infix operators
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
