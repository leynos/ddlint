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
    let tokens = tokenize_no_trivia(src);
    let mut parser = Pratt::new(tokens.into_iter(), src);
    let expr = parser.parse_expr(0);
    if let (Some(_), Some(sp)) = (&expr, parser.ts.check_unexpected_token()) {
        parser.ts.push_error(sp, "unexpected token");
    }
    if parser.ts.errors.is_empty() {
        if let Some(expr) = expr {
            return Ok(expr);
        }
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

    // New: highest-precedence postfix operators
    pub(super) fn parse_postfix(&mut self, mut lhs: Expr) -> Option<Expr> {
        loop {
            match self.ts.peek_kind() {
                Some(SyntaxKind::T_LPAREN) => {
                    let _ = self.ts.next_tok();
                    let args = self.parse_args()?;
                    if !self.ts.expect(SyntaxKind::T_RPAREN) {
                        return None;
                    }
                    lhs = Expr::Call {
                        callee: Box::new(lhs),
                        args,
                    };
                }
                Some(SyntaxKind::T_LBRACKET) => {
                    let _ = self.ts.next_tok();
                    let hi = self.parse_expr(0)?;
                    if !self.ts.expect(SyntaxKind::T_COMMA) {
                        return None;
                    }
                    let lo = self.parse_expr(0)?;
                    if !self.ts.expect(SyntaxKind::T_RBRACKET) {
                        return None;
                    }
                    lhs = Expr::BitSlice {
                        expr: Box::new(lhs),
                        hi: Box::new(hi),
                        lo: Box::new(lo),
                    };
                }
                Some(SyntaxKind::T_DOT) => {
                    let _ = self.ts.next_tok();
                    match self.ts.next_tok() {
                        Some((SyntaxKind::T_IDENT, sp)) => {
                            let name = self.ts.slice(&sp);
                            if matches!(self.ts.peek_kind(), Some(SyntaxKind::T_LPAREN)) {
                                let _ = self.ts.next_tok();
                                let args = self.parse_args()?;
                                if !self.ts.expect(SyntaxKind::T_RPAREN) {
                                    return None;
                                }
                                lhs = Expr::MethodCall {
                                    recv: Box::new(lhs),
                                    name,
                                    args,
                                };
                            } else {
                                lhs = Expr::FieldAccess {
                                    expr: Box::new(lhs),
                                    field: name,
                                };
                            }
                        }
                        Some((SyntaxKind::T_NUMBER, sp)) => {
                            let idx = self.ts.slice(&sp);
                            lhs = Expr::TupleIndex {
                                expr: Box::new(lhs),
                                index: idx,
                            };
                        }
                        other => {
                            let sp = other.map_or_else(|| self.ts.eof_span(), |(_, s)| s);
                            self.ts
                                .push_error(sp, "expected identifier or tuple index after '.'");
                            return None;
                        }
                    }
                }
                _ => break,
            }
        }
        Some(lhs)
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
        }
        Some(args)
    }
}
