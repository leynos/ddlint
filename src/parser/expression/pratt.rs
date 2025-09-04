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
        let mut lhs = self.parse_prefix()?;

        while matches!(self.ts.peek_kind(), Some(SyntaxKind::T_LPAREN)) {
            let Some((_, lparen_span)) = self.ts.next_tok() else {
                unreachable!("T_LPAREN was peeked");
            };
            let args = self.parse_args()?;
            if !self.ts.expect(SyntaxKind::T_RPAREN) {
                return None;
            }
            let Expr::Variable(name) = lhs else {
                self.ts
                    .push_error(lparen_span, "call target must be identifier");
                return None;
            };
            lhs = Expr::Call { name, args };
        }

        self.parse_infix(lhs, min_bp)
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
