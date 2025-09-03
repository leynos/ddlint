//! Core Pratt parser implementation and entry point.
//!
//! This module defines the [`Pratt`] struct and the public
//! [`parse_expression`] function which tokenises the source and builds
//! expression trees.

use std::iter::Peekable;

use chumsky::error::Simple;

use crate::parser::ast::Expr;
use crate::{Span, SyntaxKind, tokenize};

pub(super) struct Pratt<'a, I>
where
    I: Iterator<Item = (SyntaxKind, Span)>,
{
    pub(super) tokens: Peekable<I>,
    pub(super) src: &'a str,
    pub(super) errors: Vec<Simple<SyntaxKind>>,
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
    let tokens = tokenize(src);
    let iter = tokens
        .iter()
        .filter(|(k, _)| !matches!(k, SyntaxKind::T_WHITESPACE | SyntaxKind::T_COMMENT))
        .cloned();
    let mut parser = Pratt::new(iter, src);
    let expr = parser.parse_expr(0);
    if let (Some(_), Some(sp)) = (&expr, parser.check_unexpected_token()) {
        parser.push_error(sp, "unexpected token");
    }
    if parser.errors.is_empty() {
        if let Some(expr) = expr {
            return Ok(expr);
        }
        parser.push_error(src.len()..src.len(), "invalid expression");
    }
    Err(parser.errors)
}

impl<I> Pratt<'_, I>
where
    I: Iterator<Item = (SyntaxKind, Span)>,
{
    #[must_use]
    pub(super) fn new(tokens: I, src: &str) -> Pratt<'_, I> {
        let tokens = tokens.peekable();
        Pratt {
            tokens,
            src,
            errors: Vec::new(),
        }
    }

    pub(super) fn push_error(&mut self, span: Span, msg: impl Into<String>) {
        self.errors.push(Simple::custom(span, msg.into()));
    }

    pub(super) fn next(&mut self) -> Option<(SyntaxKind, Span)> {
        self.tokens.next()
    }

    pub(super) fn peek(&mut self) -> Option<SyntaxKind> {
        self.tokens.peek().map(|(k, _)| *k)
    }

    pub(super) fn check_unexpected_token(&mut self) -> Option<Span> {
        if self.peek().is_some() {
            self.next().map(|(_, sp)| sp)
        } else {
            None
        }
    }

    pub(super) fn parse_expr(&mut self, min_bp: u8) -> Option<Expr> {
        let mut lhs = self.parse_prefix()?;

        while matches!(self.peek(), Some(SyntaxKind::T_LPAREN)) {
            let Some((_, lparen_span)) = self.next() else {
                unreachable!("T_LPAREN was peeked");
            };
            let args = self.parse_args()?;
            if !self.expect(SyntaxKind::T_RPAREN) {
                return None;
            }
            let Expr::Variable(name) = lhs else {
                self.push_error(lparen_span, "call target must be identifier");
                return None;
            };
            lhs = Expr::Call { name, args };
        }

        self.parse_infix(lhs, min_bp)
    }

    pub(super) fn parse_args(&mut self) -> Option<Vec<Expr>> {
        let mut args = Vec::new();
        if matches!(self.peek(), Some(SyntaxKind::T_RPAREN)) {
            return Some(args);
        }
        loop {
            let expr = self.parse_expr(0)?;
            args.push(expr);
            if !matches!(self.peek(), Some(SyntaxKind::T_COMMA)) {
                break;
            }
            self.next();
        }
        Some(args)
    }

    pub(super) fn expect(&mut self, kind: SyntaxKind) -> bool {
        if matches!(self.peek(), Some(k) if k == kind) {
            self.next();
            true
        } else {
            let span = self
                .tokens
                .peek()
                .map(|t| t.1.clone())
                .unwrap_or(self.src.len()..self.src.len());
            self.push_error(span, format!("expected {kind:?}"));
            false
        }
    }

    pub(super) fn slice(&self, span: &Span) -> String {
        self.src
            .get(span.clone())
            .unwrap_or_else(|| panic!("lexer produced invalid span"))
            .to_string()
    }
}
