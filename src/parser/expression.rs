//! Pratt parser for `DDlog` expressions.
//!
//! The implementation is a small hand-rolled Pratt parser. `chumsky`
//! 0.9 does not provide a built-in Pratt combinator, so this module
//! offers a focused parser that operates over the token stream
//! produced by the lexer. It recognises a subset of operators needed
//! for arithmetic and logical expressions.

use chumsky::error::Simple;

use crate::parser::ast::expr::infix_binding_power;
use crate::parser::ast::{Expr, Literal, UnaryOp};
use crate::{Span, SyntaxKind, tokenize};

/// Parse a source string into an [`Expr`].
#[must_use]
pub fn parse_expression(src: &str) -> (Option<Expr>, Vec<Simple<SyntaxKind>>) {
    let tokens = tokenize(src);
    let mut parser = Pratt::new(&tokens, src);
    let expr = parser.parse_expr(0);
    (expr, parser.errors)
}

struct Pratt<'a> {
    tokens: &'a [(SyntaxKind, Span)],
    src: &'a str,
    pos: usize,
    errors: Vec<Simple<SyntaxKind>>,
}

impl<'a> Pratt<'a> {
    fn new(tokens: &'a [(SyntaxKind, Span)], src: &'a str) -> Self {
        Self {
            tokens,
            src,
            pos: 0,
            errors: Vec::new(),
        }
    }

    fn next(&mut self) -> Option<(SyntaxKind, Span)> {
        while let Some((kind, _)) = self.tokens.get(self.pos) {
            if matches!(kind, SyntaxKind::T_WHITESPACE | SyntaxKind::T_COMMENT) {
                self.pos += 1;
            } else {
                break;
            }
        }
        let tok = self.tokens.get(self.pos).cloned();
        if tok.is_some() {
            self.pos += 1;
        }
        tok
    }

    fn peek(&self) -> Option<SyntaxKind> {
        let mut idx = self.pos;
        while let Some((kind, _)) = self.tokens.get(idx) {
            if matches!(kind, SyntaxKind::T_WHITESPACE | SyntaxKind::T_COMMENT) {
                idx += 1;
            } else {
                return Some(*kind);
            }
        }
        None
    }

    fn parse_expr(&mut self, min_bp: u8) -> Option<Expr> {
        let (lhs_kind, lhs_span) = self.next()?;
        let mut lhs = match lhs_kind {
            SyntaxKind::T_NUMBER => Some(Expr::Literal(Literal::Number(self.slice(lhs_span)))),
            SyntaxKind::T_STRING => Some(Expr::Literal(Literal::String(self.slice(lhs_span)))),
            SyntaxKind::K_TRUE => Some(Expr::Literal(Literal::Bool(true))),
            SyntaxKind::K_FALSE => Some(Expr::Literal(Literal::Bool(false))),
            SyntaxKind::T_IDENT => Some(Expr::Variable(self.slice(lhs_span))),
            SyntaxKind::T_LPAREN => {
                let expr = self.parse_expr(0);
                if !self.expect(SyntaxKind::T_RPAREN) {
                    return None;
                }
                expr.map(|e| Expr::Group(Box::new(e)))
            }
            k => {
                if let Some((_, op)) = prefix_binding_power(k) {
                    let rhs = self.parse_expr(60)?;
                    Some(Expr::Unary {
                        op,
                        expr: Box::new(rhs),
                    })
                } else {
                    self.errors
                        .push(Simple::custom(lhs_span.clone(), "unexpected token"));
                    None
                }
            }
        }?;

        while let Some(op_kind) = self.peek() {
            let Some((l_bp, r_bp, op)) = infix_binding_power(op_kind) else {
                break;
            };
            if l_bp < min_bp {
                break;
            }
            self.next();
            let rhs = self.parse_expr(r_bp)?;
            lhs = Expr::Binary {
                op,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            };
        }
        Some(lhs)
    }

    fn expect(&mut self, kind: SyntaxKind) -> bool {
        if matches!(self.peek(), Some(k) if k == kind) {
            self.next();
            true
        } else {
            let span = self
                .tokens
                .get(self.pos)
                .map(|t| t.1.clone())
                .unwrap_or(self.src.len()..self.src.len());
            self.errors
                .push(Simple::custom(span, format!("expected {kind:?}")));
            false
        }
    }

    fn slice(&self, span: Span) -> String {
        self.src.get(span).unwrap_or("").to_string()
    }
}

fn prefix_binding_power(kind: SyntaxKind) -> Option<(u8, UnaryOp)> {
    use UnaryOp::{Neg, Not};
    match kind {
        SyntaxKind::T_MINUS => Some((60, Neg)),
        SyntaxKind::K_NOT => Some((60, Not)),
        _ => None,
    }
}
