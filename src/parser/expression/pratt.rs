//! Core Pratt parser implementation and entry point.
//!
//! This module defines the [`Pratt`] struct and the public
//! [`parse_expression`] function which tokenises the source and builds
//! expression trees.

use std::{cell::Cell, rc::Rc};

use chumsky::error::Simple;

use crate::parser::ast::Expr;
use crate::{Span, SyntaxKind, tokenize_without_trivia};

use super::token_stream::TokenStream;

pub(super) struct Pratt<'a, I>
where
    I: Iterator<Item = (SyntaxKind, Span)>,
{
    pub(super) ts: TokenStream<'a, I>,
    expr_depth: Rc<Cell<usize>>,
    struct_literal_guard: Rc<StructLiteralGuard>,
}

#[derive(Default)]
pub(super) struct StructLiteralGuard {
    /// Active contexts where bare struct literals must be disabled (for example `if` conditions).
    active: Cell<usize>,
    /// Nesting depth that temporarily suspends the guard for disambiguated sub-expressions.
    suspension: Cell<usize>,
}

impl StructLiteralGuard {
    pub(super) fn activate(&self) {
        self.active.set(self.active.get() + 1);
    }

    pub(super) fn deactivate(&self) {
        #[expect(
            clippy::expect_used,
            reason = "struct literal guard depth must not underflow"
        )]
        let remaining = self
            .active
            .get()
            .checked_sub(1)
            .expect("struct literal guard underflow");
        self.active.set(remaining);
        if remaining == 0 {
            self.suspension.set(0);
        }
    }

    pub(super) fn suspend(&self) -> bool {
        if self.active.get() == 0 {
            return false;
        }
        self.suspension.set(self.suspension.get() + 1);
        true
    }

    pub(super) fn resume(&self) {
        #[expect(
            clippy::expect_used,
            reason = "struct literal guard suspension depth must not underflow"
        )]
        let remaining = self
            .suspension
            .get()
            .checked_sub(1)
            .expect("struct literal guard suspension underflow");
        self.suspension.set(remaining);
    }

    pub(super) fn allows_struct_literal(&self) -> bool {
        self.active.get() == 0 || self.suspension.get() > 0
    }
}

#[must_use = "bind this guard to keep struct-literal suspension active for the scope"]
pub(super) struct SuspensionGuard {
    guard: Rc<StructLiteralGuard>,
    active: bool,
}

impl SuspensionGuard {
    fn new(guard: Rc<StructLiteralGuard>) -> Self {
        let active = guard.suspend();
        Self { guard, active }
    }
}

impl Drop for SuspensionGuard {
    fn drop(&mut self) {
        if self.active {
            self.guard.resume();
        }
    }
}

#[must_use = "bind this guard to keep struct-literal activation active for the scope"]
pub(super) struct ActivationGuard {
    guard: Rc<StructLiteralGuard>,
}

impl ActivationGuard {
    fn new(guard: Rc<StructLiteralGuard>) -> Self {
        guard.activate();
        Self { guard }
    }
}

impl Drop for ActivationGuard {
    fn drop(&mut self) {
        self.guard.deactivate();
    }
}

struct ExprDepthGuard {
    depth: Rc<Cell<usize>>,
}

impl ExprDepthGuard {
    fn new(depth: Rc<Cell<usize>>) -> Self {
        depth.set(depth.get() + 1);
        Self { depth }
    }
}

impl Drop for ExprDepthGuard {
    fn drop(&mut self) {
        let current = self.depth.get();
        #[expect(
            clippy::expect_used,
            reason = "expression depth guard must not underflow"
        )]
        self.depth
            .set(current.checked_sub(1).expect("expression depth underflow"));
    }
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
    let mut parser = Pratt::new(tokenize_without_trivia(src).into_iter(), src);
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
            expr_depth: Rc::new(Cell::new(0)),
            struct_literal_guard: Rc::new(StructLiteralGuard::default()),
        }
    }

    pub(super) fn struct_guard(&self) -> &StructLiteralGuard {
        self.struct_literal_guard.as_ref()
    }

    #[must_use = "bind the guard to keep struct-literal suspension active for the scope"]
    pub(super) fn suspend_struct_literals(&self) -> SuspensionGuard {
        SuspensionGuard::new(Rc::clone(&self.struct_literal_guard))
    }

    #[must_use = "bind the guard to keep struct-literal activation active for the scope"]
    pub(super) fn activate_struct_literal_guard(&self) -> ActivationGuard {
        ActivationGuard::new(Rc::clone(&self.struct_literal_guard))
    }

    pub(super) fn parse_expr(&mut self, min_bp: u8) -> Option<Expr> {
        let _depth_guard = ExprDepthGuard::new(Rc::clone(&self.expr_depth));
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
        let _guard = self.suspend_struct_literals();
        (|| {
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
        })()
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
        let _guard = self.suspend_struct_literals();
        (|| {
            let args = self.parse_args()?;
            if !self.ts.expect(SyntaxKind::T_RPAREN) {
                return None;
            }
            Some(args)
        })()
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
