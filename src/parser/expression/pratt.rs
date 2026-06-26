//! Core Pratt parser implementation and entry point.
//!
//! This module defines the [`Pratt`] struct and the public
//! [`parse_expression`] function which tokenizes the source and builds
//! expression trees.

use std::collections::HashMap;

use chumsky::error::Simple;

use crate::parser::ast::{Expr, StringLiteral};
use crate::parser::reserved_tokens::rejection_for;
use crate::{Span, SyntaxKind, tokenize_without_trivia};

use super::token_stream::TokenStream;

mod delay;
mod diff;
mod postfix;

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
        // The guard depth must not underflow; saturate rather than panic if
        // the scopes are ever unbalanced.
        debug_assert!(self.active > 0, "struct literal guard underflow");
        self.active = self.active.saturating_sub(1);
        if self.active == 0 {
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
        // The suspension depth must not underflow; saturate rather than
        // panic if the scopes are ever unbalanced.
        debug_assert!(
            self.suspension > 0,
            "struct literal guard suspension underflow"
        );
        self.suspension = self.suspension.saturating_sub(1);
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
            if let Some(message) = rejection_for(kind) {
                parser.ts.push_reserved_error(sp, message);
            } else {
                parser
                    .ts
                    .push_error(sp, format!("unexpected token: {kind:?}"));
            }
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
        self.with_struct_literal_scope(StructLiteralState::suspend, StructLiteralState::resume, f)
    }

    pub(super) fn with_struct_literal_activation<R>(
        &mut self,
        f: impl FnOnce(&mut Self) -> R,
    ) -> R {
        self.with_struct_literal_scope(
            |state| {
                state.activate();
                true
            },
            StructLiteralState::deactivate,
            f,
        )
    }

    fn with_struct_literal_scope<R>(
        &mut self,
        setup: impl FnOnce(&mut StructLiteralState) -> bool,
        teardown: impl FnOnce(&mut StructLiteralState),
        f: impl FnOnce(&mut Self) -> R,
    ) -> R {
        if !setup(&mut self.struct_literals) {
            return f(self);
        }
        let result = f(self);
        teardown(&mut self.struct_literals);
        result
    }

    fn with_depth_check<R>(&mut self, f: impl FnOnce(&mut Self) -> Option<R>) -> Option<R> {
        if self.expr_depth >= MAX_EXPR_DEPTH {
            let sp = self.ts.peek_span().unwrap_or_else(|| self.ts.eof_span());
            self.ts.push_error(sp, "expression nesting too deep");
            return None;
        }
        self.expr_depth += 1;
        let result = f(self);
        self.expr_depth -= 1;
        result
    }

    pub(super) fn parse_expr(&mut self, min_bp: u8) -> Option<Expr> {
        self.with_depth_check(|this| this.parse_expr_inner(min_bp))
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
        self.with_depth_check(|this| {
            let lhs = this.parse_prefix()?;
            let lhs = this.parse_postfix(lhs)?;
            this.parse_infix_with_colon_mode(lhs, min_bp, true)
        })
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
