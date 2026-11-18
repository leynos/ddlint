//! Core Pratt parser implementation and entry point.
//!
//! This module defines the [`Pratt`] struct and the public
//! [`parse_expression`] function which tokenizes the source and builds
//! expression trees.

use std::collections::HashMap;

use chumsky::error::Simple;

use super::token_stream::TokenStream;
use crate::{
    Span,
    SyntaxKind,
    parser::{
        ast::{Expr, StringLiteral},
        reserved_tokens::{complete_reserved_token_errors, rejection_for},
    },
    tokenize_without_trivia,
};

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
    /// When set, the infix loop stops at `;` instead of building
    /// [`crate::parser::ast::BinaryOp::Seq`].
    ///
    /// Block items own their `;` separators. The mode is set while block
    /// items are parsed and restored at every nested delimiter, whose `;`
    /// remains the sequencing operator.
    semicolon_terminates: bool,
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
    const fn activate(&mut self) { self.active += 1; }

    fn deactivate(&mut self) {
        // The guard depth must not underflow; saturate rather than panic if
        // the scopes are ever unbalanced.
        debug_assert!(self.active > 0, "struct literal guard underflow");
        self.active = self.active.saturating_sub(1);
        if self.active == 0 {
            self.suspension = 0;
        }
    }

    /// Suspend an active struct-literal guard, returning `false` when none is
    /// active so the caller knows the guard was not suspended.
    const fn suspend(&mut self) -> bool {
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

    /// Return `true` when a `{` here may open a struct literal, which requires
    /// either no active guard or a suspended one.
    const fn allows_struct_literal(&self) -> bool { self.active == 0 || self.suspension > 0 }
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
    if expr.is_some() {
        for (kind, sp) in parser.ts.drain_unexpected_tokens() {
            if let Some(message) = rejection_for(kind) {
                parser.ts.push_reserved_error(sp, message);
            } else {
                parser
                    .ts
                    .push_error(sp, format!("unexpected token: {kind:?}"));
            }
        }
    } else if !parser.ts.has_errors() {
        parser
            .ts
            .push_error(parser.ts.eof_span(), "invalid expression");
    }
    let mut errors = parser.ts.take_errors();
    complete_reserved_token_errors(&crate::tokenize_with_trivia(src), &mut errors);
    match expr {
        Some(parsed) if errors.is_empty() => Ok(parsed),
        _ => Err(errors),
    }
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
            semicolon_terminates: false,
            string_literal_cache: HashMap::new(),
        }
    }

    pub(super) const fn allows_struct_literals(&self) -> bool {
        self.struct_literals.allows_struct_literal()
    }

    /// Return `true` while `;` terminates infix parsing rather than forming a
    /// sequencing expression.
    pub(super) const fn semicolon_terminates(&self) -> bool { self.semicolon_terminates }

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
        let prefix = self.parse_prefix()?;
        let lhs = self.parse_postfix(prefix)?;
        self.parse_infix(lhs, min_bp)
    }

    /// Run `f` while parsing one block item.
    ///
    /// A block item needs both states at once: `;` terminates the item so a
    /// construct tail cannot swallow the separator, and struct literals stay
    /// suspended because `{ ident :` inside a block starts a nested item
    /// rather than a struct value.
    pub(super) fn with_block_item_scope<R>(&mut self, f: impl FnOnce(&mut Self) -> R) -> R {
        self.with_semicolon_scope(true, |this| this.with_struct_literals_suspended(f))
    }

    /// Run `f` inside a nested delimiter, whose contents are not block items.
    ///
    /// `;` sequences again and struct literals are allowed, so the two pieces
    /// of block state are restored together at the delimiter.
    pub(super) fn with_nested_delimiter_scope<R>(&mut self, f: impl FnOnce(&mut Self) -> R) -> R {
        self.with_semicolon_scope(false, |this| this.with_struct_literals_suspended(f))
    }

    /// Run `f` for a `for`-loop guard, which may contain a struct literal.
    pub(super) fn with_guard_expression_scope<R>(&mut self, f: impl FnOnce(&mut Self) -> R) -> R {
        self.with_semicolon_scope(false, |this| this.with_struct_literal_activation(f))
    }

    fn with_semicolon_scope<R>(&mut self, terminates: bool, f: impl FnOnce(&mut Self) -> R) -> R {
        let previous = std::mem::replace(&mut self.semicolon_terminates, terminates);
        let result = f(self);
        self.semicolon_terminates = previous;
        result
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
