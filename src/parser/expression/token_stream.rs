//! Minimal token stream with error tracking for the Pratt parser.
//!
//! Provides lookahead, expectation, and span slicing helpers over an iterator
//! of lexical tokens.

use std::iter::Peekable;

use chumsky::error::Simple;

use crate::{Span, SyntaxKind};

pub(super) struct TokenStream<'a, I>
where
    I: Iterator<Item = (SyntaxKind, Span)>,
{
    iter: Peekable<I>,
    src: &'a str,
    errors: Vec<Simple<SyntaxKind>>,
}

impl<'a, I> TokenStream<'a, I>
where
    I: Iterator<Item = (SyntaxKind, Span)>,
{
    pub(super) fn new(iter: I, src: &'a str) -> Self {
        Self {
            iter: iter.peekable(),
            src,
            errors: Vec::new(),
        }
    }

    pub(super) fn next_tok(&mut self) -> Option<(SyntaxKind, Span)> {
        self.iter.next()
    }

    pub(super) fn peek_kind(&mut self) -> Option<SyntaxKind> {
        self.iter.peek().map(|(k, _)| *k)
    }

    pub(super) fn peek_span(&mut self) -> Option<Span> {
        self.iter.peek().map(|(_, sp)| sp.clone())
    }

    /// Performs an O(n) traversal by cloning the iterator and using `nth`.
    ///
    /// Avoid large lookaheads on hot paths; prefer caching or specialized
    /// peekers for repeated deep inspection.
    pub(super) fn peek_nth_kind(&mut self, n: usize) -> Option<SyntaxKind>
    where
        I: Clone,
    {
        let mut iter = self.iter.clone();
        iter.nth(n).map(|(kind, _)| kind)
    }

    pub(super) fn expect(&mut self, kind: SyntaxKind) -> bool {
        if self.peek_kind() == Some(kind) {
            self.next_tok();
            true
        } else {
            let span = self.peek_span().unwrap_or_else(|| self.eof_span());
            self.push_error(span, format!("expected {kind:?}"));
            false
        }
    }

    /// Drain and return any remaining tokens after a parsed expression.
    pub(super) fn drain_unexpected_tokens(&mut self) -> Vec<(SyntaxKind, Span)> {
        let mut rest = Vec::new();
        while let Some(tok) = self.next_tok() {
            rest.push(tok);
        }
        rest
    }

    pub(super) fn error_count(&self) -> usize {
        self.errors.len()
    }

    pub(super) fn has_errors(&self) -> bool {
        self.error_count() > 0
    }

    pub(super) fn take_errors(&mut self) -> Vec<Simple<SyntaxKind>> {
        std::mem::take(&mut self.errors)
    }

    pub(super) fn extend_errors(&mut self, errors: Vec<Simple<SyntaxKind>>) {
        self.errors.extend(errors);
    }

    pub(super) fn push_error(&mut self, span: Span, msg: impl Into<String>) {
        self.errors.push(Simple::custom(span, msg.into()));
    }

    pub(super) fn slice(&self, span: &Span) -> String {
        debug_assert!(span.end <= self.src.len(), "lexer produced invalid span");
        self.src.get(span.clone()).unwrap_or("").to_string()
    }

    pub(super) fn eof_span(&self) -> Span {
        self.src.len()..self.src.len()
    }

    pub(super) fn src(&self) -> &'a str {
        self.src
    }
}
