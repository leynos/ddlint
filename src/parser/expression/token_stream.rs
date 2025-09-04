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
    pub(super) errors: Vec<Simple<SyntaxKind>>,
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

    pub(super) fn check_unexpected_token(&mut self) -> Option<Span> {
        if self.peek_kind().is_some() {
            self.next_tok().map(|(_, sp)| sp)
        } else {
            None
        }
    }

    pub(super) fn push_error(&mut self, span: Span, msg: impl Into<String>) {
        self.errors.push(Simple::custom(span, msg.into()));
    }

    pub(super) fn slice(&self, span: &Span) -> String {
        self.src
            .get(span.clone())
            .unwrap_or_else(|| panic!("lexer produced invalid span"))
            .to_string()
    }

    pub(super) fn eof_span(&self) -> Span {
        self.src.len()..self.src.len()
    }
}
