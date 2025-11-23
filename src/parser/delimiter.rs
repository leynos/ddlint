//! Shared helpers for delimiter depth tracking across parser utilities.
//!
//! These helpers centralise the logic for keeping delimiter depth in sync with
//! the token stream and for detecting top-level assignment operators. Keeping
//! the behaviour here avoids divergence between rule parsing and expression
//! validation when the delimiter set changes.

use crate::{Span, SyntaxKind, tokenize_without_trivia};

/// Tracks nested delimiter depth while scanning tokens.
#[derive(Default, Debug, Clone, Copy)]
pub(crate) struct DelimiterDepths {
    paren: usize,
    brace: usize,
    bracket: usize,
    angle: usize,
}

impl DelimiterDepths {
    pub(crate) fn apply(&mut self, kind: SyntaxKind) {
        match kind {
            SyntaxKind::T_LPAREN => self.paren += 1,
            SyntaxKind::T_RPAREN => self.paren = self.paren.saturating_sub(1),
            SyntaxKind::T_LBRACE => self.brace += 1,
            SyntaxKind::T_RBRACE => self.brace = self.brace.saturating_sub(1),
            SyntaxKind::T_LBRACKET => self.bracket += 1,
            SyntaxKind::T_RBRACKET => self.bracket = self.bracket.saturating_sub(1),
            SyntaxKind::T_LT => self.angle += 1,
            SyntaxKind::T_GT => self.angle = self.angle.saturating_sub(1),
            _ => {}
        }
    }

    #[must_use]
    pub(crate) fn is_top_level(&self) -> bool {
        self.paren == 0 && self.brace == 0 && self.bracket == 0 && self.angle == 0
    }
}

/// Return the span of a top-level `=` token if present.
#[must_use]
pub(crate) fn find_top_level_eq_span(raw: &str) -> Option<Span> {
    let mut depths = DelimiterDepths::default();
    for (kind, span) in tokenize_without_trivia(raw) {
        depths.apply(kind);
        if depths.is_top_level() && kind == SyntaxKind::T_EQ {
            return Some(span);
        }
    }
    None
}
