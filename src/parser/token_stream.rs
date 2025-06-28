//! Token stream utilities.
//!
//! Provides a cursor-based wrapper around a slice of tokens for safer
//! navigation during parsing.
//!
//! ```
//! use ddlint::{tokenize, SyntaxKind};
//! use ddlint::parser::token_stream::TokenStream;
//!
//! let src = "typedef A = string\n";
//! let tokens = tokenize(src);
//! let mut stream = TokenStream::new(&tokens, src);
//! let end = stream.line_end(0);
//! stream.skip_until(end);
//! assert_eq!(stream.cursor(), tokens.len());
//! ```
//!
//! This abstraction removes manual index manipulation and provides helper
//! methods used by the parser's recovery routines.
use crate::{Span, SyntaxKind};

#[derive(Debug)]
pub(crate) struct TokenStream<'a> {
    tokens: &'a [(SyntaxKind, Span)],
    src: &'a str,
    cursor: usize,
}

impl<'a> TokenStream<'a> {
    /// Create a new stream over `tokens`.
    #[must_use]
    pub(crate) fn new(tokens: &'a [(SyntaxKind, Span)], src: &'a str) -> Self {
        Self {
            tokens,
            src,
            cursor: 0,
        }
    }

    /// Current cursor position.
    #[must_use]
    pub(crate) fn cursor(&self) -> usize {
        self.cursor
    }

    /// Peek at the token under the cursor.
    #[must_use]
    pub(crate) fn peek(&self) -> Option<&(SyntaxKind, Span)> {
        self.tokens.get(self.cursor)
    }

    /// Advance the cursor by one token.
    pub(crate) fn advance(&mut self) {
        if self.cursor < self.tokens.len() {
            self.cursor += 1;
        }
    }

    /// Access the underlying token slice.
    #[must_use]
    pub(crate) fn tokens(&self) -> &[(SyntaxKind, Span)] {
        self.tokens
    }

    /// Access the source text.
    #[must_use]
    pub(crate) fn src(&self) -> &str {
        self.src
    }

    /// Advance past tokens whose span ends before or at `end`.
    pub(crate) fn skip_until(&mut self, end: usize) {
        while let Some(span) = self.tokens.get(self.cursor).map(|t| &t.1) {
            if span.end <= end {
                self.cursor += 1;
            } else {
                break;
            }
        }
    }

    /// Return the position one past the newline after `start` or the source length.
    #[must_use]
    pub(crate) fn line_end(&self, start: usize) -> usize {
        let mut end = self.tokens.get(start).map_or(self.src.len(), |t| t.1.end);
        for tok in self.tokens.iter().skip(start) {
            end = tok.1.end;
            if self
                .src
                .get(tok.1.clone())
                .is_some_and(|text| text.contains('\n'))
            {
                break;
            }
        }
        end
    }

    /// Skip whitespace and comments that do not contain newlines.
    pub(crate) fn skip_ws_inline(&mut self) {
        while let Some(tok) = self.tokens.get(self.cursor) {
            if matches!(tok.0, SyntaxKind::T_WHITESPACE | SyntaxKind::T_COMMENT)
                && !self
                    .src
                    .get(tok.1.clone())
                    .is_some_and(|text| text.contains('\n'))
            {
                self.cursor += 1;
                continue;
            }
            break;
        }
    }
}
