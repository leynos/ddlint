//! Token stream utilities.
//!
//! Provides a cursor-based wrapper around a slice of tokens for safer
//! navigation during parsing.
//!
//! ```
//! use ddlint::{tokenize_with_trivia, SyntaxKind};
//! use ddlint::parser::token_stream::TokenStream;
//!
//! let src = "typedef A = string\n";
//! let tokens = tokenize_with_trivia(src);
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
    /// Constructs a new `TokenStream` over the provided tokens and source text.
    ///
    /// The stream starts with the cursor at the beginning of the token slice.
    ///
    /// # Examples
    ///
    /// ```no_run
    /// let stream = TokenStream::new(&tokens, src);
    /// assert_eq!(stream.cursor(), 0);
    /// ```
    #[must_use]
    pub(crate) fn new(tokens: &'a [(SyntaxKind, Span)], src: &'a str) -> Self {
        Self {
            tokens,
            src,
            cursor: 0,
        }
    }

    /// Returns the current cursor position within the token stream.
    ///
    /// # Examples
    ///
    /// ```no_run
    /// let stream = TokenStream::new(tokens, src);
    /// let pos = stream.cursor();
    /// assert_eq!(pos, 0);
    /// ```
    #[must_use]
    pub(crate) fn cursor(&self) -> usize {
        self.cursor
    }

    /// Returns the token at the current cursor position, if any.
    ///
    /// Returns `None` if the cursor is at or beyond the end of the token stream.
    ///
    /// # Examples
    ///
    /// ```no_run
    /// let stream = TokenStream::new(&tokens, src);
    /// if let Some((kind, span)) = stream.peek() {
    ///     // Inspect the current token
    /// }
    /// ```
    #[must_use]
    pub(crate) fn peek(&self) -> Option<&(SyntaxKind, Span)> {
        self.tokens.get(self.cursor)
    }

    /// Moves the cursor forward by one token if not already at the end of the token stream.
    ///
    /// # Examples
    ///
    /// ```no_run
    /// let mut stream = TokenStream::new(tokens, src);
    /// stream.advance();
    /// assert_eq!(stream.cursor(), 1);
    /// ```
    pub(crate) fn advance(&mut self) {
        if self.cursor < self.tokens.len() {
            self.cursor += 1;
        }
    }

    /// Returns a reference to the underlying slice of tokens.
    ///
    /// # Examples
    ///
    /// ```no_run
    /// let stream = TokenStream::new(&tokens, src);
    /// let all_tokens = stream.tokens();
    /// assert_eq!(all_tokens.len(), tokens.len());
    /// ```
    #[must_use]
    pub(crate) fn tokens(&self) -> &[(SyntaxKind, Span)] {
        self.tokens
    }

    /// Returns a reference to the source text associated with this token stream.
    ///
    /// # Examples
    ///
    /// ```no_run
    /// let stream = TokenStream::new(tokens, "let x = 1;");
    /// assert_eq!(stream.src(), "let x = 1;");
    /// ```
    #[must_use]
    pub(crate) fn src(&self) -> &str {
        self.src
    }

    /// Advances the cursor past all tokens whose span ends at or before the specified position.
    ///
    /// Tokens are skipped until a token is found whose span end is greater than `end`, or until no tokens remain.
    ///
    /// # Examples
    ///
    /// ```no_run
    /// use parser::token_stream::TokenStream;
    /// // Assume tokens is a Vec<(SyntaxKind, Span)> and src is the source string.
    /// let mut stream = TokenStream::new(&tokens, src);
    /// stream.skip_until(42);
    /// // The cursor now points to the first token whose span ends after position 42.
    /// ```
    pub(crate) fn skip_until(&mut self, end: usize) {
        while let Some(span) = self.tokens.get(self.cursor).map(|t| &t.1) {
            if span.end <= end {
                self.cursor += 1;
            } else {
                break;
            }
        }
    }

    /// Returns the position immediately after the next newline character following the token at `start`, or the end of the source if no newline is found.
    ///
    /// Iterates through tokens starting at the given index, updating the end position to each token's span end. Stops at the first token whose span contains a newline character, or returns the source length if no such token exists.
    ///
    /// # Examples
    ///
    /// ```no_run
    /// let tokens = lex("foo\nbar");
    /// let stream = TokenStream::new(&tokens, "foo\nbar");
    /// let pos = stream.line_end(0);
    /// assert_eq!(pos, 4); // position after '\n'
    /// ```
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

    /// Advances the cursor past whitespace and comment tokens that do not contain newlines.
    ///
    /// Skips over consecutive whitespace or comment tokens as long as their spans do not
    /// include a newline character. Stops at the first token that is not whitespace/comment
    /// or contains a newline.
    ///
    /// # Examples
    ///
    /// ```no_run
    /// use parser::{TokenStream, SyntaxKind, Span};
    ///
    /// let src = "let x = 42; // comment";
    /// let tokens = vec![
    ///     (SyntaxKind::T_WHITESPACE, Span::new(0, 1)),
    ///     (SyntaxKind::T_COMMENT, Span::new(10, 20)),
    ///     (SyntaxKind::T_IDENT, Span::new(21, 22)),
    /// ];
    /// let mut stream = TokenStream::new(&tokens, src);
    /// stream.skip_ws_inline();
    /// assert_eq!(stream.cursor(), 2);
    /// ```
    pub(crate) fn skip_ws_inline(&mut self) {
        while let Some(tok) = self.tokens.get(self.cursor) {
            if !matches!(tok.0, SyntaxKind::T_WHITESPACE | SyntaxKind::T_COMMENT)
                || self
                    .src
                    .get(tok.1.clone())
                    .is_some_and(|text| text.contains('\n'))
            {
                break;
            }
            self.cursor += 1;
        }
    }

    /// Peeks ahead to the next non-whitespace token on the same line.
    ///
    /// Skips inline whitespace and comments without modifying the cursor and
    /// returns the first subsequent token. Tokens containing a newline end the
    /// search.
    #[must_use]
    pub(crate) fn peek_after_ws_inline(&self) -> Option<&(SyntaxKind, Span)> {
        let mut idx = self.cursor + 1;
        while let Some(tok) = self.tokens.get(idx) {
            if matches!(tok.0, SyntaxKind::T_WHITESPACE | SyntaxKind::T_COMMENT)
                && self
                    .src
                    .get(tok.1.clone())
                    .is_some_and(|text| !text.contains('\n'))
            {
                idx += 1;
            } else {
                break;
            }
        }
        self.tokens.get(idx)
    }
}
