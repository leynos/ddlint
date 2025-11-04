//! Utilities for collecting statement spans.
//!
//! This module provides a generic state structure used by the parser
//! when scanning tokens to record the spans of specific statements.
//! Each collector owns a `SpanCollector` with additional state as
//! required by the parsing logic.

use crate::{Span, SyntaxKind};

use super::token_stream::TokenStream;

/// Common state used when scanning the token stream.
#[derive(Debug)]
pub(crate) struct SpanCollector<'a, Extra> {
    pub(crate) stream: TokenStream<'a>,
    pub(crate) spans: Vec<Span>,
    pub(crate) extra: Extra,
}

impl<'a, Extra> SpanCollector<'a, Extra> {
    /// Constructs a new `SpanCollector` for the given token stream, source string, and extra state.
    ///
    /// # Parameters
    ///
    /// - `tokens`: Slice of token and span pairs to be scanned.
    /// - `src`: The source string corresponding to the tokens.
    /// - `extra`: Additional state required for parsing logic.
    ///
    /// # Returns
    ///
    /// A `SpanCollector` instance ready to collect statement spans during parsing.
    ///
    /// # Examples
    ///
    /// ```rust,ignore
    /// use crate::parser::{SpanCollector, SyntaxKind, Span};
    ///
    /// let tokens: &[(SyntaxKind, Span)] = &[];
    /// let src = "";
    /// let extra = ();
    /// let collector = SpanCollector::new(tokens, src, extra);
    /// ```
    #[must_use]
    pub(crate) fn new(tokens: &'a [(SyntaxKind, Span)], src: &'a str, extra: Extra) -> Self {
        Self {
            stream: TokenStream::new(tokens, src),
            spans: Vec::new(),
            extra,
        }
    }

    /// Split the collector into the recorded spans and extra data.
    #[must_use]
    pub(crate) fn into_parts(self) -> (Vec<Span>, Extra) {
        (self.spans, self.extra)
    }

    /// Record a span starting at `start` up to the end of the current line.
    pub(crate) fn push_line_span(&mut self, start: usize) {
        let end = self.stream.line_end(self.stream.cursor());
        self.stream.skip_until(end);
        self.spans.push(start..end);
    }

    /// Skip tokens up to the end of the current line without recording a span.
    pub(crate) fn skip_line(&mut self) {
        let end = self.stream.line_end(self.stream.cursor());
        self.stream.skip_until(end);
    }

    /// Parse a sub-tree starting at `start` using `parser`.
    pub(crate) fn parse_span<P>(
        &mut self,
        parser: P,
        start: usize,
    ) -> (Option<Span>, Vec<chumsky::error::Simple<SyntaxKind>>)
    where
        P: chumsky::Parser<SyntaxKind, Span, Error = chumsky::error::Simple<SyntaxKind>>,
    {
        use chumsky::Stream;
        let iter = self
            .stream
            .tokens()
            .iter()
            .skip(self.stream.cursor())
            .cloned();
        let sub = Stream::from_iter(start..self.stream.src().len(), iter);
        parser.parse_recovery(sub)
    }
}

#[cfg(test)]
mod tests {
    //! Tests for `SpanCollector` using the `TokenStream` abstraction.
    //!
    //! These tests validate that the collector exposes its token stream
    //! correctly, and that extra state can be retrieved without consuming the
    //! collected spans.
    use super::*;
    use crate::test_util::tokenize;
    use rstest::rstest;

    #[rstest]
    fn new_initialises_state() {
        let src = "import foo";
        let tokens = tokenize(src);
        let collector = SpanCollector::new(&tokens, src, ());
        assert_eq!(collector.stream.cursor(), 0);
        assert_eq!(collector.stream.tokens(), tokens.as_slice());
        assert_eq!(collector.stream.src(), src);
        assert!(collector.spans.is_empty());
    }

    #[test]
    fn into_parts_returns_collected_spans_and_extra() {
        let src = "input";
        let tokens = tokenize(src);
        let mut collector = SpanCollector::new(&tokens, src, 99u8);
        collector.spans.push(0..5);
        let (spans, extra) = collector.into_parts();
        assert_eq!(spans, vec![0..5]);
        assert_eq!(extra, 99);
    }
}
