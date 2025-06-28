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
    /// ```no_run
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
}

#[cfg(test)]
mod tests {
    //! Tests for `SpanCollector` using the `TokenStream` abstraction.
    //!
    //! These tests validate that the collector exposes its token stream
    //! correctly and that extra state can be retrieved without consuming the
    //! collected spans.
    use super::*;
    use rstest::rstest;

    #[rstest]
    fn new_initialises_state() {
        let src = "import foo";
        let tokens = crate::tokenize(src);
        let collector = SpanCollector::new(&tokens, src, ());
        assert_eq!(collector.stream.cursor(), 0);
        assert_eq!(collector.stream.tokens(), tokens.as_slice());
        assert_eq!(collector.stream.src(), src);
        assert!(collector.spans.is_empty());
    }

    #[test]
    fn into_parts_returns_collected_spans_and_extra() {
        let src = "input";
        let tokens = crate::tokenize(src);
        let mut collector = SpanCollector::new(&tokens, src, 99u8);
        collector.spans.push(0..5);
        let (spans, extra) = collector.into_parts();
        assert_eq!(spans, vec![0..5]);
        assert_eq!(extra, 99);
    }

    #[rstest]
    fn new_initialises_state() {
        let tokens = &[(SyntaxKind::K_IMPORT, 0..6)];
        let src = "import";
        let extra = vec![1, 2, 3];
        let collector = SpanCollector::new(tokens, src, extra.clone());

        assert_eq!(collector.cursor, 0);
        assert!(collector.spans.is_empty());
        assert_eq!(collector.extra, extra);
        assert_eq!(collector.tokens, tokens);
        assert_eq!(collector.src, src);
    }

    #[rstest]
    fn into_parts_returns_components() {
        let tokens = &[(SyntaxKind::K_IMPORT, 0..6)];
        let src = "import";
        let mut collector = SpanCollector::new(tokens, src, 42);
        collector.spans.push(0..6);

        let (spans, extra) = collector.into_parts();

        assert_eq!(spans, vec![0..6]);
        assert_eq!(extra, 42);
    }
}
