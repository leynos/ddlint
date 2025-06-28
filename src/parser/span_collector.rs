//! Utilities for collecting statement spans.
//!
//! This module provides a generic state structure used by the parser
//! when scanning tokens to record the spans of specific statements.
//! Each collector owns a `SpanCollector` with additional state as
//! required by the parsing logic.

use crate::{Span, SyntaxKind};

/// Common state used when scanning the token stream.
#[derive(Debug)]
pub(crate) struct SpanCollector<'a, Extra> {
    pub(crate) cursor: usize,
    pub(crate) spans: Vec<Span>,
    pub(crate) extra: Extra,
    pub(crate) tokens: &'a [(SyntaxKind, Span)],
    pub(crate) src: &'a str,
}

impl<'a, Extra> SpanCollector<'a, Extra> {
    /// Create a new collector over `tokens`.
    #[must_use]
    pub(crate) fn new(tokens: &'a [(SyntaxKind, Span)], src: &'a str, extra: Extra) -> Self {
        Self {
            cursor: 0,
            spans: Vec::new(),
            extra,
            tokens,
            src,
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
    use super::*;
    use rstest::rstest;

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
