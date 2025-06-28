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
