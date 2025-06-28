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
    /// Create a new collector over `tokens`.
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
