//! Shared span conversion helpers for linter diagnostics.
//!
//! Semantic analysis stores byte-oriented `Span` values, while diagnostics use
//! `rowan::TextRange`. These helpers keep that boundary conversion in one
//! linter-owned place so rules do not each repeat the same glue.

use rowan::{TextRange, TextSize};

/// Convert a semantic-model `Span` into a diagnostic `TextRange`.
#[must_use]
pub(crate) fn span_to_text_range(span: &crate::Span) -> Option<TextRange> {
    Some(TextRange::new(
        span_offset_to_text_size(span.start)?,
        span_offset_to_text_size(span.end)?,
    ))
}

fn span_offset_to_text_size(offset: usize) -> Option<TextSize> {
    u32::try_from(offset).ok().map(TextSize::from)
}
