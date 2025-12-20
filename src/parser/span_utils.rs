//! Span manipulation helpers shared across parser components.
//!
//! These utilities support shifting parser diagnostics produced against
//! substring slices back into the coordinate space of the full source text.

use chumsky::Error as ChumskyError;
use chumsky::error::{Simple, SimpleReason};

use crate::{Span, SyntaxKind};

/// Shift every error span by `offset`.
pub(crate) fn shift_errors(
    errors: Vec<Simple<SyntaxKind>>,
    offset: usize,
) -> Vec<Simple<SyntaxKind>> {
    errors
        .into_iter()
        .map(|error| shift_error(&error, offset))
        .collect()
}

fn shift_error(error: &Simple<SyntaxKind>, offset: usize) -> Simple<SyntaxKind> {
    let expected: Vec<Option<SyntaxKind>> = error.expected().copied().collect();
    let found = error.found().copied();
    let label = error.label();
    let reason = error.reason().clone();
    let span = shift_span(error.span(), offset);

    let shifted = match reason {
        SimpleReason::Unexpected => Simple::expected_input_found(span, expected, found),
        SimpleReason::Unclosed {
            span: unclosed_span,
            delimiter,
        } => {
            let expected_closer = expected
                .iter()
                .find_map(|token| *token)
                .unwrap_or_else(|| matching_closer(delimiter));
            Simple::unclosed_delimiter(
                shift_span(unclosed_span, offset),
                delimiter,
                span,
                expected_closer,
                found,
            )
        }
        SimpleReason::Custom(msg) => Simple::custom(span, msg),
    };

    match label {
        Some(label) => shifted.with_label(label),
        None => shifted,
    }
}

fn matching_closer(delimiter: SyntaxKind) -> SyntaxKind {
    match delimiter {
        SyntaxKind::T_LPAREN => SyntaxKind::T_RPAREN,
        SyntaxKind::T_LBRACE => SyntaxKind::T_RBRACE,
        SyntaxKind::T_LBRACKET => SyntaxKind::T_RBRACKET,
        other => other,
    }
}

fn shift_span(span: Span, offset: usize) -> Span {
    span.start.saturating_add(offset)..span.end.saturating_add(offset)
}

/// Return the start and end offsets of `text.trim()` as byte indices into `text`.
pub(crate) fn trim_byte_range(text: &str) -> (usize, usize) {
    let start = text
        .char_indices()
        .find(|(_, ch)| !ch.is_whitespace())
        .map_or(text.len(), |(idx, _)| idx);
    let end = text
        .char_indices()
        .rev()
        .find(|(_, ch)| !ch.is_whitespace())
        .map_or(start, |(idx, ch)| idx + ch.len_utf8());
    (start, end)
}
