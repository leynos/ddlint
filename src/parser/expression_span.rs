//! Utilities for locating and validating expression spans.
//!
//! The functions in this module help other parser components extract the byte
//! ranges of expressions from a token stream. They also allow validating the
//! text within those ranges using the Pratt expression parser without
//! introducing additional coupling.

use chumsky::error::Simple;

use crate::parser::expression::parse_expression;
use crate::{Span, SyntaxKind};

/// Find the span of a rule body expression within a slice of tokens.
///
/// The function scans the tokens starting at `start_idx` until `end`. It
/// returns the range of bytes between the token following `T_IMPLIES` and the
/// preceding `T_DOT`. `None` is returned if a well formed range cannot be
/// determined.
#[must_use]
pub(crate) fn rule_body_span(
    tokens: &[(SyntaxKind, Span)],
    start_idx: usize,
    end: usize,
) -> Option<Span> {
    let mut expr_start = None;
    let mut expr_end = None;
    let mut idx = start_idx;
    while let Some(tok) = tokens.get(idx) {
        if tok.1.start >= end {
            break;
        }
        match tok.0 {
            SyntaxKind::T_IMPLIES => {
                expr_start = tokens.get(idx + 1).map(|t| t.1.start);
            }
            SyntaxKind::T_DOT => {
                expr_end = Some(tok.1.start);
                break;
            }
            _ => {}
        }
        idx += 1;
    }
    match (expr_start, expr_end) {
        (Some(s), Some(e)) if s < e => Some(s..e),
        _ => None,
    }
}

/// Parse the text within `span` using the expression parser.
///
/// Any syntax errors reported by the parser are returned for the caller to
/// aggregate. A successful parse yields `()`.
pub(crate) fn validate_expression(src: &str, span: Span) -> Result<(), Vec<Simple<SyntaxKind>>> {
    src.get(span.clone())
        .map_or_else(|| Ok(()), |text| parse_expression(text).map(|_| ()))
}
