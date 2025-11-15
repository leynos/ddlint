//! Utilities for locating and validating expression spans.
//!
//! The functions in this module help other parser components extract the byte
//! ranges of expressions from a token stream. They also allow validating the
//! text within those ranges using the Pratt expression parser without
//! introducing additional coupling.

use chumsky::error::Simple;
use thiserror::Error;

use crate::parser::expression::parse_expression;
use crate::{Span, SyntaxKind};

/// Parse the text within `span` using the expression parser.
///
/// Any syntax errors reported by the parser are returned for the caller to
/// aggregate. A successful parse yields `()`.
#[derive(Debug, Error)]
pub(crate) enum ExpressionError {
    /// The provided span falls outside the source text bounds.
    #[error("span {span:?} out of bounds")]
    OutOfBounds { span: Span },
    /// The expression parser reported syntax errors.
    #[error("{0:?}")]
    Parse(Vec<Simple<SyntaxKind>>),
}

pub(crate) fn validate_expression(src: &str, span: Span) -> Result<(), ExpressionError> {
    let text = src
        .get(span.clone())
        .ok_or(ExpressionError::OutOfBounds { span })?;
    parse_expression(text)
        .map(|_| ())
        .map_err(ExpressionError::Parse)
}
