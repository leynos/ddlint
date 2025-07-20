//! Token manipulation utilities for delimiter parsing.
//!
//! These helpers maintain a `DelimStack` while building up a type expression
//! string, ensuring delimiters are tracked consistently and any mismatches are
//! reported via `ParseError`.

use crate::DdlogLanguage;

use super::errors::{Delim, DelimStack, DelimiterError, ParseError};

/// Shared context for token parsing utilities.
#[derive(Debug)]
pub(crate) struct TokenParseContext<'a> {
    pub(crate) buf: &'a mut String,
    pub(crate) stack: &'a mut DelimStack,
}

impl<'a> TokenParseContext<'a> {
    pub(crate) fn new(buf: &'a mut String, stack: &'a mut DelimStack) -> Self {
        Self { buf, stack }
    }
}

pub(crate) fn open_delimiter(stack: &mut DelimStack, delim: Delim, count: usize) {
    stack.open(delim, count);
}
pub(crate) fn close_delimiter(stack: &mut DelimStack, delim: Delim, count: usize) -> usize {
    stack.close(delim, count)
}

pub(crate) fn push(token: &rowan::SyntaxToken<DdlogLanguage>, ctx: &mut TokenParseContext<'_>) {
    ctx.buf.push_str(token.text());
}

/// Record an unexpected delimiter in the provided error list.
///
/// A [`ParseError::Delimiter`] is appended with the expected delimiter
/// type and the span of the found token.
pub(crate) fn push_error(
    errors: &mut Vec<ParseError>,
    expected: Delim,
    token: &rowan::SyntaxToken<DdlogLanguage>,
) {
    errors.push(ParseError::Delimiter(DelimiterError {
        expected,
        found: token.kind(),
        span: token.text_range(),
    }));
}
