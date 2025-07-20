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

/// Records an opening delimiter on the provided stack.
///
/// Pushes `count` instances of `delim` so that matching closing
/// tokens can be validated later. The token text itself is not
/// appended to the buffer by this helper.
///
/// # Examples
///
/// ```
/// use ddlint::parser::ast::parse_utils::errors::{Delim, DelimStack};
/// use ddlint::parser::ast::parse_utils::token_utils::open_delimiter;
///
/// let mut stack = DelimStack::default();
/// open_delimiter(&mut stack, Delim::Paren, 2);
/// assert_eq!(stack.unclosed().count(), 2);
/// ```
pub(crate) fn open_delimiter(stack: &mut DelimStack, delim: Delim, count: usize) {
    stack.open(delim, count);
}

/// Attempts to close `count` delimiters of the specified type.
///
/// Returns the number of delimiters actually removed from `stack`. A
/// value less than `count` indicates a mismatch, allowing callers to
/// emit an appropriate [`ParseError`].
///
/// # Examples
///
/// ```
/// use ddlint::parser::ast::parse_utils::errors::{Delim, DelimStack};
/// use ddlint::parser::ast::parse_utils::token_utils::{open_delimiter, close_delimiter};
///
/// let mut stack = DelimStack::default();
/// open_delimiter(&mut stack, Delim::Angle, 1);
/// assert_eq!(close_delimiter(&mut stack, Delim::Angle, 1), 1);
/// ```
pub(crate) fn close_delimiter(stack: &mut DelimStack, delim: Delim, count: usize) -> usize {
    stack.close(delim, count)
}

/// Appends token text to the buffer without modifying the stack.
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
