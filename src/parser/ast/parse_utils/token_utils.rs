//! Token manipulation utilities for parsing.
//!
//! These helpers maintain a `DelimStack` while building up a type expression
//! string, classify trivia, and report delimiter mismatches via
//! [`ParseError`].

use crate::{DdlogLanguage, SyntaxKind};
use rowan::{SyntaxElement, TextRange};

use super::errors::{Delim, DelimStack, DelimiterError, ParseError};

/// Shared context for token parsing utilities.
#[derive(Debug)]
pub(crate) struct TokenParseContext<'a> {
    pub(crate) buf: &'a mut String,
    pub(crate) stack: &'a mut DelimStack,
    pub(crate) errors: &'a mut Vec<ParseError>,
}

impl<'a> TokenParseContext<'a> {
    pub(crate) fn new(
        buf: &'a mut String,
        stack: &'a mut DelimStack,
        errors: &'a mut Vec<ParseError>,
    ) -> Self {
        Self { buf, stack, errors }
    }

    /// Record an unexpected delimiter encountered while parsing.
    ///
    /// The error is pushed onto the shared buffer within the context.
    pub(crate) fn push_error(
        &mut self,
        expected: Delim,
        token: &rowan::SyntaxToken<DdlogLanguage>,
    ) {
        self.errors.push(ParseError::Delimiter(DelimiterError {
            expected,
            found: token.kind(),
            span: token.text_range(),
        }));
    }
}

/// Records an opening delimiter on the provided stack.
///
/// Pushes `count` instances of `delim`, each tagged with the provided `span`, so
/// that matching closing tokens can be validated later. The token text itself is
/// not appended to the buffer by this helper.
///
/// # Examples
///
/// ```rust
/// use ddlint::parser::ast::parse_utils::errors::{Delim, DelimStack};
/// use ddlint::parser::ast::parse_utils::token_utils::open_delimiter;
/// use rowan::{TextRange, TextSize};
///
/// let mut stack = DelimStack::default();
/// let span = TextRange::empty(TextSize::from(0));
/// open_delimiter(&mut stack, Delim::Paren, span, 2);
/// assert_eq!(stack.unclosed().count(), 2);
/// ```
pub(crate) fn open_delimiter(stack: &mut DelimStack, delim: Delim, span: TextRange, count: usize) {
    stack.open(delim, span, count);
}

/// Attempts to close `count` delimiters of the specified type.
///
/// Returns the number of delimiters actually removed from `stack`. A
/// value less than `count` indicates a mismatch, allowing callers to
/// emit an appropriate [`ParseError`].
///
/// # Examples
///
/// ```rust
/// use ddlint::parser::ast::parse_utils::errors::{Delim, DelimStack};
/// use ddlint::parser::ast::parse_utils::token_utils::{open_delimiter, close_delimiter};
/// use rowan::{TextRange, TextSize};
///
/// let mut stack = DelimStack::default();
/// let span = TextRange::empty(TextSize::from(0));
/// open_delimiter(&mut stack, Delim::Angle, span, 1);
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

/// Determines whether the element is whitespace or a comment.
pub(crate) fn is_trivia(e: &SyntaxElement<DdlogLanguage>) -> bool {
    use rowan::NodeOrToken;

    match e {
        NodeOrToken::Token(t) => {
            matches!(t.kind(), SyntaxKind::T_WHITESPACE | SyntaxKind::T_COMMENT)
        }
        // If comments can be nodes in the CST, this cheap kind check suffices.
        NodeOrToken::Node(n) => n.kind() == SyntaxKind::T_COMMENT,
    }
}
