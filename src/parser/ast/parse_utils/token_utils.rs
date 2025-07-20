use crate::DdlogLanguage;

use super::errors::{Delim, DelimStack, DelimiterError, ParseError};

pub(crate) fn open_and_push(
    token: &rowan::SyntaxToken<DdlogLanguage>,
    buf: &mut String,
    stack: &mut DelimStack,
    delim: Delim,
    count: usize,
) {
    stack.open(delim, count);
    buf.push_str(token.text());
}

pub(crate) fn close_and_push(
    token: &rowan::SyntaxToken<DdlogLanguage>,
    buf: &mut String,
    stack: &mut DelimStack,
    delim: Delim,
    count: usize,
) -> usize {
    let closed = stack.close(delim, count);
    buf.push_str(token.text());
    closed
}

pub(crate) fn push(token: &rowan::SyntaxToken<DdlogLanguage>, buf: &mut String) {
    buf.push_str(token.text());
}

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
