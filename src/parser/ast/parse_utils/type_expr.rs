//! Type expression parsing utilities.
//!
//! Provides recursive parsing for type expressions with delimiter tracking
//! and helpers for optional return types.

use rowan::SyntaxElement;

use crate::{DdlogLanguage, SyntaxKind};

use super::super::skip_whitespace_and_comments;
use super::{
    errors::{Delim, DelimStack, ParseError},
    token_utils::{
        TokenParseContext, close_delimiter, is_trivia, open_delimiter, push, push_error,
    },
};

macro_rules! delimiter_checker {
    ($(#[$meta:meta])* $name:ident, [$($variant:path),+ $(,)?]) => {
        $(#[$meta])* fn $name(kind: SyntaxKind) -> bool {
            matches!(kind, $($variant)|+)
        }
    };
}

pub(crate) fn parse_type_expr<I>(iter: &mut std::iter::Peekable<I>) -> (String, Vec<ParseError>)
where
    I: Iterator<Item = SyntaxElement<DdlogLanguage>>,
{
    let mut buf = String::new();
    let mut errors = Vec::new();
    let mut stack = DelimStack::default();
    let mut ctx = TokenParseContext::new(&mut buf, &mut stack);

    while iter.peek().is_some() {
        if let Some(peeked) = iter.peek()
            && is_trivia(peeked)
        {
            iter.next();
            continue;
        }

        if process_type_element(iter, &mut ctx, &mut errors) {
            break;
        }
    }

    for (unclosed, span) in stack.unclosed() {
        let ch = match unclosed {
            Delim::Paren => ')',
            Delim::Angle => '>',
            Delim::Bracket => ']',
            Delim::Brace => '}',
        };
        errors.push(ParseError::UnclosedDelimiter {
            delimiter: ch,
            span,
        });
    }

    (buf.trim().to_string(), errors)
}

fn process_type_element<I>(
    iter: &mut std::iter::Peekable<I>,
    ctx: &mut TokenParseContext<'_>,
    errors: &mut Vec<ParseError>,
) -> bool
where
    I: Iterator<Item = SyntaxElement<DdlogLanguage>>,
{
    use rowan::NodeOrToken;

    let Some(element) = iter.peek().cloned() else {
        return false;
    };
    match element {
        NodeOrToken::Token(t) => process_type_token(&t, iter, ctx, errors),
        NodeOrToken::Node(n) => {
            ctx.buf.push_str(&n.text().to_string());
            iter.next();
            false
        }
    }
}

fn process_type_token<I>(
    token: &rowan::SyntaxToken<DdlogLanguage>,
    iter: &mut std::iter::Peekable<I>,
    ctx: &mut TokenParseContext<'_>,
    errors: &mut Vec<ParseError>,
) -> bool
where
    I: Iterator<Item = SyntaxElement<DdlogLanguage>>,
{
    let kind = token.kind();
    if should_break_parsing(kind, ctx.stack.is_empty(), ctx.buf.is_empty()) {
        return true;
    }

    if is_opening_delimiter(kind) {
        handle_opening_delimiter_and_advance(token, iter, ctx);
        return false;
    }

    if is_closing_delimiter(kind) {
        return handle_closing_delimiter_and_advance(token, iter, ctx, errors);
    }

    push(token, ctx);
    iter.next();
    false
}

fn handle_opening_delimiter_and_advance<I>(
    token: &rowan::SyntaxToken<DdlogLanguage>,
    iter: &mut std::iter::Peekable<I>,
    ctx: &mut TokenParseContext<'_>,
) where
    I: Iterator<Item = SyntaxElement<DdlogLanguage>>,
{
    if handle_opening_delimiter(token, ctx) {
        iter.next();
    }
}

fn handle_closing_delimiter_and_advance<I>(
    token: &rowan::SyntaxToken<DdlogLanguage>,
    iter: &mut std::iter::Peekable<I>,
    ctx: &mut TokenParseContext<'_>,
    errors: &mut Vec<ParseError>,
) -> bool
where
    I: Iterator<Item = SyntaxElement<DdlogLanguage>>,
{
    if handle_closing_delimiter(token, ctx, errors) {
        iter.next();
        false
    } else {
        true
    }
}

/// Handles an opening delimiter token.
fn handle_opening_delimiter(
    token: &rowan::SyntaxToken<DdlogLanguage>,
    ctx: &mut TokenParseContext<'_>,
) -> bool {
    let mapping = match token.kind() {
        SyntaxKind::T_LPAREN => Some((Delim::Paren, 1)),
        SyntaxKind::T_LT => Some((Delim::Angle, 1)),
        SyntaxKind::T_SHL => Some((Delim::Angle, 2)),
        SyntaxKind::T_LBRACKET => Some((Delim::Bracket, 1)),
        SyntaxKind::T_LBRACE => Some((Delim::Brace, 1)),
        _ => None,
    };
    if let Some((delim, count)) = mapping {
        open_delimiter(&mut *ctx.stack, delim, token.text_range(), count);
        push(token, ctx);
        true
    } else {
        false
    }
}

/// Handles a closing delimiter token.
///
/// Returns `true` when the closing delimiter matches the top of the stack.
/// When a mismatch occurs a [`ParseError::Delimiter`] is pushed to `errors`
/// and `false` is returned if the token should terminate parsing.
fn handle_closing_delimiter(
    token: &rowan::SyntaxToken<DdlogLanguage>,
    ctx: &mut TokenParseContext<'_>,
    errors: &mut Vec<ParseError>,
) -> bool {
    let (delim, count, break_on_mismatch) = match token.kind() {
        SyntaxKind::T_RPAREN => (Delim::Paren, 1, true),
        SyntaxKind::T_GT => (Delim::Angle, 1, false),
        SyntaxKind::T_SHR => (Delim::Angle, 2, false),
        SyntaxKind::T_RBRACKET => (Delim::Bracket, 1, false),
        SyntaxKind::T_RBRACE => (Delim::Brace, 1, false),
        _ => return false,
    };

    if close_delimiter(&mut *ctx.stack, delim, count) < count {
        if !break_on_mismatch {
            push_error(errors, delim, token);
        }
        if break_on_mismatch {
            return false;
        }
    }

    push(token, ctx);
    true
}

delimiter_checker!(
    /// Determines whether a syntax kind opens a delimiter pair.
    is_opening_delimiter,
    [
        SyntaxKind::T_LPAREN,
        SyntaxKind::T_LT,
        SyntaxKind::T_SHL,
        SyntaxKind::T_LBRACKET,
        SyntaxKind::T_LBRACE,
    ]
);

delimiter_checker!(
    /// Determines whether a syntax kind closes a delimiter pair.
    is_closing_delimiter,
    [
        SyntaxKind::T_RPAREN,
        SyntaxKind::T_GT,
        SyntaxKind::T_SHR,
        SyntaxKind::T_RBRACKET,
        SyntaxKind::T_RBRACE,
    ]
);

fn should_break_parsing(kind: SyntaxKind, stack_empty: bool, buf_empty: bool) -> bool {
    stack_empty
        && !buf_empty
        && matches!(
            kind,
            SyntaxKind::T_COMMA | SyntaxKind::T_LBRACE | SyntaxKind::T_SEMI
        )
}

pub(crate) fn parse_type_after_colon<I>(iter: &mut std::iter::Peekable<I>) -> Option<String>
where
    I: Iterator<Item = SyntaxElement<DdlogLanguage>>,
{
    skip_whitespace_and_comments(iter);
    if !matches!(
        iter.peek().map(SyntaxElement::kind),
        Some(SyntaxKind::T_COLON)
    ) {
        return None;
    }
    iter.next();

    skip_whitespace_and_comments(iter);
    if matches!(
        iter.peek().map(SyntaxElement::kind),
        Some(SyntaxKind::T_LBRACE | SyntaxKind::T_SEMI)
    ) {
        return None;
    }

    let (ty, _errs) = parse_type_expr(iter);
    let text = ty.trim();
    if text.is_empty() {
        None
    } else {
        Some(text.to_string())
    }
}

#[cfg(test)]
mod tests;
