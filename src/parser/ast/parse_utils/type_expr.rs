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

/// Skips the next element if it's trivia and advances the iterator.
///
/// Returns `true` if trivia was skipped, `false` otherwise.
fn skip_next_trivia_element<I>(iter: &mut std::iter::Peekable<I>) -> bool
where
    I: Iterator<Item = SyntaxElement<DdlogLanguage>>,
{
    if let Some(peeked) = iter.peek()
        && is_trivia(peeked)
    {
        iter.next();
        true
    } else {
        false
    }
}

/// Processes the next token within a type expression.
///
/// Returns `true` when parsing should terminate.
fn process_next_token_in_type_expr<I>(
    iter: &mut std::iter::Peekable<I>,
    ctx: &mut TokenParseContext<'_>,
    errors: &mut Vec<ParseError>,
) -> bool
where
    I: Iterator<Item = SyntaxElement<DdlogLanguage>>,
{
    if skip_next_trivia_element(iter) {
        return false;
    }
    process_type_element(iter, ctx, errors)
}

/// Parses a type expression from a token stream, returning the textual
/// representation and any parse errors. Stops when:
/// - A top-level separator is reached (`,` / `{` / `;`) after emitting some
///   text, or
/// - A closing-delimiter mismatch requires termination (e.g., `)`).
pub(crate) fn parse_type_expr<I>(iter: &mut std::iter::Peekable<I>) -> (String, Vec<ParseError>)
where
    I: Iterator<Item = SyntaxElement<DdlogLanguage>>,
{
    let mut buf = String::new();
    let mut errors = Vec::new();
    let mut stack = DelimStack::default();
    let mut ctx = TokenParseContext::new(&mut buf, &mut stack);

    while iter.peek().is_some() {
        if process_next_token_in_type_expr(iter, &mut ctx, &mut errors) {
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

/// Pushes an opening delimiter onto the stack and output buffer.
fn process_opening_delimiter<I>(
    token: &rowan::SyntaxToken<DdlogLanguage>,
    iter: &mut std::iter::Peekable<I>,
    ctx: &mut TokenParseContext<'_>,
) where
    I: Iterator<Item = SyntaxElement<DdlogLanguage>>,
{
    let (delim, count) = match token.kind() {
        SyntaxKind::T_LPAREN => (Delim::Paren, 1),
        SyntaxKind::T_LT => (Delim::Angle, 1),
        SyntaxKind::T_SHL => (Delim::Angle, 2),
        SyntaxKind::T_LBRACKET => (Delim::Bracket, 1),
        SyntaxKind::T_LBRACE => (Delim::Brace, 1),
        _ => unreachable!(),
    };
    open_delimiter(&mut *ctx.stack, delim, token.text_range(), count);
    push(token, ctx);
    iter.next();
}

/// Processes a closing delimiter and reports mismatches when necessary.
///
/// Returns `true` when a mismatched parenthesis should terminate parsing.
fn process_closing_delimiter<I>(
    token: &rowan::SyntaxToken<DdlogLanguage>,
    iter: &mut std::iter::Peekable<I>,
    ctx: &mut TokenParseContext<'_>,
    errors: &mut Vec<ParseError>,
) -> bool
where
    I: Iterator<Item = SyntaxElement<DdlogLanguage>>,
{
    let (delim, count, break_on_mismatch) = match token.kind() {
        SyntaxKind::T_RPAREN => (Delim::Paren, 1, true),
        SyntaxKind::T_GT => (Delim::Angle, 1, false),
        SyntaxKind::T_SHR => (Delim::Angle, 2, false),
        SyntaxKind::T_RBRACKET => (Delim::Bracket, 1, false),
        SyntaxKind::T_RBRACE => (Delim::Brace, 1, false),
        _ => unreachable!(),
    };

    if close_delimiter(&mut *ctx.stack, delim, count) < count {
        if !break_on_mismatch {
            push_error(errors, delim, token);
        }
        if break_on_mismatch {
            return true;
        }
    }

    push(token, ctx);
    iter.next();
    false
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

    match kind {
        SyntaxKind::T_LPAREN
        | SyntaxKind::T_LT
        | SyntaxKind::T_SHL
        | SyntaxKind::T_LBRACKET
        | SyntaxKind::T_LBRACE => {
            process_opening_delimiter(token, iter, ctx);
            false
        }
        SyntaxKind::T_RPAREN
        | SyntaxKind::T_GT
        | SyntaxKind::T_SHR
        | SyntaxKind::T_RBRACKET
        | SyntaxKind::T_RBRACE => process_closing_delimiter(token, iter, ctx, errors),
        _ => {
            push(token, ctx);
            iter.next();
            false
        }
    }
}

fn should_break_parsing(kind: SyntaxKind, stack_empty: bool, buf_empty: bool) -> bool {
    stack_empty
        && !buf_empty
        && matches!(
            kind,
            SyntaxKind::T_COMMA | SyntaxKind::T_LBRACE | SyntaxKind::T_SEMI
        )
}

/// Parses an optional type following a colon. Returns:
/// - `Some(type_text)` when a colon is present and a non-empty type is parsed
/// - `None` when no colon is present or the next token is a terminator (`{` or
///   `;`)
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
