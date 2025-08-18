//! Type expression parsing utilities.
//!
//! Provides recursive parsing for type expressions with delimiter tracking
//! and helpers for optional return types.

use rowan::SyntaxElement;

use crate::{DdlogLanguage, SyntaxKind};

use super::super::skip_whitespace_and_comments;
use super::{
    errors::{Delim, DelimStack, ParseError},
    token_utils::{TokenParseContext, close_delimiter, is_trivia, open_delimiter, push},
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
) -> bool
where
    I: Iterator<Item = SyntaxElement<DdlogLanguage>>,
{
    if skip_next_trivia_element(iter) {
        return false;
    }
    process_type_element(iter, ctx)
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
    let mut ctx = TokenParseContext::new(&mut buf, &mut stack, &mut errors);

    while iter.peek().is_some() {
        if process_next_token_in_type_expr(iter, &mut ctx) {
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
) -> bool
where
    I: Iterator<Item = SyntaxElement<DdlogLanguage>>,
{
    use rowan::NodeOrToken;

    let Some(element) = iter.peek().cloned() else {
        return false;
    };
    match element {
        NodeOrToken::Token(t) => process_type_token(&t, iter, ctx),
        NodeOrToken::Node(n) => {
            for tok in n
                .descendants_with_tokens()
                .filter_map(NodeOrToken::into_token)
            {
                push(&tok, ctx);
            }
            iter.next();
            false
        }
    }
}

#[derive(Clone, Copy)]
enum DelimiterOperation {
    Open,
    Close,
}

fn process_delimiter<I>(
    op: DelimiterOperation,
    kind: SyntaxKind,
    token: &rowan::SyntaxToken<DdlogLanguage>,
    iter: &mut std::iter::Peekable<I>,
    ctx: &mut TokenParseContext<'_>,
) -> Option<bool>
where
    I: Iterator<Item = SyntaxElement<DdlogLanguage>>,
{
    let (delim, count) = match (op, kind) {
        (DelimiterOperation::Open, SyntaxKind::T_LPAREN)
        | (DelimiterOperation::Close, SyntaxKind::T_RPAREN) => (Delim::Paren, 1),
        (DelimiterOperation::Open, SyntaxKind::T_LT)
        | (DelimiterOperation::Close, SyntaxKind::T_GT) => (Delim::Angle, 1),
        (DelimiterOperation::Open, SyntaxKind::T_SHL)
        | (DelimiterOperation::Close, SyntaxKind::T_SHR) => (Delim::Angle, 2),
        (DelimiterOperation::Open, SyntaxKind::T_LBRACKET)
        | (DelimiterOperation::Close, SyntaxKind::T_RBRACKET) => (Delim::Bracket, 1),
        (DelimiterOperation::Open, SyntaxKind::T_LBRACE)
        | (DelimiterOperation::Close, SyntaxKind::T_RBRACE) => (Delim::Brace, 1),
        _ => return None,
    };

    match op {
        DelimiterOperation::Open => {
            open_delimiter(&mut *ctx.stack, delim, token.text_range(), count);
            push(token, ctx);
            iter.next();
            Some(false)
        }
        DelimiterOperation::Close => {
            if close_delimiter(&mut *ctx.stack, delim, count) < count {
                if delim == Delim::Paren {
                    // A stray closing parenthesis terminates the type expression.
                    // Rationale: higher layers use ')' as a hard boundary for
                    // parameter lists; continuing here risks consuming tokens
                    // beyond parameters.
                    return Some(true);
                }
                ctx.push_error(delim, token);
            }
            push(token, ctx);
            iter.next();
            Some(false)
        }
    }
}
fn process_type_token<I>(
    token: &rowan::SyntaxToken<DdlogLanguage>,
    iter: &mut std::iter::Peekable<I>,
    ctx: &mut TokenParseContext<'_>,
) -> bool
where
    I: Iterator<Item = SyntaxElement<DdlogLanguage>>,
{
    let kind = token.kind();
    if should_break_parsing(kind, ctx.stack.is_empty(), ctx.buf.is_empty()) {
        return true;
    }

    if let Some(should_break) = process_delimiter(DelimiterOperation::Open, kind, token, iter, ctx)
        .or_else(|| process_delimiter(DelimiterOperation::Close, kind, token, iter, ctx))
    {
        return should_break;
    }

    push(token, ctx);
    iter.next();
    false
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
