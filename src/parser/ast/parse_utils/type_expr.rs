//! Parsing of type expressions and trailing return types.
//!
//! This module handles recursive type expression parsing, ensuring delimiters
//! remain balanced. It also provides a helper for capturing the type that
//! follows a colon, used when extracting return types from functions.

use rowan::SyntaxElement;

use crate::{DdlogLanguage, SyntaxKind};

use super::super::skip_whitespace_and_comments;
use super::{
    errors::{Delim, DelimStack, ParseError},
    token_utils::{
        TokenParseContext, close_delimiter, delimiter_checker, open_delimiter, push, push_error,
    },
};

/// Parse a type expression from the provided iterator.
///
/// Returns the collected text and any delimiter errors encountered. Parsing
/// stops at a comma encountered at the top level.
pub(super) fn parse_type_expr<I>(iter: &mut std::iter::Peekable<I>) -> (String, Vec<ParseError>)
where
    I: Iterator<Item = SyntaxElement<DdlogLanguage>>,
{
    use rowan::NodeOrToken;

    let mut buf = String::new();
    let mut errors = Vec::new();
    let mut stack = DelimStack::default();
    let mut ctx = TokenParseContext::new(&mut buf, &mut stack);

    while let Some(e) = iter.peek() {
        match e {
            NodeOrToken::Token(t) => match t.kind() {
                kind if is_opening_delimiter(kind) => {
                    if handle_opening_delimiter(t, &mut ctx) {
                        iter.next();
                    }
                }
                kind if is_closing_delimiter(kind) => {
                    if handle_closing_delimiter(t, &mut ctx, &mut errors) {
                        iter.next();
                    } else {
                        break;
                    }
                }
                kind if should_break_parsing(kind, ctx.stack.is_empty()) => break,
                _ => {
                    push(t, &mut ctx);
                    iter.next();
                }
            },
            NodeOrToken::Node(n) => {
                let text = n.text().to_string();
                let is_ws = text.chars().all(char::is_whitespace);
                let is_comment = n.kind() == SyntaxKind::T_COMMENT;
                if !is_ws && !is_comment {
                    ctx.buf.push_str(&text);
                }
                iter.next();
            }
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
/// Returns `true` when the delimiter matches the stack top. On mismatch the
/// token may still be pushed depending on the delimiter type.
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

/// Predicate for exiting the main parsing loop.
fn should_break_parsing(kind: SyntaxKind, stack_empty: bool) -> bool {
    kind == SyntaxKind::T_COMMA && stack_empty
}

/// Parse a type expression appearing after a colon.
pub(crate) fn parse_type_after_colon<I>(iter: &mut std::iter::Peekable<I>) -> Option<String>
where
    I: Iterator<Item = SyntaxElement<DdlogLanguage>>,
{
    use rowan::NodeOrToken;

    skip_whitespace_and_comments(iter);
    if !matches!(
        iter.peek().map(SyntaxElement::kind),
        Some(SyntaxKind::T_COLON)
    ) {
        return None;
    }
    iter.next();

    let mut buf = String::new();
    for e in iter {
        match e {
            NodeOrToken::Token(t) => match t.kind() {
                SyntaxKind::T_LBRACE | SyntaxKind::T_SEMI => break,
                SyntaxKind::T_WHITESPACE if t.text().contains('\n') => break,
                _ => buf.push_str(t.text()),
            },
            NodeOrToken::Node(n) => buf.push_str(&n.text().to_string()),
        }
    }

    let text = buf.trim();
    if text.is_empty() {
        None
    } else {
        Some(text.to_string())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::ast::AstNode;
    use crate::parser::parse;
    use rstest::{fixture, rstest};

    #[fixture]
    fn return_type_for(#[default("function t() {}")] src: &str) -> Option<String> {
        let parsed = parse(src);
        let functions = parsed.root().functions();
        #[expect(clippy::expect_used, reason = "Using expect for clearer test failures")]
        let func = functions.first().expect("function missing");
        let mut iter = func.syntax().children_with_tokens().peekable();
        let mut depth = 0usize;
        for e in &mut iter {
            match e.kind() {
                SyntaxKind::T_LPAREN => depth += 1,
                SyntaxKind::T_RPAREN => {
                    depth -= 1;
                    if depth == 0 {
                        break;
                    }
                }
                _ => {}
            }
        }
        parse_type_after_colon(&mut iter)
    }

    #[rstest]
    #[case("function f(): u32 {}", Some("u32".to_string()))]
    #[case("extern function f(): bool;", Some("bool".to_string()))]
    #[case("function f() {}", None)]
    #[case("function f():\n    u32 {}", None)]
    #[case("function f(): {}", None)]
    fn trailing_type(
        #[case] src: &str,
        #[case] expected: Option<String>,
        #[with(src)] return_type_for: Option<String>,
    ) {
        let _ = src;
        assert_eq!(return_type_for, expected);
    }
}
