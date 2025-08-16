//! Type expression parsing utilities for function and relation parameters.
//!
//! This module provides functions to parse parameter name-type pairs,
//! output lists, and type expressions from syntax tokens. It handles
//! balanced delimiters and generates detailed parse errors.

use rowan::{SyntaxElement, TextRange, TextSize};

use crate::{DdlogLanguage, SyntaxKind};

use super::super::skip_whitespace_and_comments;
use super::{
    errors::{Delim, DelimStack, ParseError},
    token_utils::{TokenParseContext, close_delimiter, open_delimiter, push, push_error},
};

macro_rules! delimiter_checker {
    ($(#[$meta:meta])* $name:ident, [$($variant:path),+ $(,)?]) => {
        $(#[$meta])* fn $name(kind: SyntaxKind) -> bool {
            matches!(kind, $($variant)|+)
        }
    };
}

pub(crate) fn parse_name_type_pairs<I>(iter: I) -> (Vec<(String, String)>, Vec<ParseError>)
where
    I: Iterator<Item = SyntaxElement<DdlogLanguage>>,
{
    use rowan::NodeOrToken;

    let mut iter = iter.peekable();

    for e in &mut iter {
        if e.kind() == SyntaxKind::T_LPAREN {
            break;
        }
    }

    let mut pairs = Vec::new();
    let mut errors = Vec::new();

    loop {
        skip_whitespace_and_comments(&mut iter);

        match iter.peek() {
            Some(NodeOrToken::Token(t)) if t.kind() == SyntaxKind::T_RPAREN => {
                iter.next();
                break;
            }
            None => break,
            _ => {}
        }

        let (name, found_colon, span) = collect_parameter_name(&mut iter);
        if let Some(pair) = finalise_parameter(name, found_colon, span, &mut iter, &mut errors) {
            pairs.push(pair);
        }

        skip_whitespace_and_comments(&mut iter);
        match iter.peek() {
            Some(NodeOrToken::Token(t)) if t.kind() == SyntaxKind::T_COMMA => {
                iter.next();
            }
            Some(NodeOrToken::Token(t)) if t.kind() == SyntaxKind::T_RPAREN => {
                iter.next();
                break;
            }
            _ => {}
        }
    }

    for e in iter {
        if let NodeOrToken::Token(t) = e {
            match t.kind() {
                SyntaxKind::T_RPAREN => push_error(&mut errors, Delim::Paren, &t),
                SyntaxKind::T_RBRACKET => push_error(&mut errors, Delim::Bracket, &t),
                SyntaxKind::T_RBRACE => push_error(&mut errors, Delim::Brace, &t),
                SyntaxKind::T_GT | SyntaxKind::T_SHR => push_error(&mut errors, Delim::Angle, &t),
                _ => break,
            }
        } else {
            break;
        }
    }

    (pairs, errors)
}

fn skip_to_top_level_colon<I>(iter: &mut std::iter::Peekable<I>)
where
    I: Iterator<Item = SyntaxElement<DdlogLanguage>>,
{
    let mut depth = 0usize;
    for e in iter.by_ref() {
        match e.kind() {
            SyntaxKind::T_LPAREN => depth += 1,
            SyntaxKind::T_RPAREN => depth = depth.saturating_sub(1),
            SyntaxKind::T_COLON if depth == 0 => break,
            _ => {}
        }
    }
}

pub(crate) fn parse_output_list<I>(iter: I) -> Vec<String>
where
    I: Iterator<Item = SyntaxElement<DdlogLanguage>>,
{
    use rowan::NodeOrToken;

    let mut iter = iter.peekable();
    skip_to_top_level_colon(&mut iter);

    let mut names = Vec::new();
    loop {
        skip_whitespace_and_comments(&mut iter);
        match iter.next() {
            Some(NodeOrToken::Token(t)) if t.kind() == SyntaxKind::T_IDENT => {
                names.push(t.text().to_string());
            }
            _ => break,
        }
        skip_whitespace_and_comments(&mut iter);
        match iter.peek() {
            Some(NodeOrToken::Token(t)) if t.kind() == SyntaxKind::T_COMMA => {
                iter.next();
            }
            _ => break,
        }
    }

    names
}

pub(crate) fn parse_type_expr<I>(iter: &mut std::iter::Peekable<I>) -> (String, Vec<ParseError>)
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

/// Predicate for exiting the main parsing loop.
fn should_break_parsing(kind: SyntaxKind, stack_empty: bool) -> bool {
    kind == SyntaxKind::T_COMMA && stack_empty
}

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

/// Updates start and end positions for an element.
///
/// Records the beginning of the first element encountered and updates the end
/// position on each call so the resulting span covers all processed tokens.
fn track_span_for_element(
    element: &SyntaxElement<DdlogLanguage>,
    start_pos: &mut Option<TextSize>,
    end_pos: &mut Option<TextSize>,
) {
    let range = element.text_range();
    if start_pos.is_none() {
        *start_pos = Some(range.start());
    }
    *end_pos = Some(range.end());
}

/// Determines whether the element is whitespace or a comment.
fn is_trivia(e: &SyntaxElement<DdlogLanguage>) -> bool {
    use rowan::NodeOrToken;

    match e {
        NodeOrToken::Token(t) => {
            matches!(t.kind(), SyntaxKind::T_WHITESPACE | SyntaxKind::T_COMMENT)
        }
        NodeOrToken::Node(n) => {
            let text = n.text().to_string();
            n.kind() == SyntaxKind::T_COMMENT || text.chars().all(char::is_whitespace)
        }
    }
}

fn collect_parameter_name<I>(iter: &mut std::iter::Peekable<I>) -> (String, bool, Option<TextRange>)
where
    I: Iterator<Item = SyntaxElement<DdlogLanguage>>,
{
    let mut name_buf = String::new();
    let mut found_colon = false;
    let mut start_pos: Option<TextSize> = None;
    let mut end_pos: Option<TextSize> = None;

    while let Some(peeked) = iter.peek() {
        if is_trivia(peeked) {
            iter.next();
            continue;
        }

        if process_parameter_token(
            iter,
            &mut name_buf,
            &mut found_colon,
            &mut start_pos,
            &mut end_pos,
        ) {
            break;
        }
    }

    let span = create_text_span(start_pos, end_pos);
    (name_buf.trim().to_string(), found_colon, span)
}

fn process_parameter_token<I>(
    iter: &mut std::iter::Peekable<I>,
    name_buf: &mut String,
    found_colon: &mut bool,
    start_pos: &mut Option<TextSize>,
    end_pos: &mut Option<TextSize>,
) -> bool
where
    I: Iterator<Item = SyntaxElement<DdlogLanguage>>,
{
    use rowan::NodeOrToken;

    let Some(element) = iter.peek().cloned() else {
        return false;
    };
    track_span_for_element(&element, start_pos, end_pos);
    match element {
        NodeOrToken::Token(t) => process_token(&t, iter, name_buf, found_colon),
        NodeOrToken::Node(n) => process_node(&n, iter, name_buf),
    }
}

fn process_token<I>(
    token: &rowan::SyntaxToken<DdlogLanguage>,
    iter: &mut std::iter::Peekable<I>,
    name_buf: &mut String,
    found_colon: &mut bool,
) -> bool
where
    I: Iterator<Item = SyntaxElement<DdlogLanguage>>,
{
    match token.kind() {
        SyntaxKind::T_COLON => {
            iter.next();
            *found_colon = true;
            true
        }
        SyntaxKind::T_COMMA | SyntaxKind::T_RPAREN => true,
        _ => {
            name_buf.push_str(token.text());
            iter.next();
            false
        }
    }
}

fn process_node<I>(
    node: &rowan::SyntaxNode<DdlogLanguage>,
    iter: &mut std::iter::Peekable<I>,
    name_buf: &mut String,
) -> bool
where
    I: Iterator<Item = SyntaxElement<DdlogLanguage>>,
{
    name_buf.push_str(&node.text().to_string());
    iter.next();
    false
}

fn create_text_span(start_pos: Option<TextSize>, end_pos: Option<TextSize>) -> Option<TextRange> {
    match (start_pos, end_pos) {
        (Some(start), Some(end)) => Some(TextRange::new(start, end)),
        _ => None,
    }
}

fn finalise_parameter<I>(
    name: String,
    found_colon: bool,
    span: Option<TextRange>,
    iter: &mut std::iter::Peekable<I>,
    errors: &mut Vec<ParseError>,
) -> Option<(String, String)>
where
    I: Iterator<Item = SyntaxElement<DdlogLanguage>>,
{
    if !found_colon {
        if let Some(s) = span {
            errors.push(ParseError::MissingColon {
                message: "Expected ':' after name, but found ',' or ')' instead".to_string(),
                span: s,
            });
        }

        while let Some(e) = iter.peek() {
            match e.kind() {
                SyntaxKind::T_COMMA => {
                    iter.next();
                    break;
                }
                SyntaxKind::T_RPAREN => {
                    iter.next();
                    return None;
                }
                _ => {
                    iter.next();
                }
            }
        }
        return None;
    }

    skip_whitespace_and_comments(iter);
    let (ty, mut errs) = parse_type_expr(iter);
    errors.append(&mut errs);

    if name.is_empty()
        && !ty.is_empty()
        && let Some(s) = span
    {
        errors.push(ParseError::MissingName { span: s });
    }
    if !name.is_empty()
        && ty.is_empty()
        && let Some(s) = span
    {
        errors.push(ParseError::MissingType { span: s });
    }

    if !name.is_empty() && !ty.is_empty() {
        Some((name, ty))
    } else {
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::ast::AstNode;
    use crate::parser::parse;
    use rstest::{fixture, rstest};

    #[fixture]
    fn tokens_for(#[default("function t() {}")] src: &str) -> Vec<SyntaxElement<DdlogLanguage>> {
        let parsed = parse(src);
        let functions = parsed.root().functions();
        functions.first().map_or_else(
            || {
                let relations = parsed.root().relations();
                relations.first().map_or_else(
                    || parsed.root().syntax().children_with_tokens().collect(),
                    |rel| rel.syntax().children_with_tokens().collect(),
                )
            },
            |func| func.syntax().children_with_tokens().collect(),
        )
    }

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
    #[case(
        "function f(a: u32, b: string) {}",
        vec![("a".into(), "u32".into()), ("b".into(), "string".into())],
        0
    )]
    #[case(
        "input relation R(id: u32, name: string)",
        vec![("id".into(), "u32".into()), ("name".into(), "string".into())],
        0
    )]
    #[case(
        "function wrap(t: Option<(u32, string)>) {}",
        vec![("t".into(), "Option<(u32, string)>".into())],
        0
    )]
    #[case(
        "function g(m: Map<string, u64>) {}",
        vec![("m".into(), "Map<string, u64>".into())],
        0
    )]
    #[case(
        "function nested(p: Vec<Map<string, Vec<u8>>>) {}",
        vec![("p".into(), "Vec<Map<string, Vec<u8>>>".into())],
        0
    )]
    #[case(
        "function array(a: [Vec<u32>]) {}",
        vec![("a".into(), "[Vec<u32>]".into())],
        0
    )]
    #[case(
        "function nested_vec(v: Vec<Vec<u8>>) {}",
        vec![("v".into(), "Vec<Vec<u8>>".into())],
        0
    )]
    #[case(
        "function weird(x: Vec<<u8>>) {}",
        vec![("x".into(), "Vec<<u8>>".into())],
        0
    )]
    #[case(
        "function with_comment(a /* c */: u32) {}",
        vec![("a".into(), "u32".into())],
        0,
    )]
    #[case(
        "function with_space(a   : u32) {}",
        vec![("a".into(), "u32".into())],
        0,
    )]
    #[case("function empty() {}", Vec::new(), 0)]
    #[case(
        "function missing(a u32, b: bool) {}",
        vec![("b".into(), "bool".into())],
        1
    )]
    fn name_type_pairs(
        #[case] src: &str,
        #[case] expected: Vec<(String, String)>,
        #[case] err_count: usize,
        #[with(src)] tokens_for: Vec<SyntaxElement<DdlogLanguage>>,
    ) {
        let _ = src;
        let elements = tokens_for;
        let (result, errors) = parse_name_type_pairs(elements.into_iter());
        assert_eq!(errors.len(), err_count);
        assert_eq!(result, expected);
    }

    #[test]
    fn unmatched_shift_errors() {
        let src = "function bad(x: Vec<u8>>): bool {}";
        let elements = tokens_for(src);
        let (_pairs, errors) = parse_name_type_pairs(elements.into_iter());
        assert_eq!(errors.len(), 1);
    }

    #[test]
    fn unmatched_bracket_error() {
        let src = "function bad(x: Vec<u8>], y: u32) {}";
        let elements = tokens_for(src);
        let (_pairs, errors) = parse_name_type_pairs(elements.into_iter());
        assert_eq!(errors.len(), 1);
    }

    #[test]
    fn unmatched_brace_error() {
        let src = "function bad(x: u32}, y: bool) {}";
        let elements = tokens_for(src);
        let (_pairs, errors) = parse_name_type_pairs(elements.into_iter());
        assert_eq!(errors.len(), 1);
    }

    #[test]
    fn unclosed_angle_error() {
        let src = "function bad(x: Vec<u32) {}";
        let elements = tokens_for(src);
        let (_pairs, errors) = parse_name_type_pairs(elements.clone().into_iter());
        assert_eq!(errors.len(), 1);
        #[expect(clippy::expect_used, reason = "Using expect for clearer test failures")]
        let angle_span = elements
            .iter()
            .find_map(|e| match e {
                SyntaxElement::Token(t) if t.text() == "<" => Some(t.text_range()),
                _ => None,
            })
            .expect("angle token missing");
        #[expect(clippy::expect_used, reason = "Using expect for clearer test failures")]
        let err = errors.first().expect("no error");
        match err {
            ParseError::UnclosedDelimiter {
                delimiter: '>',
                span,
            } => {
                assert_eq!(*span, angle_span);
            }
            other => panic!("unexpected error: {other:?}"),
        }
    }

    #[rstest]
    #[case("function bad(x: (u32 {}", ')', "(")]
    #[case("function bad(x: [u32 {}", ']', "[")]
    #[case("function bad(x: {u32 {}", '}', "{")]
    fn unclosed_other_delims(
        #[case] src: &str,
        #[case] expected_delim: char,
        #[case] opener: &str,
    ) {
        let elements = tokens_for(src);
        let (_pairs, errors) = parse_name_type_pairs(elements.clone().into_iter());
        let spans: Vec<TextRange> = elements
            .iter()
            .filter_map(|e| match e {
                SyntaxElement::Token(t) if t.text() == opener => Some(t.text_range()),
                SyntaxElement::Token(_) | SyntaxElement::Node(_) => None,
            })
            .collect();
        let open_span = if opener == "(" {
            #[expect(clippy::expect_used, reason = "Using expect for clearer test failures")]
            spans.last().copied().expect("opening token missing")
        } else {
            #[expect(clippy::expect_used, reason = "Using expect for clearer test failures")]
            spans.first().copied().expect("opening token missing")
        };
        #[expect(clippy::expect_used, reason = "Using expect for clearer test failures")]
        let err = errors
            .iter()
            .find(|e| {
                matches!(
                    e,
                    ParseError::UnclosedDelimiter { delimiter, .. }
                        if *delimiter == expected_delim
                )
            })
            .expect("expected unclosed delimiter missing");
        match err {
            ParseError::UnclosedDelimiter { span, .. } => {
                assert_eq!(*span, open_span);
            }
            _ => unreachable!(),
        }
    }

    #[test]
    fn empty_name_error() {
        let src = "function bad(: u32) {}";
        let elements = tokens_for(src);
        let (_pairs, errors) = parse_name_type_pairs(elements.into_iter());
        assert_eq!(errors.len(), 1);
    }

    #[test]
    fn empty_type_error() {
        let src = "function bad(x:) {}";
        let elements = tokens_for(src);
        let (_pairs, errors) = parse_name_type_pairs(elements.into_iter());
        assert_eq!(errors.len(), 1);
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
