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
    let mut depth = DelimStack::default();
    let mut ctx = TokenParseContext::new(&mut buf, &mut depth);

    while let Some(e) = iter.peek() {
        match e {
            NodeOrToken::Token(t) => match t.kind() {
                SyntaxKind::T_LPAREN => {
                    open_delimiter(&mut *ctx.stack, Delim::Paren, 1);
                    push(t, &mut ctx);
                    iter.next();
                }
                SyntaxKind::T_RPAREN => {
                    if ctx.stack.close(Delim::Paren, 1) == 0 {
                        break;
                    }
                    push(t, &mut ctx);
                    iter.next();
                }
                SyntaxKind::T_LT => {
                    open_delimiter(&mut *ctx.stack, Delim::Angle, 1);
                    push(t, &mut ctx);
                    iter.next();
                }
                SyntaxKind::T_SHL => {
                    open_delimiter(&mut *ctx.stack, Delim::Angle, 2);
                    push(t, &mut ctx);
                    iter.next();
                }
                SyntaxKind::T_GT => {
                    if close_delimiter(&mut *ctx.stack, Delim::Angle, 1) < 1 {
                        push_error(&mut errors, Delim::Angle, t);
                    }
                    push(t, &mut ctx);
                    iter.next();
                }
                SyntaxKind::T_SHR => {
                    if close_delimiter(&mut *ctx.stack, Delim::Angle, 2) < 2 {
                        push_error(&mut errors, Delim::Angle, t);
                    }
                    push(t, &mut ctx);
                    iter.next();
                }
                SyntaxKind::T_LBRACKET => {
                    open_delimiter(&mut *ctx.stack, Delim::Bracket, 1);
                    push(t, &mut ctx);
                    iter.next();
                }
                SyntaxKind::T_RBRACKET => {
                    if close_delimiter(&mut *ctx.stack, Delim::Bracket, 1) < 1 {
                        push_error(&mut errors, Delim::Bracket, t);
                    }
                    push(t, &mut ctx);
                    iter.next();
                }
                SyntaxKind::T_LBRACE => {
                    open_delimiter(&mut *ctx.stack, Delim::Brace, 1);
                    push(t, &mut ctx);
                    iter.next();
                }
                SyntaxKind::T_RBRACE => {
                    if close_delimiter(&mut *ctx.stack, Delim::Brace, 1) < 1 {
                        push_error(&mut errors, Delim::Brace, t);
                    }
                    push(t, &mut ctx);
                    iter.next();
                }
                SyntaxKind::T_COMMA if ctx.stack.is_empty() => break,
                _ => {
                    push(t, &mut ctx);
                    iter.next();
                }
            },
            NodeOrToken::Node(n) => {
                let text = n.text().to_string();
                let is_whitespace = text.chars().all(char::is_whitespace);
                let is_comment = n.kind() == SyntaxKind::T_COMMENT;
                if !is_whitespace && !is_comment {
                    ctx.buf.push_str(&text);
                }
                iter.next();
            }
        }
    }

    for unclosed in depth.unclosed() {
        let ch = match unclosed {
            Delim::Paren => ')',
            Delim::Angle => '>',
            Delim::Bracket => ']',
            Delim::Brace => '}',
        };
        errors.push(ParseError::UnclosedDelimiter {
            delimiter: ch,
            span: TextRange::empty(0.into()),
        });
    }

    (buf.trim().to_string(), errors)
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

fn collect_parameter_name<I>(iter: &mut std::iter::Peekable<I>) -> (String, bool, Option<TextRange>)
where
    I: Iterator<Item = SyntaxElement<DdlogLanguage>>,
{
    use rowan::NodeOrToken;

    let mut name_buf = String::new();
    let mut found_colon = false;
    let mut start_pos: Option<TextSize> = None;
    let mut end_pos: Option<TextSize> = None;

    while let Some(e) = iter.peek() {
        match e {
            NodeOrToken::Token(t) => match t.kind() {
                SyntaxKind::T_COLON => {
                    track_span_for_element(e, &mut start_pos, &mut end_pos);
                    iter.next();
                    found_colon = true;
                    break;
                }
                SyntaxKind::T_COMMA | SyntaxKind::T_RPAREN => {
                    track_span_for_element(e, &mut start_pos, &mut end_pos);
                    break;
                }
                _ => {
                    track_span_for_element(e, &mut start_pos, &mut end_pos);
                    name_buf.push_str(t.text());
                    iter.next();
                }
            },
            NodeOrToken::Node(_) => {
                track_span_for_element(e, &mut start_pos, &mut end_pos);
                if let NodeOrToken::Node(n) = e {
                    name_buf.push_str(&n.text().to_string());
                }
                iter.next();
            }
        }
    }

    let span = match (start_pos, end_pos) {
        (Some(start), Some(end)) => Some(TextRange::new(start, end)),
        _ => None,
    };
    (name_buf.trim().to_string(), found_colon, span)
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
                #[expect(clippy::expect_used, reason = "Using expect for clearer test failures")]
                relations
                    .first()
                    .expect("relation missing in test")
                    .syntax()
                    .children_with_tokens()
                    .collect()
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
        let (_pairs, errors) = parse_name_type_pairs(elements.into_iter());
        assert_eq!(errors.len(), 1);
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
