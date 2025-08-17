//! Parameter parsing utilities.
//!
//! Extracts name-type pairs from parameter lists, reporting missing
//! names, types, or mismatched delimiters.

use rowan::{SyntaxElement, TextRange, TextSize};

use crate::{DdlogLanguage, SyntaxKind};

use super::super::skip_whitespace_and_comments;
use super::type_expr::parse_type_expr;
use super::{
    errors::{Delim, ParseError},
    token_utils::{is_trivia, push_error},
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

#[cfg(test)]
mod tests;
