//! Parameter parsing utilities.
//!
//! Extracts name–type pairs from parameter lists. The iterator should be
//! positioned at the function or relation name so parsing begins at the
//! opening `(`. Emits `MissingColon`, `MissingName`, `MissingType`, and
//! delimiter errors when the list is malformed.

use rowan::{SyntaxElement, TextRange, TextSize};

use crate::{DdlogLanguage, SyntaxKind};

use super::super::skip_whitespace_and_comments;
use super::{
    errors::{Delim, ParseError},
    token_utils::{record_delimiter_error, skip_next_trivia},
};

mod builder;

use builder::{ParameterBuilder, ProcessingContext};

struct ParameterParsingState {
    name_buf: String,
    found_colon: bool,
    start_pos: Option<TextSize>,
    end_pos: Option<TextSize>,
}

impl ParameterParsingState {
    fn new() -> Self {
        Self {
            name_buf: String::new(),
            found_colon: false,
            start_pos: None,
            end_pos: None,
        }
    }
}

fn find_opening_paren<I>(iter: &mut std::iter::Peekable<I>) -> Option<TextRange>
where
    I: Iterator<Item = SyntaxElement<DdlogLanguage>>,
{
    for e in iter {
        if e.kind() == SyntaxKind::T_LPAREN {
            return Some(e.text_range());
        }
    }
    None
}

fn parse_single_parameter<I>(
    iter: &mut std::iter::Peekable<I>,
    errors: &mut Vec<ParseError>,
) -> Option<(String, String)>
where
    I: Iterator<Item = SyntaxElement<DdlogLanguage>>,
{
    let state = collect_parameter_name(iter);
    ParameterBuilder::new(state, ProcessingContext { iter, errors }).build()
}

/// Consuming a trailing `,` returns `false` to continue; consuming `)` returns
/// `true` to signal termination. Returns `false` for other tokens.
fn handle_parameter_separator<I>(iter: &mut std::iter::Peekable<I>) -> bool
where
    I: Iterator<Item = SyntaxElement<DdlogLanguage>>,
{
    use rowan::NodeOrToken;
    if let Some(NodeOrToken::Token(t)) = iter.peek() {
        match t.kind() {
            SyntaxKind::T_COMMA => {
                iter.next();
                false
            }
            SyntaxKind::T_RPAREN => {
                iter.next();
                true
            }
            _ => false,
        }
    } else {
        false
    }
}

fn parse_parameter_list<I>(
    iter: &mut std::iter::Peekable<I>,
    errors: &mut Vec<ParseError>,
) -> (Vec<(String, String)>, bool)
where
    I: Iterator<Item = SyntaxElement<DdlogLanguage>>,
{
    use rowan::NodeOrToken;
    let mut pairs = Vec::new();
    let mut terminated_by_rparen = false;
    loop {
        skip_whitespace_and_comments(iter);
        match iter.peek() {
            Some(NodeOrToken::Token(t)) if t.kind() == SyntaxKind::T_RPAREN => {
                iter.next();
                terminated_by_rparen = true;
                break;
            }
            Some(NodeOrToken::Token(t)) if t.kind() == SyntaxKind::T_LBRACE => break,
            None => break,
            _ => {}
        }
        if let Some(pair) = parse_single_parameter(iter, errors) {
            pairs.push(pair);
        }
        skip_whitespace_and_comments(iter);
        if handle_parameter_separator(iter) {
            terminated_by_rparen = true;
            break;
        }
    }
    (pairs, terminated_by_rparen)
}

fn collect_trailing_delimiter_errors<I>(iter: I, errors: &mut Vec<ParseError>)
where
    I: Iterator<Item = SyntaxElement<DdlogLanguage>>,
{
    use rowan::NodeOrToken;
    for e in iter {
        match e {
            NodeOrToken::Token(t) => match t.kind() {
                SyntaxKind::T_WHITESPACE | SyntaxKind::T_COMMENT => {}
                SyntaxKind::T_RPAREN => record_delimiter_error(errors, Delim::Paren, &t),
                SyntaxKind::T_RBRACKET => record_delimiter_error(errors, Delim::Bracket, &t),
                SyntaxKind::T_RBRACE => record_delimiter_error(errors, Delim::Brace, &t),
                SyntaxKind::T_GT | SyntaxKind::T_SHR => {
                    record_delimiter_error(errors, Delim::Angle, &t);
                }
                _ => break,
            },
            // Stop on structural nodes; trailing delimiter errors target tokens only.
            NodeOrToken::Node(_) => break,
        }
    }
}

pub(crate) fn parse_name_type_pairs<I>(iter: I) -> (Vec<(String, String)>, Vec<ParseError>)
where
    I: Iterator<Item = SyntaxElement<DdlogLanguage>>,
{
    let mut iter = iter.peekable();
    let open_span = find_opening_paren(&mut iter);
    if open_span.is_none() {
        return (Vec::new(), Vec::new());
    }
    let mut errors = Vec::new();
    let (pairs, terminated) = parse_parameter_list(&mut iter, &mut errors);
    let (pairs, maybe_error) = report_unclosed_parameter_list(terminated, open_span, pairs);
    if let Some(error) = maybe_error {
        errors.push(error);
    }
    collect_trailing_delimiter_errors(iter, &mut errors);
    (pairs, errors)
}

fn report_unclosed_parameter_list(
    terminated: bool,
    open_span: Option<TextRange>,
    mut pairs: Vec<(String, String)>,
) -> (Vec<(String, String)>, Option<ParseError>) {
    if !terminated && let Some(span) = open_span {
        pairs.clear();
        return (
            pairs,
            Some(ParseError::UnclosedDelimiter {
                delimiter: ')',
                span,
            }),
        );
    }
    (pairs, None)
}

fn collect_parameter_name<I>(iter: &mut std::iter::Peekable<I>) -> ParameterParsingState
where
    I: Iterator<Item = SyntaxElement<DdlogLanguage>>,
{
    let mut state = ParameterParsingState::new();

    while iter.peek().is_some() {
        if skip_next_trivia(iter) {
            continue;
        }
        if process_parameter_token(iter, &mut state) {
            break;
        }
    }

    state
}

fn process_parameter_token<I>(
    iter: &mut std::iter::Peekable<I>,
    state: &mut ParameterParsingState,
) -> bool
where
    I: Iterator<Item = SyntaxElement<DdlogLanguage>>,
{
    use rowan::NodeOrToken;

    let Some(element) = iter.peek().cloned() else {
        return false;
    };
    track_span_for_element(&element, &mut state.start_pos, &mut state.end_pos);
    match element {
        NodeOrToken::Token(t) => process_token(&t, iter, state),
        NodeOrToken::Node(n) => process_node(&n, iter, state),
    }
}

fn process_token<I>(
    token: &rowan::SyntaxToken<DdlogLanguage>,
    iter: &mut std::iter::Peekable<I>,
    state: &mut ParameterParsingState,
) -> bool
where
    I: Iterator<Item = SyntaxElement<DdlogLanguage>>,
{
    match token.kind() {
        SyntaxKind::T_COLON => {
            iter.next();
            state.found_colon = true;
            true
        }
        SyntaxKind::T_COMMA | SyntaxKind::T_RPAREN => true,
        _ => {
            state.name_buf.push_str(token.text());
            iter.next();
            false
        }
    }
}

fn process_node<I>(
    node: &rowan::SyntaxNode<DdlogLanguage>,
    iter: &mut std::iter::Peekable<I>,
    state: &mut ParameterParsingState,
) -> bool
where
    I: Iterator<Item = SyntaxElement<DdlogLanguage>>,
{
    state.name_buf.push_str(&node.text().to_string());
    iter.next();
    false
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
