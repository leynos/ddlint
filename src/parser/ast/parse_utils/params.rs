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

fn find_opening_paren<I>(iter: &mut std::iter::Peekable<I>)
where
    I: Iterator<Item = SyntaxElement<DdlogLanguage>>,
{
    for e in iter {
        if e.kind() == SyntaxKind::T_LPAREN {
            break;
        }
    }
}

fn should_stop_parsing<I>(iter: &mut std::iter::Peekable<I>) -> bool
where
    I: Iterator<Item = SyntaxElement<DdlogLanguage>>,
{
    use rowan::NodeOrToken;
    match iter.peek() {
        Some(NodeOrToken::Token(t)) if t.kind() == SyntaxKind::T_RPAREN => {
            iter.next();
            true
        }
        None => true,
        _ => false,
    }
}

fn parse_single_parameter<I>(
    iter: &mut std::iter::Peekable<I>,
    errors: &mut Vec<ParseError>,
) -> Option<(String, String)>
where
    I: Iterator<Item = SyntaxElement<DdlogLanguage>>,
{
    let (name, found_colon, span) = collect_parameter_name(iter);
    finalise_parameter(name, found_colon, span, iter, errors)
}

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
) -> Vec<(String, String)>
where
    I: Iterator<Item = SyntaxElement<DdlogLanguage>>,
{
    let mut pairs = Vec::new();
    loop {
        skip_whitespace_and_comments(iter);
        if should_stop_parsing(iter) {
            break;
        }
        if let Some(pair) = parse_single_parameter(iter, errors) {
            pairs.push(pair);
        }
        skip_whitespace_and_comments(iter);
        if handle_parameter_separator(iter) {
            break;
        }
    }
    pairs
}

fn collect_trailing_delimiter_errors<I>(iter: I, errors: &mut Vec<ParseError>)
where
    I: Iterator<Item = SyntaxElement<DdlogLanguage>>,
{
    use rowan::NodeOrToken;
    for e in iter {
        if let NodeOrToken::Token(t) = e {
            match t.kind() {
                SyntaxKind::T_RPAREN => push_error(errors, Delim::Paren, &t),
                SyntaxKind::T_RBRACKET => push_error(errors, Delim::Bracket, &t),
                SyntaxKind::T_RBRACE => push_error(errors, Delim::Brace, &t),
                SyntaxKind::T_GT | SyntaxKind::T_SHR => push_error(errors, Delim::Angle, &t),
                _ => break,
            }
        } else {
            break;
        }
    }
}

pub(crate) fn parse_name_type_pairs<I>(iter: I) -> (Vec<(String, String)>, Vec<ParseError>)
where
    I: Iterator<Item = SyntaxElement<DdlogLanguage>>,
{
    let mut iter = iter.peekable();
    find_opening_paren(&mut iter);
    let mut errors = Vec::new();
    let pairs = parse_parameter_list(&mut iter, &mut errors);
    collect_trailing_delimiter_errors(iter, &mut errors);
    (pairs, errors)
}

fn collect_parameter_name<I>(iter: &mut std::iter::Peekable<I>) -> (String, bool, Option<TextRange>)
where
    I: Iterator<Item = SyntaxElement<DdlogLanguage>>,
{
    let mut state = ParameterParsingState::new();

    while let Some(peeked) = iter.peek() {
        if is_trivia(peeked) {
            iter.next();
            continue;
        }

        if process_parameter_token(iter, &mut state) {
            break;
        }
    }

    let span = create_text_span(state.start_pos, state.end_pos);
    (state.name_buf.trim().to_string(), state.found_colon, span)
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

fn create_text_span(start_pos: Option<TextSize>, end_pos: Option<TextSize>) -> Option<TextRange> {
    match (start_pos, end_pos) {
        (Some(start), Some(end)) => Some(TextRange::new(start, end)),
        _ => None,
    }
}

struct ParameterBuilder<'a, I>
where
    I: Iterator<Item = SyntaxElement<DdlogLanguage>>,
{
    name: String,
    found_colon: bool,
    span: Option<TextRange>,
    iter: &'a mut std::iter::Peekable<I>,
    errors: &'a mut Vec<ParseError>,
}

impl<'a, I> ParameterBuilder<'a, I>
where
    I: Iterator<Item = SyntaxElement<DdlogLanguage>>,
{
    fn new(
        name: String,
        found_colon: bool,
        span: Option<TextRange>,
        iter: &'a mut std::iter::Peekable<I>,
        errors: &'a mut Vec<ParseError>,
    ) -> Self {
        Self {
            name,
            found_colon,
            span,
            iter,
            errors,
        }
    }

    fn build(mut self) -> Option<(String, String)> {
        if self.handle_missing_colon() {
            return None;
        }
        let ty = self.parse_type();
        self.validate_parameter(&ty);
        if self.name.is_empty() || ty.is_empty() {
            None
        } else {
            Some((self.name, ty))
        }
    }

    fn handle_missing_colon(&mut self) -> bool {
        if self.found_colon {
            return false;
        }
        if let Some(s) = self.span {
            self.errors.push(ParseError::MissingColon {
                message: "Expected ':' after name, but found ',' or ')' instead".to_string(),
                span: s,
            });
        }
        while let Some(e) = self.iter.peek() {
            match e.kind() {
                SyntaxKind::T_COMMA => {
                    self.iter.next();
                    break;
                }
                SyntaxKind::T_RPAREN => {
                    self.iter.next();
                    return true;
                }
                _ => {
                    self.iter.next();
                }
            }
        }
        true
    }

    fn parse_type(&mut self) -> String {
        skip_whitespace_and_comments(self.iter);
        let (ty, mut errs) = parse_type_expr(self.iter);
        self.errors.append(&mut errs);
        ty
    }

    fn validate_parameter(&mut self, ty: &str) {
        if self.name.is_empty()
            && !ty.is_empty()
            && let Some(s) = self.span
        {
            self.errors.push(ParseError::MissingName { span: s });
        }
        if !self.name.is_empty()
            && ty.is_empty()
            && let Some(s) = self.span
        {
            self.errors.push(ParseError::MissingType { span: s });
        }
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
    ParameterBuilder::new(name, found_colon, span, iter, errors).build()
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
