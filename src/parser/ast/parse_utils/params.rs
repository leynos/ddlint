//! Parameter parsing utilities.
//!
//! Extracts name–type pairs from parameter lists. The iterator should be
//! positioned at the function or relation name so parsing begins at the
//! opening `(`. Emits `MissingColon`, `MissingName`, `MissingType`, and
//! delimiter errors when the list is malformed.

use rowan::{SyntaxElement, TextRange, TextSize};

use crate::{DdlogLanguage, SyntaxKind};

use super::super::skip_whitespace_and_comments;
use super::type_expr::parse_type_expr;
use super::{
    errors::{Delim, ParseError},
    token_utils::{is_trivia, record_delimiter_error},
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
    ParameterBuilder::new(state, ProcessingContext::new(iter, errors)).build()
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
    let (mut pairs, terminated) = parse_parameter_list(&mut iter, &mut errors);
    report_unclosed_parameter_list(terminated, open_span, &mut pairs, &mut errors);
    collect_trailing_delimiter_errors(iter, &mut errors);
    (pairs, errors)
}

fn report_unclosed_parameter_list(
    terminated: bool,
    open_span: Option<TextRange>,
    pairs: &mut Vec<(String, String)>,
    errors: &mut Vec<ParseError>,
) {
    if terminated {
        return;
    }
    if let Some(span) = open_span {
        pairs.clear();
        errors.push(ParseError::UnclosedDelimiter {
            delimiter: ')',
            span,
        });
    }
}

fn collect_parameter_name<I>(iter: &mut std::iter::Peekable<I>) -> ParameterParsingState
where
    I: Iterator<Item = SyntaxElement<DdlogLanguage>>,
{
    let mut state = ParameterParsingState::new();

    while iter.peek().is_some() {
        if skip_trivia_element(iter) {
            continue;
        }
        if should_stop_collecting(iter, &mut state) {
            break;
        }
    }

    state
}

fn skip_trivia_element<I>(iter: &mut std::iter::Peekable<I>) -> bool
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

fn should_stop_collecting<I>(
    iter: &mut std::iter::Peekable<I>,
    state: &mut ParameterParsingState,
) -> bool
where
    I: Iterator<Item = SyntaxElement<DdlogLanguage>>,
{
    process_parameter_token(iter, state)
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
        kind if is_parameter_terminator(kind) => true,
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

fn is_parameter_terminator(kind: SyntaxKind) -> bool {
    matches!(kind, SyntaxKind::T_COMMA | SyntaxKind::T_RPAREN)
}

fn create_text_span(start_pos: Option<TextSize>, end_pos: Option<TextSize>) -> Option<TextRange> {
    match (start_pos, end_pos) {
        (Some(start), Some(end)) => Some(TextRange::new(start, end)),
        _ => None,
    }
}

/// Shared parsing context for parameter processing.
struct ProcessingContext<'a, I>
where
    I: Iterator<Item = SyntaxElement<DdlogLanguage>>,
{
    iter: &'a mut std::iter::Peekable<I>,
    errors: &'a mut Vec<ParseError>,
}

impl<'a, I> ProcessingContext<'a, I>
where
    I: Iterator<Item = SyntaxElement<DdlogLanguage>>,
{
    fn new(iter: &'a mut std::iter::Peekable<I>, errors: &'a mut Vec<ParseError>) -> Self {
        Self { iter, errors }
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
    fn new(state: ParameterParsingState, ctx: ProcessingContext<'a, I>) -> Self {
        let ParameterParsingState {
            name_buf,
            found_colon,
            start_pos,
            end_pos,
        } = state;
        let span = create_text_span(start_pos, end_pos);
        let name = name_buf.trim().to_string();
        let ProcessingContext { iter, errors } = ctx;
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
        let ty = self.parse_type(0);
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
        self.report_missing_colon();
        consume_until_delimiter(self.iter);
        true
    }

    fn report_missing_colon(&mut self) {
        if let Some(span) = self.span {
            self.errors.push(ParseError::MissingColon {
                message: "Expected ':' after name, but found ',' or ')' instead".to_string(),
                span,
            });
        }
    }

    fn parse_type(&mut self, _min_bp: u8) -> String {
        skip_whitespace_and_comments(self.iter);
        if matches!(
            self.iter.peek().map(SyntaxElement::kind),
            Some(SyntaxKind::T_COMMA | SyntaxKind::T_RPAREN | SyntaxKind::T_LBRACE)
        ) {
            String::new()
        } else {
            let (ty, mut errs) = parse_type_expr(self.iter);
            self.errors.append(&mut errs);
            ty
        }
    }

    fn validate_parameter(&mut self, ty: &str) {
        self.report_missing_name_if_needed(ty);
        self.report_missing_type_if_needed(ty);
    }

    fn report_missing_name_if_needed(&mut self, ty: &str) {
        if !self.should_report_missing_name(ty) {
            return;
        }
        if let Some(span) = self.span {
            self.errors.push(ParseError::MissingName { span });
        }
    }

    fn report_missing_type_if_needed(&mut self, ty: &str) {
        if !self.should_report_missing_type(ty) {
            return;
        }
        if let Some(span) = self.span {
            self.errors.push(ParseError::MissingType { span });
        }
    }

    fn should_report_missing_name(&self, ty: &str) -> bool {
        self.name.is_empty() && !ty.is_empty()
    }

    fn should_report_missing_type(&self, ty: &str) -> bool {
        !self.name.is_empty() && ty.is_empty()
    }
}

fn consume_until_delimiter<I>(iter: &mut std::iter::Peekable<I>)
where
    I: Iterator<Item = SyntaxElement<DdlogLanguage>>,
{
    while let Some(element) = iter.peek() {
        let kind = element.kind();
        if is_parameter_separator(kind) {
            // Consume ',' so the caller proceeds to the next parameter.
            iter.next();
            break;
        }
        if is_parameter_list_terminator(kind) {
            // Do NOT consume '{' or ')'; leave it for the outer loop to terminate cleanly.
            break;
        }
        // Consume junk until reaching a delimiter.
        iter.next();
    }
}

fn is_parameter_list_terminator(kind: SyntaxKind) -> bool {
    matches!(kind, SyntaxKind::T_LBRACE | SyntaxKind::T_RPAREN)
}

fn is_parameter_separator(kind: SyntaxKind) -> bool {
    matches!(kind, SyntaxKind::T_COMMA)
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
