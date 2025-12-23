//! Parameter builder helpers.
//!
//! Provides the builder used by parameter parsing once a name token has been
//! collected. `ParameterBuilder` consumes the remaining token stream to parse
//! the type annotation, validates missing-name/type cases, and records errors.
//! `ProcessingContext` holds the shared iterator and error sink so the builder
//! can report diagnostics while returning the final name/type pair.

use rowan::{SyntaxElement, TextRange, TextSize};

use crate::{DdlogLanguage, SyntaxKind};

use super::super::super::skip_whitespace_and_comments;
use super::super::errors::ParseError;
use super::super::type_expr::parse_type_expr;
use super::ParameterParsingState;

/// Shared parsing context for parameter processing.
pub(super) struct ProcessingContext<'a, I>
where
    I: Iterator<Item = SyntaxElement<DdlogLanguage>>,
{
    pub(super) iter: &'a mut std::iter::Peekable<I>,
    pub(super) errors: &'a mut Vec<ParseError>,
}

pub(super) struct ParameterBuilder<'a, I>
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
    pub(super) fn new(state: ParameterParsingState, ctx: ProcessingContext<'a, I>) -> Self {
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

    pub(super) fn build(mut self) -> Option<(String, String)> {
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
        self.report_missing_colon();
        while let Some(element) = self.iter.peek() {
            match element.kind() {
                SyntaxKind::T_COMMA => {
                    // Consume ',' so the caller proceeds to the next parameter.
                    self.iter.next();
                    break;
                }
                SyntaxKind::T_LBRACE | SyntaxKind::T_RPAREN => {
                    // Do NOT consume '{' or ')'; leave it for the outer loop to terminate cleanly.
                    break;
                }
                _ => {
                    // Consume junk until reaching a delimiter.
                    self.iter.next();
                }
            }
        }
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

    fn parse_type(&mut self) -> String {
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
        let missing_name = self.name.is_empty() && !ty.is_empty();
        let missing_type = !self.name.is_empty() && ty.is_empty();
        if missing_name && let Some(span) = self.span {
            self.errors.push(ParseError::MissingName { span });
        }
        if missing_type && let Some(span) = self.span {
            self.errors.push(ParseError::MissingType { span });
        }
    }
}

fn create_text_span(start_pos: Option<TextSize>, end_pos: Option<TextSize>) -> Option<TextRange> {
    start_pos
        .zip(end_pos)
        .map(|(start, end)| TextRange::new(start, end))
}
