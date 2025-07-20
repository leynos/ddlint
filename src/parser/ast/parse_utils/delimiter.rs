//! Utilities for parsing balanced delimiters and extracting enclosed content.
//!
//! This module provides functions to parse parenthesised blocks and extract
//! text content from within balanced delimiters, handling nested structures
//! correctly.

use chumsky::prelude::*;
use rowan::SyntaxElement;

use super::super::super::lexer_helpers::balanced_block;
use crate::{DdlogLanguage, Span, SyntaxKind};

/// Parser for a parenthesised block, returning its span.
///
/// # Example
///
/// ```
/// use ddlint::parser::ast::parse_utils::paren_block_span;
/// use ddlint::tokenize;
/// use chumsky::{Parser, Stream};
///
/// let src = "(foo)";
/// let tokens = tokenize(src);
/// let span = paren_block_span()
///     .parse(Stream::from_iter(0..src.len(), tokens.into_iter()))
///     .unwrap();
/// assert_eq!(span.start, 0);
/// ```
#[inline]
pub(crate) fn paren_block_span() -> impl Parser<SyntaxKind, Span, Error = Simple<SyntaxKind>> + Clone
{
    balanced_block(SyntaxKind::T_LPAREN, SyntaxKind::T_RPAREN).map_with_span(|(), sp: Span| sp)
}

/// Error returned when an opening delimiter is never closed.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnclosedDelimiterError {
    pub collected: String,
    pub expected: SyntaxKind,
}

/// Extract the text inside the first matching pair of delimiters.
///
/// The iterator advances until `open_kind` is encountered. Nested delimiters
/// are balanced. If the closing delimiter is missing the partially collected
/// text is returned in [`UnclosedDelimiterError`].
///
/// # Errors
///
/// Returns an [`UnclosedDelimiterError`] if the closing delimiter is missing.
pub fn extract_parenthesized<I>(
    iter: &mut std::iter::Peekable<I>,
    open_kind: SyntaxKind,
    close_kind: SyntaxKind,
) -> Result<String, UnclosedDelimiterError>
where
    I: Iterator<Item = SyntaxElement<DdlogLanguage>>,
{
    skip_to_opening_delimiter(iter, open_kind);

    let mut depth = 1usize;
    let mut buf = String::new();

    for e in iter.by_ref() {
        match process_element(&e, &mut depth, &mut buf, open_kind, close_kind) {
            ElementResult::Continue => {}
            ElementResult::Complete => return Ok(buf),
        }
    }

    Err(UnclosedDelimiterError {
        collected: buf,
        expected: close_kind,
    })
}

enum ElementResult {
    Continue,
    Complete,
}

fn skip_to_opening_delimiter<I>(iter: &mut std::iter::Peekable<I>, open_kind: SyntaxKind)
where
    I: Iterator<Item = SyntaxElement<DdlogLanguage>>,
{
    for e in iter.by_ref() {
        if e.kind() == open_kind {
            break;
        }
    }
}

fn process_element(
    e: &SyntaxElement<DdlogLanguage>,
    depth: &mut usize,
    buf: &mut String,
    open_kind: SyntaxKind,
    close_kind: SyntaxKind,
) -> ElementResult {
    let text = element_text(e);
    match e.kind() {
        k if k == open_kind => {
            *depth += 1;
            buf.push_str(&text);
            ElementResult::Continue
        }
        k if k == close_kind => {
            *depth -= 1;
            if *depth == 0 {
                ElementResult::Complete
            } else {
                buf.push_str(&text);
                ElementResult::Continue
            }
        }
        _ => {
            buf.push_str(&text);
            ElementResult::Continue
        }
    }
}

fn element_text(e: &SyntaxElement<DdlogLanguage>) -> String {
    match e {
        SyntaxElement::Token(t) => t.text().to_string(),
        SyntaxElement::Node(n) => n.text().to_string(),
    }
}
