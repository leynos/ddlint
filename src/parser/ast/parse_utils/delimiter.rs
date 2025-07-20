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
#[inline]
pub(crate) fn paren_block_span() -> impl Parser<SyntaxKind, Span, Error = Simple<SyntaxKind>> + Clone
{
    balanced_block(SyntaxKind::T_LPAREN, SyntaxKind::T_RPAREN).map_with_span(|(), sp: Span| sp)
}

#[must_use]
pub fn extract_parenthesized<I>(
    iter: &mut std::iter::Peekable<I>,
    open_kind: SyntaxKind,
    close_kind: SyntaxKind,
) -> Option<String>
where
    I: Iterator<Item = SyntaxElement<DdlogLanguage>>,
{
    advance_to_delimiter(iter, open_kind)?;
    collect_balanced_content(iter, open_kind, close_kind)
}

fn advance_to_delimiter<I>(iter: &mut std::iter::Peekable<I>, open_kind: SyntaxKind) -> Option<()>
where
    I: Iterator<Item = SyntaxElement<DdlogLanguage>>,
{
    for e in iter.by_ref() {
        if e.kind() == open_kind {
            return Some(());
        }
    }
    None
}

fn collect_balanced_content<I>(
    iter: &mut std::iter::Peekable<I>,
    open_kind: SyntaxKind,
    close_kind: SyntaxKind,
) -> Option<String>
where
    I: Iterator<Item = SyntaxElement<DdlogLanguage>>,
{
    let mut depth = 1usize;
    let mut buf = String::new();

    for e in iter.by_ref() {
        if process_token(e, open_kind, close_kind, &mut depth, &mut buf) {
            return Some(buf);
        }
    }
    None
}

fn process_token(
    e: SyntaxElement<DdlogLanguage>,
    open_kind: SyntaxKind,
    close_kind: SyntaxKind,
    depth: &mut usize,
    buf: &mut String,
) -> bool {
    match e.kind() {
        k if k == open_kind => handle_opening_delimiter(e, depth, buf),
        k if k == close_kind => {
            if handle_closing_delimiter(e, depth, buf) {
                return true;
            }
        }
        _ => buf.push_str(&extract_element_text(e)),
    }
    false
}

fn handle_opening_delimiter(e: SyntaxElement<DdlogLanguage>, depth: &mut usize, buf: &mut String) {
    *depth += 1;
    buf.push_str(&extract_element_text(e));
}

fn handle_closing_delimiter(
    e: SyntaxElement<DdlogLanguage>,
    depth: &mut usize,
    buf: &mut String,
) -> bool {
    *depth -= 1;
    if *depth == 0 {
        return true;
    }
    buf.push_str(&extract_element_text(e));
    false
}

fn extract_element_text(e: SyntaxElement<DdlogLanguage>) -> String {
    match e {
        SyntaxElement::Token(t) => t.text().to_string(),
        SyntaxElement::Node(n) => n.text().to_string(),
    }
}
