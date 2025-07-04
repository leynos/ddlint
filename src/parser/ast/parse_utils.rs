//! Shared parsing utilities for AST helpers.
//!
//! This module contains small functions reused by multiple
//! AST nodes when extracting typed data from the CST.

use rowan::{NodeOrToken, SyntaxElement};

use super::skip_whitespace_and_comments;
use crate::{DdlogLanguage, SyntaxKind};

/// Consume `(name: type)` pairs from the provided iterator.
///
/// The iterator should yield the tokens of a parameter or column list
/// starting at the opening parenthesis. The returned vector contains
/// each name and its associated type text.
#[must_use]
pub(super) fn parse_name_type_pairs<I>(mut iter: I) -> Vec<(String, String)>
where
    I: Iterator<Item = SyntaxElement<DdlogLanguage>>,
{
    // Skip to the first '(' to handle leading trivia.
    for e in &mut iter {
        if e.kind() == SyntaxKind::T_LPAREN {
            break;
        }
    }

    let mut pairs = Vec::new();
    let mut buf = String::new();
    let mut name: Option<String> = None;
    let mut depth = 0usize;
    for e in iter {
        match e {
            NodeOrToken::Token(t) => match t.kind() {
                SyntaxKind::T_LPAREN => {
                    depth += 1;
                    buf.push_str(t.text());
                }
                SyntaxKind::T_RPAREN => {
                    if depth == 0 {
                        if let Some(n) = name.take() {
                            let ty = buf.trim();
                            if !ty.is_empty() {
                                pairs.push((n, ty.to_string()));
                            }
                        }
                        break;
                    }
                    depth -= 1;
                    buf.push_str(t.text());
                }
                SyntaxKind::T_COMMA if depth == 0 => {
                    if let Some(n) = name.take() {
                        let ty = buf.trim();
                        if !ty.is_empty() {
                            pairs.push((n, ty.to_string()));
                        }
                    }
                    buf.clear();
                }
                SyntaxKind::T_COLON if depth == 0 => {
                    name = Some(buf.trim().to_string());
                    buf.clear();
                }
                _ => buf.push_str(t.text()),
            },
            NodeOrToken::Node(n) => buf.push_str(&n.text().to_string()),
        }
    }

    pairs
}

/// Parse a trailing type after a colon, stopping at braces or a newline.
#[must_use]
pub(super) fn parse_type_after_colon<I>(iter: &mut std::iter::Peekable<I>) -> Option<String>
where
    I: Iterator<Item = SyntaxElement<DdlogLanguage>>,
{
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
