//! Output list parsing utilities.
//!
//! Parses comma-separated output relation names following a transformer
//! declaration's colon.

use rowan::SyntaxElement;

use crate::{DdlogLanguage, SyntaxKind};

use super::super::skip_whitespace_and_comments;

pub(crate) fn skip_to_top_level_colon<I>(iter: &mut std::iter::Peekable<I>)
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

fn try_parse_identifier<I>(iter: &mut std::iter::Peekable<I>) -> Option<String>
where
    I: Iterator<Item = SyntaxElement<DdlogLanguage>>,
{
    use rowan::NodeOrToken;

    skip_whitespace_and_comments(iter);
    match iter.next() {
        Some(NodeOrToken::Token(t)) if t.kind() == SyntaxKind::T_IDENT => {
            Some(t.text().to_string())
        }
        _ => None,
    }
}

fn has_comma_separator<I>(iter: &mut std::iter::Peekable<I>) -> bool
where
    I: Iterator<Item = SyntaxElement<DdlogLanguage>>,
{
    use rowan::NodeOrToken;

    skip_whitespace_and_comments(iter);
    if let Some(NodeOrToken::Token(t)) = iter.peek()
        && t.kind() == SyntaxKind::T_COMMA
    {
        iter.next();
        true
    } else {
        false
    }
}

pub(crate) fn parse_output_list<I>(iter: I) -> Vec<String>
where
    I: Iterator<Item = SyntaxElement<DdlogLanguage>>,
{
    let mut iter = iter.peekable();
    skip_to_top_level_colon(&mut iter);

    let mut names = Vec::new();
    while let Some(name) = try_parse_identifier(&mut iter) {
        names.push(name);
        if !has_comma_separator(&mut iter) {
            break;
        }
    }

    names
}

#[cfg(test)]
mod tests;
