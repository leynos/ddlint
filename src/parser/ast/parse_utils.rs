//! Shared parsing utilities for AST helpers.
//!
//! This module contains small functions reused by multiple AST nodes when
//! extracting typed data from the CST.

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
            NodeOrToken::Token(t) => {
                if handle_token(&t, &mut buf, &mut name, &mut pairs, &mut depth) {
                    break;
                }
            }
            NodeOrToken::Node(n) => buf.push_str(&n.text().to_string()),
        }
    }

    pairs
}

/// Handle a single token during name-type pair parsing.
fn handle_token(
    token: &rowan::SyntaxToken<DdlogLanguage>,
    buf: &mut String,
    name: &mut Option<String>,
    pairs: &mut Vec<(String, String)>,
    depth: &mut usize,
) -> bool {
    match token.kind() {
        SyntaxKind::T_LPAREN => {
            *depth += 1;
            buf.push_str(token.text());
        }
        SyntaxKind::T_RPAREN => {
            if *depth == 0 {
                finalize_pair(name, buf, pairs);
                return true; // end of list
            }
            *depth -= 1;
            buf.push_str(token.text());
        }
        SyntaxKind::T_COMMA if *depth == 0 => {
            finalize_pair(name, buf, pairs);
        }
        SyntaxKind::T_COLON if *depth == 0 => {
            *name = Some(buf.trim().to_string());
            buf.clear();
        }
        _ => buf.push_str(token.text()),
    }
    false
}

/// Finalise a name-type pair and add it to the pairs vector.
fn finalize_pair(name: &mut Option<String>, buf: &mut String, pairs: &mut Vec<(String, String)>) {
    if let Some(n) = name.take() {
        let ty = buf.trim();
        if !ty.is_empty() {
            pairs.push((n, ty.to_string()));
        }
    }
    buf.clear();
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::parse;
    use rstest::{fixture, rstest};

    #[fixture]
    fn tokens_for(#[default("function t() {}")] src: &str) -> Vec<SyntaxElement<DdlogLanguage>> {
        let parsed = parse(src);
        let functions = parsed.root().functions();
        functions.first().map_or_else(
            || {
                let relations = parsed.root().relations();
                relations
                    .first()
                    .unwrap_or_else(|| panic!("relation missing in test"))
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
        let func = functions
            .first()
            .unwrap_or_else(|| panic!("function missing"));
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
    #[case("function f(a: u32, b: string) {}", vec![("a".into(), "u32".into()), ("b".into(), "string".into())])]
    #[case(
        "input relation R(id: u32, name: string)",
        vec![("id".into(), "u32".into()), ("name".into(), "string".into())]
    )]
    #[case("function wrap(t: Option<(u32, string)>) {}", vec![("t".into(), "Option<(u32, string)>".into())])]
    #[case("function empty() {}", Vec::new())]
    #[case("function missing(a u32, b: bool) {}", vec![("b".into(), "bool".into())])]
    fn name_type_pairs(
        #[case] src: &str,
        #[case] expected: Vec<(String, String)>,
        #[with(src)] tokens_for: Vec<SyntaxElement<DdlogLanguage>>,
    ) {
        let _ = src;
        let elements = tokens_for;
        let result = parse_name_type_pairs(elements.into_iter());
        assert_eq!(result, expected);
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
