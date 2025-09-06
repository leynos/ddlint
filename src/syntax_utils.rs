//!
//! Utilities for traversing the syntax tree.
//!
//! These helpers provide small parsing-related operations that work
//! on `rowan` syntax elements without constructing additional nodes.
//! They are shared by AST wrappers when extracting text from the CST.

use crate::{DdlogLanguage, SyntaxKind};
use rowan::SyntaxElement;

/// Skips to the first `(` then collects comma-separated substrings while
/// preserving nested parentheses.
///
/// # Examples
///
/// ```rust,ignore
/// use ddlint::{parse, syntax_utils::parse_parenthesized_list};
///
/// let src = "relation R(x: u32) // context to build a CST\nfoo(bar, baz)";
/// let parsed = parse(src);
/// let elements = parsed.root().syntax().children_with_tokens();
/// let result = parse_parenthesized_list(elements);
/// assert_eq!(result, vec!["bar".into(), "baz".into()]);
/// ```
#[must_use]
pub fn parse_parenthesized_list(
    tokens: impl Iterator<Item = SyntaxElement<DdlogLanguage>>,
) -> Vec<String> {
    let inner = extract_parenthesized(tokens);
    split_top_level(&inner)
        .into_iter()
        .filter(|s| !s.is_empty())
        .map(String::from)
        .collect()
}

fn extract_parenthesized(tokens: impl Iterator<Item = SyntaxElement<DdlogLanguage>>) -> String {
    use crate::parser::ast::parse_utils::extract_delimited as inner;

    let mut iter = tokens.peekable();
    inner(&mut iter, SyntaxKind::T_LPAREN, SyntaxKind::T_RPAREN).unwrap_or_else(|err| err.collected)
}

fn split_top_level(s: &str) -> Vec<&str> {
    let mut parts = Vec::new();
    let mut depth = 0usize;
    let mut start = 0usize;
    for (i, c) in s.char_indices() {
        match c {
            '(' => depth += 1,
            ')' if depth > 0 => depth -= 1,
            ',' if depth == 0 => {
                let segment = s.get(start..i).unwrap_or("");
                parts.push(segment.trim());
                start = i + 1;
            }
            _ => {}
        }
    }
    if let Some(segment) = s.get(start..) {
        parts.push(segment.trim());
    }
    parts
}
