//!
//! Utilities for traversing the syntax tree.
//!
//! These helpers provide small parsing-related operations that work
//! on `rowan` syntax elements without constructing additional nodes.
//! They are shared by AST wrappers when extracting text from the CST.

use crate::{DdlogLanguage, SyntaxKind};
use rowan::SyntaxElement;

/// Skips to the first `(` then collects comma separated substrings while
/// preserving nested parentheses.
///
/// # Examples
///
/// ```
/// use ddlint::syntax_utils::parse_parenthesized_list;
/// use ddlint::tokenize;
///
/// let src = "foo(bar, baz)";
/// let tokens = tokenize(src);
/// let result = parse_parenthesized_list(tokens.into_iter());
/// assert_eq!(result, vec!["bar".into(), "baz".into()]);
/// ```
#[must_use]
pub fn parse_parenthesized_list(
    mut tokens: impl Iterator<Item = SyntaxElement<DdlogLanguage>>,
) -> Vec<String> {
    // skip until '('
    for e in &mut tokens {
        if e.kind() == SyntaxKind::T_LPAREN {
            break;
        }
    }
    let mut cols = Vec::new();
    let mut buf = String::new();
    let mut depth = 0usize;
    for e in tokens {
        match e {
            SyntaxElement::Token(t) => match t.kind() {
                SyntaxKind::T_LPAREN => {
                    depth += 1;
                    buf.push_str(t.text());
                }
                SyntaxKind::T_RPAREN => {
                    if depth == 0 {
                        let col = buf.trim();
                        if !col.is_empty() {
                            cols.push(col.to_string());
                        }
                        break;
                    }
                    depth -= 1;
                    buf.push_str(t.text());
                }
                SyntaxKind::T_COMMA if depth == 0 => {
                    let col = buf.trim();
                    if !col.is_empty() {
                        cols.push(col.to_string());
                    }
                    buf.clear();
                }
                _ => buf.push_str(t.text()),
            },
            SyntaxElement::Node(n) => buf.push_str(&n.text().to_string()),
        }
    }
    cols
}
