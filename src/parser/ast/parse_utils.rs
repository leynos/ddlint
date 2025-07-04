//! Shared parsing utilities for AST helpers.
//!
//! This module contains small functions reused by multiple AST nodes when
//! extracting typed data from the CST.

use rowan::{NodeOrToken, SyntaxElement, TextRange};

use super::skip_whitespace_and_comments;
use crate::{DdlogLanguage, SyntaxKind};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Delim {
    Paren,
    Angle,
    Bracket,
    Brace,
}

/// Track delimiter nesting using a stack.
///
/// This structure allows the parser to support new delimiter pairs without
/// additional counters and ensures they close in the correct order.
#[derive(Default)]
struct DelimStack(Vec<Delim>);

/// An error emitted when a closing token does not match the expected
/// delimiter.
///
/// During parameter parsing the parser maintains a stack of opening
/// delimiters as described in `docs/function-parsing-design.md`. If a
/// closing token arrives before the matching delimiter, it is recorded as a
/// `DelimiterError` so callers can report the unexpected character along with
/// its span.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) struct DelimiterError {
    expected: Delim,
    found: SyntaxKind,
    span: TextRange,
}

impl std::fmt::Display for DelimiterError {
    /// Formats a `DelimiterError` for display, indicating the expected delimiter,
    /// the actual token found, and the location of the error.
    ///
    /// This is used to provide clear error messages when delimiter mismatches
    /// occur during parsing.
    ///
    /// # Examples
    ///
    /// ```no_run
    /// use crate::parser::ast::parse_utils::{DelimiterError, Delim};
    /// use rowan::TextRange;
    /// use crate::parser::SyntaxKind;
    ///
    /// let err = DelimiterError {
    ///     expected: Delim::Angle,
    ///     found: SyntaxKind::T_SHR,
    ///     span: TextRange::new(0.into(), 2.into()),
    /// };
    /// assert_eq!(
    ///     format!("{}", err),
    ///     "expected '>' before '>>' at 0..2"
    /// );
    /// ```
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let expected = match self.expected {
            Delim::Paren => ")",
            Delim::Angle => ">",
            Delim::Bracket => "]",
            Delim::Brace => "}",
        };
        let found = match self.found {
            SyntaxKind::T_RPAREN => ")",
            SyntaxKind::T_GT => ">",
            SyntaxKind::T_SHR => ">>",
            SyntaxKind::T_RBRACKET => "]",
            SyntaxKind::T_RBRACE => "}",
            _ => "?",
        };
        write!(
            f,
            "expected '{}' before '{}' at {:#?}",
            expected, found, self.span
        )
    }
}

impl std::error::Error for DelimiterError {}

impl DelimStack {
    /// Pushes one or more delimiters of the specified type onto the stack.
    ///
    /// This increases the nesting level for the given delimiter by the specified count.
    ///
    /// # Examples
    ///
    /// ```no_run
    /// let mut stack = DelimStack::default();
    /// stack.open(Delim::Paren, 2);
    /// assert_eq!(stack.0, vec![Delim::Paren, Delim::Paren]);
    /// ```
    fn open(&mut self, delim: Delim, count: usize) {
        for _ in 0..count {
            self.0.push(delim);
        }
    }

    /// Attempt to close `count` instances of `delim`.
    ///
    /// Returns the number of delimiters successfully closed so that callers
    /// can detect mismatches.
    fn close(&mut self, delim: Delim, count: usize) -> usize {
        let mut closed = 0;
        for _ in 0..count {
            match self.0.pop() {
                Some(d) if d == delim => closed += 1,
                Some(d) => {
                    self.0.push(d);
                    break;
                }
                None => break,
            }
        }
        closed
    }

    fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

fn open_and_push(
    token: &rowan::SyntaxToken<DdlogLanguage>,
    buf: &mut String,
    stack: &mut DelimStack,
    delim: Delim,
    count: usize,
) {
    stack.open(delim, count);
    buf.push_str(token.text());
}

fn close_and_push(
    token: &rowan::SyntaxToken<DdlogLanguage>,
    buf: &mut String,
    stack: &mut DelimStack,
    delim: Delim,
    count: usize,
) -> usize {
    let closed = stack.close(delim, count);
    buf.push_str(token.text());
    closed
}

/// Appends the text of a syntax token to the provided buffer.
///
/// # Example
///
/// ```ignore
/// // Assuming you have a `SyntaxToken<DdlogLanguage>` named `token`:
/// let mut buf = String::new();
/// push(&token, &mut buf);
/// assert!(buf.contains(token.text()));
/// ```
///
/// Note: Constructing a `SyntaxToken<DdlogLanguage>` requires a parsed syntax tree,
/// so a fully self-contained example is not practical here.
fn push(token: &rowan::SyntaxToken<DdlogLanguage>, buf: &mut String) {
    buf.push_str(token.text());
}

/// Appends a `DelimiterError` to the error list for an unexpected delimiter token.
///
/// Records the expected delimiter, the actual token kind found, and the token's text range.
fn push_error(
    errors: &mut Vec<DelimiterError>,
    expected: Delim,
    token: &rowan::SyntaxToken<DdlogLanguage>,
) {
    errors.push(DelimiterError {
        expected,
        found: token.kind(),
        span: token.text_range(),
    });
}

/// Parses `(name: type)` pairs from a parameter or column list, returning both the pairs and any delimiter errors encountered.
///
/// The iterator should yield syntax elements starting at the opening parenthesis of the list. The function tracks delimiter nesting and collects errors for unmatched or unexpected delimiters. Each returned pair consists of the parameter or column name and its associated type as strings.
///
/// # Returns
///
/// A tuple containing:
/// - A vector of `(name, type)` pairs extracted from the list.
/// - A vector of `DelimiterError`s for any unmatched or unexpected delimiters found during parsing.
///
/// # Examples
///
/// ```no_run
/// use parser::ast::parse_utils::{parse_name_type_pairs, DelimiterError};
/// use parser::ast::DdlogLanguage;
/// use parser::syntax::{SyntaxElement, SyntaxKind};
///
/// // Example: parsing a parameter list "(x: int, y: string)"
/// let tokens: Vec<SyntaxElement<DdlogLanguage>> = /* token stream for "(x: int, y: string)" */;
/// let (pairs, errors): (Vec<(String, String)>, Vec<DelimiterError>) = parse_name_type_pairs(tokens.into_iter());
/// assert_eq!(pairs, vec![("x".to_string(), "int".to_string()), ("y".to_string(), "string".to_string())]);
/// assert!(errors.is_empty());
/// ```
#[must_use]
pub(super) fn parse_name_type_pairs<I>(mut iter: I) -> (Vec<(String, String)>, Vec<DelimiterError>)
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
    let mut errors = Vec::new();
    let mut buf = String::new();
    let mut name: Option<String> = None;
    let mut depth = DelimStack::default();
    // Track the outer parameter list separately so that nested
    // parentheses inside types do not terminate parsing.
    let mut outer_parens = 1usize;

    for e in iter.by_ref() {
        match e {
            NodeOrToken::Token(t) => {
                if handle_token(
                    &t,
                    &mut buf,
                    &mut name,
                    &mut pairs,
                    &mut depth,
                    &mut outer_parens,
                    &mut errors,
                ) {
                    break;
                }
            }
            NodeOrToken::Node(n) => buf.push_str(&n.text().to_string()),
        }
    }

    // Capture unmatched closing tokens after the parameter list ends.
    for e in iter {
        if let NodeOrToken::Token(t) = e {
            match t.kind() {
                SyntaxKind::T_RPAREN => push_error(&mut errors, Delim::Paren, &t),
                SyntaxKind::T_RBRACKET => push_error(&mut errors, Delim::Bracket, &t),
                SyntaxKind::T_RBRACE => push_error(&mut errors, Delim::Brace, &t),
                SyntaxKind::T_GT | SyntaxKind::T_SHR => push_error(&mut errors, Delim::Angle, &t),
                _ => break,
            }
        } else {
            break;
        }
    }

    (pairs, errors)
}

/// Processes a single token while parsing name-type pairs, updating buffers, delimiter
/// stack, and error tracking as needed.
///
/// Handles delimiter nesting, finalises pairs on commas, and records delimiter errors.
/// Returns `true` if the outermost parentheses have closed and parsing should stop.
///
/// # Examples
///
/// ```no_run
/// # use ddlog_parser::ast::parse_utils::{handle_token, DelimStack, DelimiterError};
/// # use ddlog_parser::DdlogLanguage;
/// # use rowan::{SyntaxKind, SyntaxToken, TextRange};
/// let mut buf = String::new();
/// let mut name = None;
/// let mut pairs = Vec::new();
/// let mut depth = DelimStack::default();
/// let mut outer_parens = 1;
/// let mut errors = Vec::<DelimiterError>::new();
/// // Suppose `token` is a colon at the outermost level:
/// // handle_token(&token, &mut buf, &mut name, &mut pairs, &mut depth, &mut outer_parens, &mut errors);
/// ```
fn handle_token(
    token: &rowan::SyntaxToken<DdlogLanguage>,
    buf: &mut String,
    name: &mut Option<String>,
    pairs: &mut Vec<(String, String)>,
    depth: &mut DelimStack,
    outer_parens: &mut usize,
    errors: &mut Vec<DelimiterError>,
) -> bool {
    match token.kind() {
        SyntaxKind::T_LPAREN => open_and_push(token, buf, depth, Delim::Paren, 1),
        SyntaxKind::T_RPAREN => {
            if depth.close(Delim::Paren, 1) == 0 {
                *outer_parens = outer_parens.saturating_sub(1);
                finalize_pair(name, buf, pairs);
                return *outer_parens == 0;
            }
            push(token, buf);
        }
        SyntaxKind::T_LT => open_and_push(token, buf, depth, Delim::Angle, 1),
        SyntaxKind::T_GT => {
            if close_and_push(token, buf, depth, Delim::Angle, 1) < 1 {
                push_error(errors, Delim::Angle, token);
            }
        }
        SyntaxKind::T_SHL => open_and_push(token, buf, depth, Delim::Angle, 2),
        SyntaxKind::T_SHR => {
            if close_and_push(token, buf, depth, Delim::Angle, 2) < 2 {
                push_error(errors, Delim::Angle, token);
            }
        }
        SyntaxKind::T_LBRACKET => open_and_push(token, buf, depth, Delim::Bracket, 1),
        SyntaxKind::T_RBRACKET => {
            if close_and_push(token, buf, depth, Delim::Bracket, 1) < 1 {
                push_error(errors, Delim::Bracket, token);
            }
        }
        SyntaxKind::T_LBRACE => open_and_push(token, buf, depth, Delim::Brace, 1),
        SyntaxKind::T_RBRACE => {
            if close_and_push(token, buf, depth, Delim::Brace, 1) < 1 {
                push_error(errors, Delim::Brace, token);
            }
        }
        SyntaxKind::T_COMMA if depth.is_empty() && *outer_parens == 1 => {
            finalize_pair(name, buf, pairs);
        }
        SyntaxKind::T_COLON if depth.is_empty() && *outer_parens == 1 => {
            *name = Some(buf.trim().to_string());
            buf.clear();
        }
        _ => push(token, buf),
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

    /// Tests the extraction of name-type pairs from function and relation parameter lists.
    ///
    /// This parameterised test verifies that `parse_name_type_pairs` correctly parses
    /// parameter declarations of the form `(name: type)` from various function and relation
    /// signatures, including cases with nested delimiters, missing colons, and empty parameter
    /// lists. It asserts that the extracted pairs match the expected output and that no
    /// delimiter errors are reported.
    ///
    /// # Examples
    ///
    /// ```no_run
    /// name_type_pairs(
    ///     "function f(a: u32, b: string) {}",
    ///     vec![("a".into(), "u32".into()), ("b".into(), "string".into())],
    ///     tokens_for,
    /// );
    /// ```
    #[rstest]
    #[case("function f(a: u32, b: string) {}", vec![("a".into(), "u32".into()), ("b".into(), "string".into())])]
    #[case(
    "input relation R(id: u32, name: string)",
    vec![("id".into(), "u32".into()), ("name".into(), "string".into())]
    )]
    #[case("function wrap(t: Option<(u32, string)>) {}", vec![("t".into(), "Option<(u32, string)>".into())])]
    #[case("function g(m: Map<string, u64>) {}", vec![("m".into(), "Map<string, u64>".into())])]
    #[case(
    "function nested(p: Vec<Map<string, Vec<u8>>>) {}",
    vec![("p".into(), "Vec<Map<string, Vec<u8>>>".into())]
    )]
    #[case("function array(a: [Vec<u32>]) {}", vec![("a".into(), "[Vec<u32>]".into())])]
    #[case("function nested_vec(v: Vec<Vec<u8>>) {}", vec![("v".into(), "Vec<Vec<u8>>".into())])]
    #[case("function weird(x: Vec<<u8>>) {}", vec![("x".into(), "Vec<<u8>>".into())])]
    #[case("function empty() {}", Vec::new())]
    #[case("function missing(a u32, b: bool) {}", vec![("b".into(), "bool".into())])]
    fn name_type_pairs(
        #[case] src: &str,
        #[case] expected: Vec<(String, String)>,
        #[with(src)] tokens_for: Vec<SyntaxElement<DdlogLanguage>>,
    ) {
        let _ = src;
        let elements = tokens_for;
        let (result, errors) = parse_name_type_pairs(elements.into_iter());
        assert!(errors.is_empty());
        assert_eq!(result, expected);
    }

    #[test]
    fn unmatched_shift_errors() {
        let src = "function bad(x: Vec<u8>>): bool {}";
        let elements = tokens_for(src);
        let (_pairs, errors) = parse_name_type_pairs(elements.into_iter());
        assert_eq!(errors.len(), 1);
    }

    #[test]
    fn unmatched_bracket_error() {
        let src = "function bad(x: Vec<u8>], y: u32) {}";
        let elements = tokens_for(src);
        let (_pairs, errors) = parse_name_type_pairs(elements.into_iter());
        assert_eq!(errors.len(), 1);
    }

    #[test]
    fn unmatched_brace_error() {
        let src = "function bad(x: u32}, y: bool) {}";
        let elements = tokens_for(src);
        let (_pairs, errors) = parse_name_type_pairs(elements.into_iter());
        assert_eq!(errors.len(), 1);
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
