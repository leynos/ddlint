//! Parsing helpers shared across AST modules.
//!
//! This module provides small utilities for collecting parameter names and
//! types from the CST and for recursively parsing type expressions. Both the
//! `Function` and `Relation` nodes import these helpers so they can share the
//! same logic when interpreting their declarations. See
//! `docs/function-parsing-design.md` for an overview.

use rowan::{NodeOrToken, SyntaxElement, TextRange, TextSize};

use super::super::lexer_helpers::balanced_block;
use super::skip_whitespace_and_comments;
use crate::{DdlogLanguage, Span, SyntaxKind};
use chumsky::prelude::*;

/// Parser for a parenthesised block, returning its span.
///
/// This helper wraps [`balanced_block`] to provide the range covering the
/// entire block. Nested parentheses are handled correctly so callers can skip
/// or collect the text without building an AST node.
///
/// # Examples
///
/// ```rust,no_run
/// use crate::parser::ast::parse_utils::paren_block_span;
/// use crate::tokenize;
/// use chumsky::{Parser, Stream};
///
/// let src = "(foo(bar))";
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

/// Error types produced when parsing name-type pairs.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) enum ParseError {
    /// A closing delimiter did not match the expected opener.
    Delimiter(DelimiterError),
    /// An opening delimiter was not closed before the expression ended.
    UnclosedDelimiter { delimiter: char, span: TextRange },
    /// A parameter name was not followed by a colon.
    MissingColon { message: String, span: TextRange },
    /// A parameter name was not provided.
    MissingName { span: TextRange },
    /// A parameter type was not provided.
    MissingType { span: TextRange },
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Delimiter(err) => write!(f, "{err}"),
            Self::UnclosedDelimiter { delimiter, span } => {
                write!(f, "unclosed delimiter '{delimiter}' at {span:#?}")
            }
            Self::MissingColon { message, span } => write!(f, "{message} at {span:#?}"),
            Self::MissingName { span } => write!(f, "parameter name missing at {span:#?}"),
            Self::MissingType { span } => write!(f, "parameter type missing at {span:#?}"),
        }
    }
}

impl std::error::Error for ParseError {}

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
    errors: &mut Vec<ParseError>,
    expected: Delim,
    token: &rowan::SyntaxToken<DdlogLanguage>,
) {
    errors.push(ParseError::Delimiter(DelimiterError {
        expected,
        found: token.kind(),
        span: token.text_range(),
    }));
}

fn collect_parameter_name<I>(iter: &mut std::iter::Peekable<I>) -> (String, bool, Option<TextRange>)
where
    I: Iterator<Item = SyntaxElement<DdlogLanguage>>,
{
    use rowan::NodeOrToken;

    let mut name_buf = String::new();
    let mut found_colon = false;
    let mut start_pos: Option<TextSize> = None;
    let mut end_pos: Option<TextSize> = None;

    while let Some(e) = iter.peek() {
        match e {
            NodeOrToken::Token(t) => match t.kind() {
                SyntaxKind::T_COLON => {
                    if start_pos.is_none() {
                        start_pos = Some(t.text_range().start());
                    }
                    end_pos = Some(t.text_range().start());
                    iter.next();
                    found_colon = true;
                    break;
                }
                SyntaxKind::T_COMMA | SyntaxKind::T_RPAREN => {
                    if start_pos.is_none() {
                        start_pos = Some(t.text_range().start());
                    }
                    end_pos = Some(t.text_range().start());
                    break;
                }
                _ => {
                    if start_pos.is_none() {
                        start_pos = Some(t.text_range().start());
                    }
                    end_pos = Some(t.text_range().end());
                    name_buf.push_str(t.text());
                    iter.next();
                }
            },
            NodeOrToken::Node(n) => {
                if start_pos.is_none() {
                    start_pos = Some(n.text_range().start());
                }
                end_pos = Some(n.text_range().end());
                name_buf.push_str(&n.text().to_string());
                iter.next();
            }
        }
    }

    let span = match (start_pos, end_pos) {
        (Some(start), Some(end)) => Some(TextRange::new(start, end)),
        _ => None,
    };
    (name_buf.trim().to_string(), found_colon, span)
}

fn finalise_parameter<I>(
    name: String,
    found_colon: bool,
    span: Option<TextRange>,
    iter: &mut std::iter::Peekable<I>,
    errors: &mut Vec<ParseError>,
) -> Option<(String, String)>
where
    I: Iterator<Item = SyntaxElement<DdlogLanguage>>,
{
    if !found_colon {
        if let Some(s) = span {
            errors.push(ParseError::MissingColon {
                message: "Expected ':' after name, but found ',' or ')' instead".to_string(),
                span: s,
            });
        }

        while let Some(e) = iter.peek() {
            match e.kind() {
                SyntaxKind::T_COMMA => {
                    iter.next();
                    break;
                }
                SyntaxKind::T_RPAREN => {
                    iter.next();
                    return None;
                }
                _ => {
                    iter.next();
                }
            }
        }
        return None;
    }

    skip_whitespace_and_comments(iter);
    let (ty, mut errs) = parse_type_expr(iter);
    errors.append(&mut errs);

    if name.is_empty()
        && !ty.is_empty()
        && let Some(s) = span
    {
        errors.push(ParseError::MissingName { span: s });
    }
    if !name.is_empty()
        && ty.is_empty()
        && let Some(s) = span
    {
        errors.push(ParseError::MissingType { span: s });
    }

    if !name.is_empty() && !ty.is_empty() {
        Some((name, ty))
    } else {
        None
    }
}

/// Parses `(name: type)` pairs from a parameter or column list, returning both
/// the pairs and any parse errors encountered.
///
/// The iterator should yield syntax elements starting at the opening parenthesis of the list. The function tracks delimiter nesting and collects errors for unmatched or unexpected delimiters. Each returned pair consists of the parameter or column name and its associated type as strings.
///
/// # Returns
///
/// A tuple containing:
/// - A vector of `(name, type)` pairs extracted from the list.
/// - A vector of [`ParseError`]s for unmatched delimiters or missing colons
///   encountered during parsing.
///
/// # Examples
///
/// ```no_run
/// use parser::ast::parse_utils::{parse_name_type_pairs, ParseError};
/// use parser::ast::DdlogLanguage;
/// use parser::syntax::{SyntaxElement, SyntaxKind};
///
/// // Example: parsing a parameter list "(x: int, y: string)"
/// let tokens: Vec<SyntaxElement<DdlogLanguage>> = /* token stream for "(x: int, y: string)" */;
/// let (pairs, errors): (Vec<(String, String)>, Vec<ParseError>) = parse_name_type_pairs(tokens.into_iter());
/// assert_eq!(pairs, vec![("x".to_string(), "int".to_string()), ("y".to_string(), "string".to_string())]);
/// assert!(errors.is_empty());
/// ```
#[must_use]
pub(super) fn parse_name_type_pairs<I>(iter: I) -> (Vec<(String, String)>, Vec<ParseError>)
where
    I: Iterator<Item = SyntaxElement<DdlogLanguage>>,
{
    use rowan::NodeOrToken;

    let mut iter = iter.peekable();

    // Skip to the opening '('.
    for e in &mut iter {
        if e.kind() == SyntaxKind::T_LPAREN {
            break;
        }
    }

    let mut pairs = Vec::new();
    let mut errors = Vec::new();

    loop {
        skip_whitespace_and_comments(&mut iter);

        match iter.peek() {
            Some(NodeOrToken::Token(t)) if t.kind() == SyntaxKind::T_RPAREN => {
                iter.next();
                break;
            }
            None => break,
            _ => {}
        }

        let (name, found_colon, span) = collect_parameter_name(&mut iter);
        if let Some(pair) = finalise_parameter(name, found_colon, span, &mut iter, &mut errors) {
            pairs.push(pair);
        }

        skip_whitespace_and_comments(&mut iter);
        match iter.peek() {
            Some(NodeOrToken::Token(t)) if t.kind() == SyntaxKind::T_COMMA => {
                iter.next();
            }
            Some(NodeOrToken::Token(t)) if t.kind() == SyntaxKind::T_RPAREN => {
                iter.next();
                break;
            }
            _ => {}
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

/// Skip elements until a colon outside parentheses is reached.
fn skip_to_top_level_colon<I>(iter: &mut std::iter::Peekable<I>)
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

/// Parse a comma separated list of identifiers after a colon.
///
/// This helper is tailored for transformer declarations where the outputs are
/// specified after the input list and a colon.
///
/// # Examples
///
/// ```
/// # use ddlint::parser::ast::parse_utils::parse_output_list;
/// use ddlint::tokenize;
/// let tokens = tokenize("foo(bar: Baz): Out1, Out2");
/// let names = parse_output_list(tokens.into_iter());
/// assert_eq!(names, vec!["Out1", "Out2"]);
/// ```
#[must_use]
pub(super) fn parse_output_list<I>(iter: I) -> Vec<String>
where
    I: Iterator<Item = SyntaxElement<DdlogLanguage>>,
{
    use rowan::NodeOrToken;

    let mut iter = iter.peekable();
    skip_to_top_level_colon(&mut iter);

    let mut names = Vec::new();
    loop {
        skip_whitespace_and_comments(&mut iter);
        match iter.next() {
            Some(NodeOrToken::Token(t)) if t.kind() == SyntaxKind::T_IDENT => {
                names.push(t.text().to_string());
            }
            _ => break,
        }
        skip_whitespace_and_comments(&mut iter);
        match iter.peek() {
            Some(NodeOrToken::Token(t)) if t.kind() == SyntaxKind::T_COMMA => {
                iter.next();
            }
            _ => break,
        }
    }

    names
}

/// Parse a single type expression until a comma or closing parenthesis.
///
/// Nested delimiters are handled recursively so that constructs like
/// `Vec<Map<string, u32>>` are captured correctly. Delimiter mismatches are
/// recorded in the returned [`ParseError`] list. The iterator is left positioned
/// on the token that ended the type expression so the caller can consume the
/// comma or closing parenthesis as appropriate.
#[must_use]
pub(super) fn parse_type_expr<I>(iter: &mut std::iter::Peekable<I>) -> (String, Vec<ParseError>)
where
    I: Iterator<Item = SyntaxElement<DdlogLanguage>>,
{
    use rowan::NodeOrToken;

    let mut buf = String::new();
    let mut errors = Vec::new();
    let mut depth = DelimStack::default();

    while let Some(e) = iter.peek() {
        match e {
            NodeOrToken::Token(t) => match t.kind() {
                SyntaxKind::T_LPAREN => {
                    open_and_push(t, &mut buf, &mut depth, Delim::Paren, 1);
                    iter.next();
                }
                SyntaxKind::T_RPAREN => {
                    if depth.close(Delim::Paren, 1) == 0 {
                        break;
                    }
                    push(t, &mut buf);
                    iter.next();
                }
                SyntaxKind::T_LT => {
                    open_and_push(t, &mut buf, &mut depth, Delim::Angle, 1);
                    iter.next();
                }
                SyntaxKind::T_SHL => {
                    open_and_push(t, &mut buf, &mut depth, Delim::Angle, 2);
                    iter.next();
                }
                SyntaxKind::T_GT => {
                    if close_and_push(t, &mut buf, &mut depth, Delim::Angle, 1) < 1 {
                        push_error(&mut errors, Delim::Angle, t);
                    }
                    iter.next();
                }
                SyntaxKind::T_SHR => {
                    if close_and_push(t, &mut buf, &mut depth, Delim::Angle, 2) < 2 {
                        push_error(&mut errors, Delim::Angle, t);
                    }
                    iter.next();
                }
                SyntaxKind::T_LBRACKET => {
                    open_and_push(t, &mut buf, &mut depth, Delim::Bracket, 1);
                    iter.next();
                }
                SyntaxKind::T_RBRACKET => {
                    if close_and_push(t, &mut buf, &mut depth, Delim::Bracket, 1) < 1 {
                        push_error(&mut errors, Delim::Bracket, t);
                    }
                    iter.next();
                }
                SyntaxKind::T_LBRACE => {
                    open_and_push(t, &mut buf, &mut depth, Delim::Brace, 1);
                    iter.next();
                }
                SyntaxKind::T_RBRACE => {
                    if close_and_push(t, &mut buf, &mut depth, Delim::Brace, 1) < 1 {
                        push_error(&mut errors, Delim::Brace, t);
                    }
                    iter.next();
                }
                SyntaxKind::T_COMMA if depth.is_empty() => break,
                _ => {
                    push(t, &mut buf);
                    iter.next();
                }
            },
            NodeOrToken::Node(n) => {
                let text = n.text().to_string();
                let is_whitespace = text.chars().all(char::is_whitespace);
                let is_comment = n.kind() == SyntaxKind::T_COMMENT;
                if !is_whitespace && !is_comment {
                    buf.push_str(&text);
                }
                iter.next();
            }
        }
    }

    // Report any unclosed delimiters that remain on the stack.
    while let Some(unclosed) = depth.0.pop() {
        let ch = match unclosed {
            Delim::Paren => ')',
            Delim::Angle => '>',
            Delim::Bracket => ']',
            Delim::Brace => '}',
        };
        errors.push(ParseError::UnclosedDelimiter {
            delimiter: ch,
            span: TextRange::empty(0.into()),
        });
    }

    (buf.trim().to_string(), errors)
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
    use crate::parser::ast::AstNode;
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
    #[case(
        "function f(a: u32, b: string) {}",
        vec![("a".into(), "u32".into()), ("b".into(), "string".into())],
        0
    )]
    #[case(
        "input relation R(id: u32, name: string)",
        vec![("id".into(), "u32".into()), ("name".into(), "string".into())],
        0
    )]
    #[case(
        "function wrap(t: Option<(u32, string)>) {}",
        vec![("t".into(), "Option<(u32, string)>".into())],
        0
    )]
    #[case(
        "function g(m: Map<string, u64>) {}",
        vec![("m".into(), "Map<string, u64>".into())],
        0
    )]
    #[case(
        "function nested(p: Vec<Map<string, Vec<u8>>>) {}",
        vec![("p".into(), "Vec<Map<string, Vec<u8>>>".into())],
        0
    )]
    #[case(
        "function array(a: [Vec<u32>]) {}",
        vec![("a".into(), "[Vec<u32>]".into())],
        0
    )]
    #[case(
        "function nested_vec(v: Vec<Vec<u8>>) {}",
        vec![("v".into(), "Vec<Vec<u8>>".into())],
        0
    )]
    #[case(
        "function weird(x: Vec<<u8>>) {}",
        vec![("x".into(), "Vec<<u8>>".into())],
        0
    )]
    #[case("function empty() {}", Vec::new(), 0)]
    #[case(
        "function missing(a u32, b: bool) {}",
        vec![("b".into(), "bool".into())],
        1
    )]
    fn name_type_pairs(
        #[case] src: &str,
        #[case] expected: Vec<(String, String)>,
        #[case] err_count: usize,
        #[with(src)] tokens_for: Vec<SyntaxElement<DdlogLanguage>>,
    ) {
        let _ = src;
        let elements = tokens_for;
        let (result, errors) = parse_name_type_pairs(elements.into_iter());
        assert_eq!(errors.len(), err_count);
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

    #[test]
    fn unclosed_angle_error() {
        let src = "function bad(x: Vec<u32) {}";
        let elements = tokens_for(src);
        let (_pairs, errors) = parse_name_type_pairs(elements.into_iter());
        assert_eq!(errors.len(), 1);
    }

    #[test]
    fn empty_name_error() {
        let src = "function bad(: u32) {}";
        let elements = tokens_for(src);
        let (_pairs, errors) = parse_name_type_pairs(elements.into_iter());
        assert_eq!(errors.len(), 1);
    }

    #[test]
    fn empty_type_error() {
        let src = "function bad(x:) {}";
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
