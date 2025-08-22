//! Utilities for parsing balanced delimiters and extracting enclosed content.
//!
//! This module provides functions to parse parenthesised blocks and extract
//! text content from within balanced delimiters, handling nested structures
//! correctly.

use std::fmt;

use rowan::SyntaxElement;

use crate::{DdlogLanguage, SyntaxKind};

/// Error returned when an opening delimiter is never closed.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnclosedDelimiterError {
    pub collected: String,
    pub expected: SyntaxKind,
}

impl fmt::Display for UnclosedDelimiterError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "unclosed delimiter; expected {:#?}, collected: {:?}",
            self.expected, self.collected
        )
    }
}

impl std::error::Error for UnclosedDelimiterError {}

/// Extract the text inside the first matching pair of delimiters.
///
/// The iterator advances until `open_kind` is encountered and then collects all
/// elements until the matching `close_kind`. Nested delimiters are balanced so
/// nested structures are handled correctly. If the closing delimiter is not
/// found the partially collected text is returned in
/// [`UnclosedDelimiterError`].
///
/// # Example
///
/// ```rust,no_run
/// # use ddlint::{parse, SyntaxKind};
/// # use ddlint::parser::ast::parse_utils::extract_delimited;
/// # let mut elems = parse("function f() { (nested (content)) }")
/// #     .root()
/// #     .syntax()
/// #     .children_with_tokens()
/// #     .peekable();
/// let text = extract_delimited(
///     &mut elems,
///     SyntaxKind::T_LPAREN,
///     SyntaxKind::T_RPAREN,
/// ).unwrap();
/// assert_eq!(text, "nested (content)");
/// ```
///
/// # Errors
///
/// Returns an [`UnclosedDelimiterError`] if the closing delimiter is missing.
#[must_use = "discarding the extracted text loses delimiter content"]
pub fn extract_delimited<I>(
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
    let mut ctx = DelimiterParseContext::new(&mut depth, &mut buf, open_kind, close_kind);

    for e in iter.by_ref() {
        match process_element(&e, &mut ctx) {
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

struct DelimiterParseContext<'a> {
    depth: &'a mut usize,
    buf: &'a mut String,
    open_kind: SyntaxKind,
    close_kind: SyntaxKind,
}

impl<'a> DelimiterParseContext<'a> {
    fn new(
        depth: &'a mut usize,
        buf: &'a mut String,
        open_kind: SyntaxKind,
        close_kind: SyntaxKind,
    ) -> Self {
        Self {
            depth,
            buf,
            open_kind,
            close_kind,
        }
    }
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
    ctx: &mut DelimiterParseContext<'_>,
) -> ElementResult {
    let text = element_text(e);
    match e.kind() {
        k if k == ctx.open_kind => {
            *ctx.depth += 1;
            ctx.buf.push_str(text.as_ref());
            ElementResult::Continue
        }
        k if k == ctx.close_kind => handle_close_delimiter(ctx, text.as_ref()),
        _ => {
            ctx.buf.push_str(text.as_ref());
            ElementResult::Continue
        }
    }
}

fn handle_close_delimiter(ctx: &mut DelimiterParseContext<'_>, text: &str) -> ElementResult {
    *ctx.depth -= 1;
    if *ctx.depth == 0 {
        ElementResult::Complete
    } else {
        ctx.buf.push_str(text);
        ElementResult::Continue
    }
}

use std::borrow::Cow;

fn element_text(e: &SyntaxElement<DdlogLanguage>) -> Cow<'_, str> {
    match e {
        SyntaxElement::Token(t) => Cow::Borrowed(t.text()),
        SyntaxElement::Node(n) => Cow::Owned(n.text().to_string()),
    }
}
