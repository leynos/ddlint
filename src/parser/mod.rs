//! Chumsky-based parser producing a rowan CST.
//!
//! This module contains the entry point for parsing `DDlog` source code.
//! The parser tokenises the input and wraps tokens into a `rowan::GreenNode`,
//! with support for parsing imports, typedefs, relations, indexes, functions,
//! and rules. It lays down the framework for integrating `chumsky` combinators
//! and error recovery in later stages.

use crate::tokenize;

#[macro_use]
mod lexer_helpers;

mod token_stream;

mod span_collector;

mod span_scanner;
use span_scanner::parse_tokens;
mod cst_builder;
use cst_builder::build_green_tree;
pub mod expression;
pub use cst_builder::{Parsed, ParsedSpans};

/// Parse the provided source string.
///
/// The function tokenises the source using [`tokenize`], then uses a minimal
/// `chumsky` parser to wrap those tokens into a CST. Syntactic error recovery
/// will insert `N_ERROR` nodes when grammar rules fail once they exist.
///
/// # Examples
///
/// ```rust,no_run
/// use ddlint::parse;
///
/// let parsed = parse("input relation R(x: u32);");
/// assert!(parsed.errors().is_empty());
/// assert_eq!(parsed.root().relations().len(), 1);
/// ```
#[must_use]
pub fn parse(src: &str) -> Parsed {
    let tokens = tokenize(src);
    let (spans, errors) = parse_tokens(&tokens, src);

    let green = build_green_tree(&tokens, src, &spans);
    let root = ast::Root::from_green(green.clone());

    Parsed::new(green, root, errors)
}

pub mod ast;

#[cfg(test)]
mod tests {
    mod expression;
    mod expression_integration;
    mod parser;
    use super::token_stream::TokenStream;
    use crate::{SyntaxKind, tokenize};
    use rstest::rstest;

    /// Tests that `skip_until` advances the token stream cursor past the specified span end.
    ///
    /// # Examples
    ///
    /// ```no_run
    /// let src = "import foo\n";
    /// let tokens = tokenize(src);
    /// let mut stream = TokenStream::new(&tokens, src);
    /// let end = stream.line_end(0);
    /// stream.skip_until(end);
    /// assert_eq!(stream.cursor(), tokens.len());
    /// ```
    #[rstest]
    fn skip_until_advances_past_span() {
        let src = "import foo\n";
        let tokens = tokenize(src);
        let mut stream = TokenStream::new(&tokens, src);
        let end = stream.line_end(0);
        stream.skip_until(end);
        assert_eq!(stream.cursor(), tokens.len());
    }

    /// Tests that `TokenStream::line_end` returns the position immediately after the end of the current line.
    ///
    /// # Examples
    ///
    /// ```no_run
    /// let src = "typedef A = string\nnext";
    /// let tokens = tokenize(src);
    /// let stream = TokenStream::new(&tokens, src);
    /// let start = 1; // token after 'typedef'
    /// let end = stream.line_end(start);
    /// let newline = src.find('\n').unwrap_or_else(|| panic!("newline missing"));
    /// assert_eq!(end, newline + 1);
    /// ```
    #[rstest]
    fn line_end_returns_span_end() {
        let src = "typedef A = string\nnext";
        let tokens = tokenize(src);
        let stream = TokenStream::new(&tokens, src);
        let start = 1; // token after 'typedef'
        let end = stream.line_end(start);
        let newline = src.find('\n').unwrap_or_else(|| panic!("newline missing"));
        assert_eq!(end, newline + 1);
    }

    /// Tests that `skip_ws_inline` correctly skips inline whitespace tokens in the token stream.
    ///
    /// # Examples
    ///
    /// ```no_run
    /// let src = "extern    type Foo";
    /// let tokens = tokenize(src);
    /// let mut stream = TokenStream::new(&tokens, src);
    /// stream.advance();
    /// stream.skip_ws_inline();
    /// assert!(matches!(
    ///     stream.peek().map(|t| t.0),
    ///     Some(SyntaxKind::K_TYPE)
    /// ));
    /// ```
    #[rstest]
    fn skip_ws_inline_skips_spaces() {
        let src = "extern    type Foo";
        let tokens = tokenize(src);
        let mut stream = TokenStream::new(&tokens, src);
        stream.advance();
        stream.skip_ws_inline();
        assert!(matches!(
            stream.peek().map(|t| t.0),
            Some(SyntaxKind::K_TYPE)
        ));
    }

    /// Tests that `line_end` returns the length of the source string when called with an out-of-bounds index.
    ///
    /// # Examples
    ///
    /// ```no_run
    /// let src = "typedef A = string\n";
    /// let tokens = tokenize(src);
    /// let stream = TokenStream::new(&tokens, src);
    /// let start = tokens.len();
    /// assert_eq!(stream.line_end(start), src.len());
    /// ```
    #[rstest]
    fn line_end_out_of_bounds_returns_len() {
        let src = "typedef A = string\n";
        let tokens = tokenize(src);
        let stream = TokenStream::new(&tokens, src);
        let start = tokens.len();
        assert_eq!(stream.line_end(start), src.len());
    }
}
