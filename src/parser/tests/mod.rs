//! Tests for parser components and integration scenarios.

mod collections;
mod control_flow;
mod expression;
mod expression_integration;
mod expression_span;
mod functions;
mod helpers;
mod imports;
mod indexes;
mod numeric_literals;
mod operator_precedence;
mod relations;
mod round_trip;
mod rules;
mod transformers;
mod types;

use super::token_stream::TokenStream;
use crate::{Span, SyntaxKind, test_util::tokenize};
use rstest::{fixture, rstest};

#[fixture]
fn typedef_stream() -> (String, Vec<(SyntaxKind, Span)>) {
    let src = String::from("typedef A = string\nnext");
    let tokens = tokenize(&src);
    (src, tokens)
}

/// Tests that `skip_until` advances the token stream cursor past the specified
/// span end.
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
#[test]
fn skip_until_advances_past_span() {
    let src = "import foo\n";
    let tokens = tokenize(src);
    let mut stream = TokenStream::new(&tokens, src);
    let end = stream.line_end(0);
    stream.skip_until(end);
    assert_eq!(stream.cursor(), tokens.len());
}

/// Tests that `TokenStream::line_end` returns the position immediately after
/// the end of the current line.
///
/// # Examples
///
/// ```no_run
/// let src = "typedef A = string\nnext";
/// let tokens = tokenize(src);
/// let stream = TokenStream::new(&tokens, src);
/// let start = 1; // token after 'typedef'
/// let end = stream.line_end(start);
/// let newline = src.find('\n').expect("newline missing");
/// assert_eq!(end, newline + 1);
/// ```
#[expect(
    clippy::expect_used,
    reason = "tests require a newline span to compare against line_end"
)]
#[rstest]
fn line_end_returns_span_end(typedef_stream: (String, Vec<(SyntaxKind, Span)>)) {
    let (src, tokens) = typedef_stream;
    let stream = TokenStream::new(&tokens, &src);
    let start = 1; // token after 'typedef'
    let end = stream.line_end(start);
    let newline = src.find('\n').expect("newline missing");
    assert_eq!(end, newline + 1);
}

/// Tests that `skip_ws_inline` correctly skips inline whitespace tokens in the
/// token stream.
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
#[test]
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

/// Tests that `line_end` returns the length of the source string when called
/// with an out-of-bounds index.
///
/// # Examples
///
/// ```no_run
/// let src = "typedef A = string\nnext";
/// let tokens = tokenize(src);
/// let stream = TokenStream::new(&tokens, src);
/// let start = tokens.len();
/// assert_eq!(stream.line_end(start), src.len());
/// ```
#[rstest]
fn line_end_out_of_bounds_returns_len(typedef_stream: (String, Vec<(SyntaxKind, Span)>)) {
    let (src, tokens) = typedef_stream;
    let stream = TokenStream::new(&tokens, &src);
    let start = tokens.len();
    assert_eq!(stream.line_end(start), src.len());
}
