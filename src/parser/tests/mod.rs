//! Tests for parser components and integration scenarios.

mod apply;
mod attributes;
mod collections;
mod control_flow;
mod cst_integration;
mod expression;
mod expression_integration;
mod expression_span;
mod functions;
mod helpers;
mod imports;
mod indexes;
mod numeric_literals;
mod operator_precedence;
mod relation_proptest;
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

#[test]
fn dispatch_on_empty_stream_does_not_invoke_the_callback() {
    let src = "";
    let tokens = tokenize(src);
    let mut stream = TokenStream::new(&tokens, src);
    let mut calls = 0;

    stream.dispatch(|stream, _, _| {
        calls += 1;
        stream.advance();
    });

    assert_eq!(calls, 0);
}

#[test]
fn dispatch_passes_each_kind_with_its_span() {
    let src = "()";
    let tokens = tokenize(src);
    let mut stream = TokenStream::new(&tokens, src);
    let mut seen = Vec::new();

    stream.dispatch(|stream, kind, span| {
        seen.push((kind, span));
        stream.advance();
    });

    assert_eq!(
        seen,
        vec![(SyntaxKind::T_LPAREN, 0..1), (SyntaxKind::T_RPAREN, 1..2),]
    );
}

#[test]
fn dispatch_invokes_matching_handlers_and_skips_others() {
    let src = "()";
    let tokens = tokenize(src);
    let mut stream = TokenStream::new(&tokens, src);
    let mut out = Vec::new();

    stream.dispatch(|stream, kind, _span| match kind {
        SyntaxKind::T_LPAREN => {
            stream.advance();
            out.push("lp");
        }
        SyntaxKind::T_RPAREN => {
            stream.advance();
            out.push("rp");
        }
        _ => stream.advance(),
    });

    assert_eq!(out, vec!["lp", "rp"]);
    assert!(stream.peek().is_none());
}

#[test]
fn dispatch_handler_may_consume_several_tokens() {
    let src = "typedef A = string\nnext";
    let tokens = tokenize(src);
    let mut stream = TokenStream::new(&tokens, src);
    let mut consumed = Vec::new();

    stream.dispatch(|stream, kind, span| {
        if kind == SyntaxKind::K_TYPEDEF {
            let end = stream.line_end(stream.cursor());
            stream.skip_until(end);
            consumed.push(span.start..end);
        } else {
            stream.advance();
        }
    });

    assert_eq!(consumed.len(), 1);
    assert!(stream.peek().is_none());
}
