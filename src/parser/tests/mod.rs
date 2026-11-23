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

    assert_eq!(
        calls, 0,
        "dispatch must not call the closure on empty input"
    );
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
        vec![(SyntaxKind::T_LPAREN, 0..1), (SyntaxKind::T_RPAREN, 1..2)],
        "dispatch must report each token kind with its own span"
    );
}

#[test]
fn dispatch_invokes_matching_handlers_and_skips_others() {
    // The identifier between the parentheses only matches the catch-all arm,
    // so the test fails if that arm forgets to advance or stops dispatch.
    let src = "(x)";
    let tokens = tokenize(src);
    let mut stream = TokenStream::new(&tokens, src);
    let mut out = Vec::new();
    let mut skipped = 0;

    stream.dispatch(|stream, kind, _span| match kind {
        SyntaxKind::T_LPAREN => {
            stream.advance();
            out.push("lp");
        }
        SyntaxKind::T_RPAREN => {
            stream.advance();
            out.push("rp");
        }
        _ => {
            skipped += 1;
            stream.advance();
        }
    });

    assert_eq!(
        out,
        vec!["lp", "rp"],
        "dispatch must reach the closing paren after an unmatched token"
    );
    assert_eq!(
        skipped, 1,
        "the identifier must be handled by the catch-all arm exactly once"
    );
    assert!(
        stream.peek().is_none(),
        "dispatch must consume the whole stream"
    );
}

#[test]
fn dispatch_handler_may_consume_several_tokens() {
    let src = "typedef A = string\nnext";
    let tokens = tokenize(src);
    let newline = src.find('\n').expect("test source must contain a newline");
    let typedef_line = 0..newline + 1;
    let mut stream = TokenStream::new(&tokens, src);
    let mut consumed = Vec::new();
    let mut fallback = Vec::new();

    stream.dispatch(|stream, kind, span| {
        if kind == SyntaxKind::K_TYPEDEF {
            let end = stream.line_end(stream.cursor());
            stream.skip_until(end);
            consumed.push(span.start..end);
        } else {
            fallback.push((kind, span));
            stream.advance();
        }
    });

    assert_eq!(
        consumed,
        vec![typedef_line],
        "the handler must consume exactly the typedef line"
    );
    assert_eq!(
        fallback.len(),
        1,
        "only the token after the consumed line may reach the fallback arm: {fallback:?}"
    );
    assert_eq!(
        fallback.first().map(|(kind, span)| (*kind, span.clone())),
        Some((SyntaxKind::T_IDENT, newline + 1..src.len())),
        "dispatch must continue at the token following the consumed line"
    );
    assert!(
        stream.peek().is_none(),
        "dispatch must finish the stream after the trailing token"
    );
}

#[test]
fn nested_atom_arguments_parse_at_the_limit_on_a_default_thread() {
    // The atom's own argument list counts as the first nesting level, so the
    // inner source supplies one fewer pair.
    let nested = super::lexer_helpers::MAX_BLOCK_DEPTH - 1;
    let src = format!("R({}a{})", "(".repeat(nested), ")".repeat(nested));
    let errors = std::thread::spawn(move || {
        crate::parse(&src)
            .errors()
            .iter()
            .map(|error| format!("{error:?}"))
            .collect::<Vec<_>>()
    })
    .join()
    .expect("parser thread should not panic");

    assert!(
        errors.is_empty(),
        "expected nested atom arguments at the limit to parse, got {errors:?}"
    );
}
