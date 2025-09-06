//! Token parsing helpers used by the lexer and span collectors.
//!
//! This module groups small parsers and utilities shared across the
//! parsing pipeline. They operate on token streams produced by the
//! tokenizer and are kept lightweight so they can be reused easily in
//! different parsing contexts.

use chumsky::prelude::*;

use crate::SyntaxKind;

/// Convert a punctuation [`SyntaxKind`] to a printable character.
///
/// Diagnostic messages use symbolic token kinds which are not human
/// readable. This helper maps those kinds to the punctuation displayed in
/// errors.
///
/// # Examples
///
/// ```rust,ignore
/// use ddlint::{parser::lexer_helpers::token_display, SyntaxKind};
/// use chumsky::error::Simple;
///
/// let err = Simple::custom(0..0, format!(
///     "expected '{}'", token_display(SyntaxKind::T_RPAREN)
/// ));
/// ```
pub(super) fn token_display(kind: SyntaxKind) -> &'static str {
    match kind {
        SyntaxKind::T_LPAREN => "(",
        SyntaxKind::T_RPAREN => ")",
        SyntaxKind::T_LBRACE => "{",
        SyntaxKind::T_RBRACE => "}",
        SyntaxKind::T_LBRACKET => "[",
        SyntaxKind::T_RBRACKET => "]",
        SyntaxKind::T_COMMA => ",",
        SyntaxKind::T_COLON => ":",
        SyntaxKind::T_SEMI => ";",
        _ => "",
    }
}

/// Iterate over the token stream and dispatch handlers by [`SyntaxKind`].
///
/// The macro expects a parsing context `ctx` with a `stream` field. It loops
/// until the stream is exhausted, invoking the handler associated with each
/// recognised kind. Handlers must advance the stream to consume the tokens they
/// process. Any unhandled kind is skipped.
///
/// # Examples
///
/// ```rust,ignore
/// use ddlint::parser::token_stream::TokenStream;
/// use ddlint::parser::lexer_helpers::token_dispatch;
/// use ddlint::{Span, SyntaxKind};
///
/// struct State<'a> {
///     stream: TokenStream<'a>,
/// }
///
/// fn handle_kw(st: &mut State<'_>, _span: Span) {
///     st.stream.advance();
/// }
///
/// # let tokens = ddlint::tokenize_with_trivia("extern type Foo;");
/// # let src = "extern type Foo;";
/// let mut st = State { stream: TokenStream::new(&tokens, src) };
/// token_dispatch!(st, {
///     SyntaxKind::K_EXTERN => handle_kw,
/// });
/// ```
macro_rules! token_dispatch {
    ( $ctx:ident, {
        $( $kind:path => $handler:ident ),* $(,)?
    } ) => {{
        while let Some(&(kind, ref span_ref)) = $ctx.stream.peek() {
            let span = span_ref.clone();
            match kind {
                $( $kind => $handler(&mut $ctx, span.clone()), )*
                _ => $ctx.stream.advance(),
            }
        }
    }};
}

/// Parser recognising whitespace and comment tokens.
///
/// The combinator is useful for padding other parsers where whitespace is
/// allowed. It matches [`SyntaxKind::T_WHITESPACE`] and
/// [`SyntaxKind::T_COMMENT`] tokens and discards them.
///
/// # Examples
///
/// ```rust,ignore
/// use ddlint::parser::lexer_helpers::inline_ws;
/// let parser = inline_ws().repeated();
/// ```
pub(super) fn inline_ws() -> impl Parser<SyntaxKind, (), Error = Simple<SyntaxKind>> + Clone {
    filter(|kind: &SyntaxKind| matches!(kind, SyntaxKind::T_WHITESPACE | SyntaxKind::T_COMMENT))
        .ignored()
}

/// Parser for an identifier padded by optional inline whitespace.
///
/// # Examples
///
/// ```rust,ignore
/// use ddlint::parser::lexer_helpers::ident;
/// let parser = ident();
/// ```
pub(super) fn ident() -> impl Parser<SyntaxKind, (), Error = Simple<SyntaxKind>> + Clone {
    just(SyntaxKind::T_IDENT)
        .ignored()
        .padded_by(inline_ws().repeated())
}

/// Parser for a rule atom: `Ident` with optional argument list.
///
/// # Examples
///
/// ```rust,ignore
/// use ddlint::parser::lexer_helpers::atom;
/// let parser = atom();
/// ```
pub(super) fn atom() -> impl Parser<SyntaxKind, (), Error = Simple<SyntaxKind>> + Clone {
    ident()
        .clone()
        .then(
            just(SyntaxKind::T_LPAREN)
                .padded_by(inline_ws().repeated())
                .ignore_then(
                    filter(|kind: &SyntaxKind| *kind != SyntaxKind::T_RPAREN)
                        .ignored()
                        .padded_by(inline_ws().repeated())
                        .repeated(),
                )
                .then_ignore(just(SyntaxKind::T_RPAREN))
                .or_not(),
        )
        .ignored()
        .padded_by(inline_ws().repeated())
}

/// Parser for a balanced token block such as parentheses or braces.
///
/// The parser consumes the opening delimiter, then all tokens until the
/// matching closing delimiter while tracking nested pairs. Whitespace and
/// comments are permitted between tokens. An error is produced if a closing
/// token appears without a corresponding opener.
fn balanced_block_with_min(
    open: SyntaxKind,
    close: SyntaxKind,
    min: usize,
) -> impl Parser<SyntaxKind, (), Error = Simple<SyntaxKind>> + Clone {
    use std::cell::Cell;

    let depth = Cell::new(0usize);
    just(open)
        .padded_by(inline_ws().repeated())
        .ignore_then(
            filter_map(move |span, kind| match kind {
                k if k == open => {
                    depth.set(depth.get() + 1);
                    Ok(())
                }
                k if k == close => {
                    if depth.get() == 0 {
                        Err(Simple::custom(
                            span,
                            format!("unexpected '{}'", token_display(close)),
                        ))
                    } else {
                        depth.set(depth.get() - 1);
                        Ok(())
                    }
                }
                _ => Ok(()),
            })
            .padded_by(inline_ws().repeated())
            .repeated()
            .at_least(min),
        )
        .then_ignore(just(close))
        .ignored()
}

pub(super) fn balanced_block(
    open: SyntaxKind,
    close: SyntaxKind,
) -> impl Parser<SyntaxKind, (), Error = Simple<SyntaxKind>> + Clone {
    balanced_block_with_min(open, close, 0)
}

/// As [`balanced_block`] but requires at least one token inside the delimiters.
pub(super) fn balanced_block_nonempty(
    open: SyntaxKind,
    close: SyntaxKind,
) -> impl Parser<SyntaxKind, (), Error = Simple<SyntaxKind>> + Clone {
    balanced_block_with_min(open, close, 1)
}

#[cfg(test)]
mod tests {
    //! Unit tests for the lexer helper utilities.
    use super::*;
    use crate::{Span, parser::token_stream::TokenStream, tokenize_with_trivia};
    use chumsky::Stream;
    use rstest::rstest;

    #[rstest]
    #[case(SyntaxKind::T_LPAREN, "(")]
    #[case(SyntaxKind::T_RPAREN, ")")]
    #[case(SyntaxKind::T_LBRACE, "{")]
    #[case(SyntaxKind::T_RBRACE, "}")]
    #[case(SyntaxKind::T_LBRACKET, "[")]
    #[case(SyntaxKind::T_RBRACKET, "]")]
    #[case(SyntaxKind::T_COMMA, ",")]
    #[case(SyntaxKind::T_COLON, ":")]
    #[case(SyntaxKind::T_SEMI, ";")]
    #[case(SyntaxKind::K_IMPORT, "")]
    fn display_matches(#[case] kind: SyntaxKind, #[case] expected: &str) {
        assert_eq!(token_display(kind), expected);
    }

    #[test]
    fn dispatch_invokes_handlers() {
        struct State<'a> {
            stream: TokenStream<'a>,
            out: Vec<&'static str>,
        }

        fn lp(st: &mut State<'_>, _span: Span) {
            st.stream.advance();
            st.out.push("lp");
        }

        fn rp(st: &mut State<'_>, _span: Span) {
            st.stream.advance();
            st.out.push("rp");
        }

        let src = "()";
        let tokens = tokenize_with_trivia(src);
        let mut st = State {
            stream: TokenStream::new(&tokens, src),
            out: Vec::new(),
        };

        token_dispatch!(st, {
            SyntaxKind::T_LPAREN => lp,
            SyntaxKind::T_RPAREN => rp,
        });

        assert_eq!(st.out, vec!["lp", "rp"]);
    }

    #[test]
    fn inline_ws_consumes_ws_and_comments() {
        let src = " \t//c";
        let tokens = tokenize_with_trivia(src);
        let parser = inline_ws().repeated();
        let res = parser.parse(Stream::from_iter(0..src.len(), tokens.into_iter()));
        assert!(res.is_ok());
    }

    #[test]
    fn ident_parses_with_padding() {
        let src = "  foo  ";
        let tokens = tokenize_with_trivia(src);
        let res = ident().parse(Stream::from_iter(0..src.len(), tokens.into_iter()));
        assert!(res.is_ok());
    }

    #[test]
    fn atom_accepts_optional_args() {
        let src = "foo(bar)";
        let tokens = tokenize_with_trivia(src);
        let parser = atom();
        let res = parser.parse(Stream::from_iter(0..src.len(), tokens.into_iter()));
        assert!(res.is_ok());
    }

    #[test]
    fn balanced_block_parses_nested() {
        let src = "(a(b)c)";
        let tokens = tokenize_with_trivia(src);
        let parser = balanced_block(SyntaxKind::T_LPAREN, SyntaxKind::T_RPAREN);
        let res = parser.parse(Stream::from_iter(0..src.len(), tokens.into_iter()));
        assert!(res.is_ok());
    }

    #[test]
    fn balanced_block_nonempty_fails_on_empty() {
        let src = "()";
        let tokens = tokenize_with_trivia(src);
        let parser = balanced_block_nonempty(SyntaxKind::T_LPAREN, SyntaxKind::T_RPAREN);
        let res = parser.parse(Stream::from_iter(0..src.len(), tokens.into_iter()));
        assert!(res.is_err());
    }
}
