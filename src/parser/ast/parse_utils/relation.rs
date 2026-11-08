//! Chumsky helpers for parsing relation primary-key clauses.
//!
//! The relation AST uses these helpers to validate and preserve compatible
//! primary-key binder lists while the span scanner owns declaration recovery.

use chumsky::prelude::*;

use crate::{
    Span,
    SyntaxKind,
    parser::lexer_helpers::{ident, inline_ws},
};

#[expect(
    clippy::result_large_err,
    reason = "chumsky's `Simple` error is large and `filter_map` fixes the closure's error type"
)]
fn keyword<'a>(
    src: &'a str,
    expected: &'static str,
) -> impl Parser<SyntaxKind, (), Error = Simple<SyntaxKind>> + 'a {
    let ws = inline_ws().repeated();
    filter_map(move |span: Span, kind| {
        let found = src.get(span.clone()).unwrap_or("<none>");
        if kind == SyntaxKind::T_IDENT && found == expected {
            Ok(())
        } else {
            let msg = format!("expected `{expected}`, but found `{found}` at span {span:?}");
            Err(Simple::custom(span, msg))
        }
    })
    .then_ignore(ws)
}

/// Parse a `primary key` clause and return its binder names with spans.
///
/// Each span is relative to `src`, the clause text passed in. Callers that
/// slice the clause out of a larger source add the clause's start offset to
/// obtain source-absolute spans.
///
/// # Examples
///
/// ```rust,ignore
/// use chumsky::Parser as _;
/// let src = "primary key(id, other)";
/// let tokens = crate::tokenize_with_trivia(src);
/// let stream = chumsky::Stream::from_iter(0..src.len(), tokens.into_iter());
/// let binders = primary_key_clause(src)
///     .parse(stream)
///     .expect("expected binder list");
/// let names: Vec<&str> = binders.iter().map(|(name, _)| name.as_str()).collect();
/// assert_eq!(names, vec!["id", "other"]);
/// ```
///
/// ```rust,ignore
/// use chumsky::Parser as _;
/// let src = "primary key"; // Missing column list
/// let tokens = crate::tokenize_with_trivia(src);
/// let stream = chumsky::Stream::from_iter(0..src.len(), tokens.into_iter());
/// assert!(primary_key_clause(src).parse(stream).is_err());
/// ```
pub(crate) fn primary_key_clause(
    src: &str,
) -> impl Parser<SyntaxKind, Vec<(String, Span)>, Error = Simple<SyntaxKind>> + '_ {
    let ws = inline_ws().repeated();
    let binder = ident()
        .map_with_span(move |(), sp: Span| (src.get(sp.clone()).unwrap_or("").to_owned(), sp));

    inline_ws()
        .repeated()
        .ignore_then(keyword(src, "primary"))
        .ignore_then(keyword(src, "key"))
        .ignore_then(
            just(SyntaxKind::T_LPAREN)
                .ignore_then(
                    binder
                        .clone()
                        .padded_by(ws.clone())
                        .separated_by(just(SyntaxKind::T_COMMA).padded_by(ws.clone()))
                        .at_least(1),
                )
                .then_ignore(just(SyntaxKind::T_RPAREN)),
        )
        .then_ignore(ws)
}
