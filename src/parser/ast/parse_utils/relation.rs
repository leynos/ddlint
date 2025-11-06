//! Parsers for relation declarations and clauses.
//!
//! These helpers parse relation column lists and `primary key` clauses using
//! `chumsky`, allowing callers to validate structure without manual token
//! juggling.
//! They are shared by the span scanner and AST helpers to keep behaviour
//! consistent.

use chumsky::prelude::*;

use crate::parser::lexer_helpers::{balanced_block_nonempty, ident, inline_ws};
use crate::{Span, SyntaxKind};

/// Parse a relation name followed by a non-empty column list.
///
/// # Examples
///
/// ```rust,ignore
/// use chumsky::Parser as _;
/// let src = "User(id: u32)";
/// let tokens = crate::tokenize_with_trivia(src);
/// let stream = chumsky::Stream::from_iter(0..src.len(), tokens.into_iter());
/// assert!(relation_columns().parse(stream).is_ok());
/// ```
///
/// ```rust,ignore
/// use chumsky::Parser as _;
/// let src = "User"; // Missing column list
/// let tokens = crate::tokenize_with_trivia(src);
/// let stream = chumsky::Stream::from_iter(0..src.len(), tokens.into_iter());
/// assert!(relation_columns().parse(stream).is_err());
/// ```
pub(crate) fn relation_columns() -> impl Parser<SyntaxKind, Span, Error = Simple<SyntaxKind>> {
    let ws = inline_ws().repeated();
    ident()
        .padded_by(ws.clone())
        .then(balanced_block_nonempty(
            SyntaxKind::T_LPAREN,
            SyntaxKind::T_RPAREN,
        ))
        .map_with_span(|_, sp: Span| sp)
        .then_ignore(ws)
}

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

/// Parse a `primary key` clause and return the column names.
///
/// # Examples
///
/// ```rust,ignore
/// use chumsky::Parser as _;
/// let src = "primary key(id, other)";
/// let tokens = crate::tokenize_with_trivia(src);
/// let stream = chumsky::Stream::from_iter(0..src.len(), tokens.into_iter());
/// let keys = primary_key_clause(src)
///     .parse(stream)
///     .expect("expected column list");
/// assert_eq!(keys, vec!["id", "other"]);
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
) -> impl Parser<SyntaxKind, Vec<String>, Error = Simple<SyntaxKind>> + '_ {
    let ws = inline_ws().repeated();
    let ident_text =
        ident().map_with_span(move |(), sp: Span| src.get(sp.clone()).unwrap_or("").to_string());

    inline_ws()
        .repeated()
        .ignore_then(keyword(src, "primary"))
        .ignore_then(keyword(src, "key"))
        .ignore_then(
            just(SyntaxKind::T_LPAREN)
                .ignore_then(
                    ident_text
                        .clone()
                        .padded_by(ws.clone())
                        .separated_by(just(SyntaxKind::T_COMMA).padded_by(ws.clone()))
                        .at_least(1),
                )
                .then_ignore(just(SyntaxKind::T_RPAREN)),
        )
        .then_ignore(ws)
}
