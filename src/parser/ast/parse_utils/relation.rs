//! Parsers for relation declarations and clauses.
//!
//! These helpers parse relation column lists and `primary key` clauses using
//! `chumsky` so callers can validate structure without manual token juggling.
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
/// let tokens = crate::tokenize(src);
/// let stream = chumsky::Stream::from_iter(0..src.len(), tokens.into_iter());
/// assert!(relation_columns().parse(stream).is_ok());
/// ```
///
/// ```rust,ignore
/// use chumsky::Parser as _;
/// let src = "User"; // Missing column list
/// let tokens = crate::tokenize(src);
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
            Err(Simple::custom(
                span.clone(),
                format!("expected `{expected}`, but found `{found}` at span {span:?}"),
            ))
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
/// let tokens = crate::tokenize(src);
/// let stream = chumsky::Stream::from_iter(0..src.len(), tokens.into_iter());
/// let keys = primary_key_clause(src).parse(stream).unwrap();
/// assert_eq!(keys, vec!["id", "other"]);
/// ```
///
/// ```rust,ignore
/// use chumsky::Parser as _;
/// let src = "primary key"; // Missing column list
/// let tokens = crate::tokenize(src);
/// let stream = chumsky::Stream::from_iter(0..src.len(), tokens.into_iter());
/// assert!(primary_key_clause(src).parse(stream).is_err());
/// ```
pub(crate) fn primary_key_clause(
    src: &str,
) -> impl Parser<SyntaxKind, Vec<String>, Error = Simple<SyntaxKind>> + '_ {
    inline_ws()
        .repeated()
        .ignore_then(keyword(src, "primary"))
        .ignore_then(keyword(src, "key"))
        .ignore_then(
            balanced_block_nonempty(SyntaxKind::T_LPAREN, SyntaxKind::T_RPAREN).map_with_span(
                |(), sp: Span| {
                    let inner = src.get(sp.start + 1..sp.end - 1).unwrap_or("");
                    inner
                        .split(',')
                        .map(str::trim)
                        .filter(|s| !s.is_empty())
                        .map(str::to_string)
                        .collect::<Vec<_>>()
                },
            ),
        )
        .then_ignore(inline_ws().repeated())
}
