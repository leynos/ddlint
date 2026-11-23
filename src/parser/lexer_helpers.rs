//! Token parsing helpers used by the lexer and span collectors.
//!
//! This module groups small parsers and utilities shared across the
//! parsing pipeline. They operate on token streams produced by the
//! tokenizer, keeping them lightweight for easy reuse in different parsing
//! contexts.

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

/// Parser recognizing whitespace and comment tokens.
///
/// The combinator is useful when padding other parsers that allow whitespace.
/// It matches [`SyntaxKind::T_WHITESPACE`] and
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
    let ws = inline_ws().repeated();

    let scoped_ident = ident()
        .then(
            choice((just(SyntaxKind::T_COLON_COLON), just(SyntaxKind::T_DOT)))
                .padded_by(ws.clone())
                .ignore_then(ident())
                .repeated(),
        )
        .ignored()
        .padded_by(ws.clone());

    let ref_mark = just(SyntaxKind::T_AMP)
        .ignored()
        .padded_by(ws.clone())
        .or_not();
    let diff_mark = just(SyntaxKind::T_APOSTROPHE)
        .ignored()
        .padded_by(ws.clone())
        .or_not();

    let args = choice((
        balanced_block(SyntaxKind::T_LPAREN, SyntaxKind::T_RPAREN),
        balanced_block(SyntaxKind::T_LBRACKET, SyntaxKind::T_RBRACKET),
        balanced_block(SyntaxKind::T_LBRACE, SyntaxKind::T_RBRACE),
    ))
    .or_not();

    ref_mark
        .then(scoped_ident)
        .then(diff_mark)
        .then(args)
        .ignored()
        .padded_by(ws)
}

/// Build one level of a balanced block from its nested-block parser.
fn balanced_block_body<P>(
    open: SyntaxKind,
    close: SyntaxKind,
    block: P,
    min: usize,
) -> impl Parser<SyntaxKind, (), Error = Simple<SyntaxKind>> + Clone
where
    P: Parser<SyntaxKind, (), Error = Simple<SyntaxKind>> + Clone,
{
    just(open)
        .padded_by(inline_ws().repeated())
        .ignore_then(
            block
                .or(filter(move |kind: &SyntaxKind| *kind != open && *kind != close).ignored())
                .padded_by(inline_ws().repeated())
                .repeated()
                .at_least(min),
        )
        .then_ignore(just(close))
        .ignored()
}

/// Maximum nesting depth accepted inside one balanced block.
///
/// Each nesting level adds several Rust stack frames while parsing, and an
/// unoptimized build needs roughly 24 KiB per level. The limit keeps a
/// pathological input from exhausting a default 2 MiB thread stack; nesting
/// beyond it is reported as a parse error. Real `DDlog` declarations nest only
/// a handful of levels, so the limit leaves a wide margin.
pub(super) const MAX_BLOCK_DEPTH: usize = 64;

/// Parser for a balanced token block such as parentheses or braces.
///
/// Nested blocks use the same delimiter pair. The first matching closing
/// token ends the current block; other delimiter kinds are ordinary tokens.
/// Whitespace and comments are permitted between tokens. Nesting is bounded
/// by [`MAX_BLOCK_DEPTH`], so deeper input fails to parse instead of
/// recursing without limit. The grammar holds no mutable state, so a failed
/// parse cannot affect a later use of the parser.
///
/// # Examples
///
/// ```rust
/// # use ddlint::parse;
/// let parsed = parse(concat!(
///     "Totals(user, total) :- Orders(user, amt), ",
///     "Aggregate((user), sum(amt)), total = __group.",
/// ));
/// assert!(
///     parsed.errors().is_empty(),
///     "expected no parse errors, got {:?}",
///     parsed.errors(),
/// );
/// assert_eq!(
///     parsed.root().rules().len(),
///     1,
///     "expected exactly one rule",
/// );
/// ```
pub(super) fn balanced_block(
    open: SyntaxKind,
    close: SyntaxKind,
) -> impl Parser<SyntaxKind, (), Error = Simple<SyntaxKind>> + Clone {
    // Build the grammar from the deepest level outwards. The innermost parser
    // rejects the block's own delimiters, so the level above the limit fails
    // to parse rather than recursing further.
    let ordinary = filter(move |kind: &SyntaxKind| *kind != open && *kind != close)
        .ignored()
        .boxed();
    (0..MAX_BLOCK_DEPTH).fold(ordinary, |nested, _| {
        balanced_block_body(open, close, nested, 0).boxed()
    })
}

/// As [`balanced_block`] but requires at least one item inside the delimiters.
///
/// Nested blocks use the ordinary, possibly empty, balanced-block grammar.
#[cfg(test)]
pub(super) fn balanced_block_nonempty(
    open: SyntaxKind,
    close: SyntaxKind,
) -> impl Parser<SyntaxKind, (), Error = Simple<SyntaxKind>> + Clone {
    balanced_block_body(open, close, balanced_block(open, close), 1)
}

#[cfg(test)]
#[path = "lexer_helpers_tests.rs"]
mod tests;
