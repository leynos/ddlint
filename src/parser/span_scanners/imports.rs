//! Scanner for import statement spans.
//!
//! Parses `import` declarations and records their spans, reporting any parse
//! errors while skipping invalid lines to keep later passes aligned.

use chumsky::prelude::*;

use super::utils::collect_with_parser;
use crate::{
    Span,
    SyntaxKind,
    parser::lexer_helpers::{ident, inline_ws},
};

/// Grammar for the body of one `import` declaration.
///
/// Matches the module path with its `::` segments, then an optional `as` alias,
/// and returns the statement span including trailing inline whitespace.
fn import_parser() -> impl Parser<SyntaxKind, Span, Error = Simple<SyntaxKind>> {
    let ws = inline_ws().repeated();
    let ident_parser = ident();

    let module_path = ident_parser
        .clone()
        .then(
            just(SyntaxKind::T_COLON_COLON)
                .padded_by(ws.clone())
                .ignore_then(ident_parser.clone())
                .repeated(),
        )
        .ignored();

    let alias = just(SyntaxKind::K_AS)
        .padded_by(ws.clone())
        .ignore_then(ident_parser);

    just(SyntaxKind::K_IMPORT)
        .padded_by(ws.clone())
        .ignore_then(module_path)
        .then(alias.or_not())
        .padded_by(ws)
        .map_with_span(|_, sp: Span| sp)
}

pub(crate) fn collect_import_spans(
    tokens: &[(SyntaxKind, Span)],
    src: &str,
) -> (Vec<Span>, Vec<Simple<SyntaxKind>>) {
    collect_with_parser(tokens, src, SyntaxKind::K_IMPORT, import_parser)
}
