//! Scanner for import statement spans.
//!
//! Parses `import` declarations and records their spans, reporting any parse
//! errors while skipping invalid lines to keep later passes aligned.

use chumsky::prelude::*;

use crate::parser::lexer_helpers::{ident, inline_ws};
use crate::parser::token_stream::TokenStream;
use crate::{Span, SyntaxKind};

use super::utils::{State, parse_and_record_split};

fn handle_import(
    stream: &mut TokenStream<'_>,
    spans: &mut Vec<Span>,
    diagnostics: &mut Vec<Simple<SyntaxKind>>,
    span: Span,
) {
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
        .ignore_then(ident_parser.clone());

    let parser = just(SyntaxKind::K_IMPORT)
        .padded_by(ws.clone())
        .ignore_then(module_path)
        .then(alias.or_not())
        .padded_by(ws)
        .map_with_span(|_, sp: Span| sp);

    parse_and_record_split(stream, spans, diagnostics, span.start, parser);
}

pub(crate) fn collect_import_spans(
    tokens: &[(SyntaxKind, Span)],
    src: &str,
) -> (Vec<Span>, Vec<Simple<SyntaxKind>>) {
    let mut st: State<'_> = State::new(tokens, src, Vec::new());

    let State {
        stream,
        spans,
        extra,
    } = &mut st;
    stream.dispatch(|stream, kind, span| match kind {
        SyntaxKind::K_IMPORT => handle_import(stream, spans, extra, span),
        _ => stream.advance(),
    });

    st.into_parts()
}
