//! Scanner for import statement spans.
//!
//! Parses `import` declarations and records their spans, reporting any parse
//! errors while skipping invalid lines to keep later passes aligned.

use chumsky::prelude::*;

use crate::parser::lexer_helpers::{ident, inline_ws};
use crate::{Span, SyntaxKind};

use super::utils::{parse_and_record, State};

pub(crate) fn collect_import_spans(
    tokens: &[(SyntaxKind, Span)],
    src: &str,
) -> (Vec<Span>, Vec<Simple<SyntaxKind>>) {
    let mut st: State<'_> = State::new(tokens, src, Vec::new());

    let handle_import = |st: &mut State<'_>, span: Span| {
        let ws = inline_ws().repeated();

        let ident = ident();

        let module_path = ident
            .clone()
            .then(
                just(SyntaxKind::T_COLON_COLON)
                    .padded_by(ws.clone())
                    .ignore_then(ident.clone())
                    .repeated(),
            )
            .ignored();

        let alias = just(SyntaxKind::K_AS)
            .padded_by(ws.clone())
            .ignore_then(ident.clone());

        let parser = just(SyntaxKind::K_IMPORT)
            .padded_by(ws.clone())
            .ignore_then(module_path)
            .then(alias.or_not())
            .padded_by(ws)
            .map_with_span(|_, sp: Span| sp);

        parse_and_record(st, span.start, parser);
    };

    token_dispatch!(st, {
        SyntaxKind::K_IMPORT => handle_import,
    });

    st.into_parts()
}
