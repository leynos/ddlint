//! Build a `rowan` green tree from tokens and spans.

use log::warn;
use rowan::{GreenNode, GreenNodeBuilder, Language};

use crate::{DdlogLanguage, Span, SyntaxKind};

use super::spans::ParsedSpans;

struct SpanCursor<'a> {
    iter: std::iter::Peekable<std::slice::Iter<'a, Span>>,
    kind: SyntaxKind,
}

impl<'a> SpanCursor<'a> {
    fn new(spans: &'a [Span], kind: SyntaxKind) -> Self {
        Self {
            iter: spans.iter().peekable(),
            kind,
        }
    }

    fn advance_to(&mut self, pos: usize) {
        while matches!(self.iter.peek(), Some(s) if pos >= s.end) {
            self.iter.next();
        }
    }

    fn start_if(&mut self, builder: &mut GreenNodeBuilder, pos: usize) {
        if matches!(self.iter.peek(), Some(s) if pos == s.start) {
            builder.start_node(DdlogLanguage::kind_to_raw(self.kind));
        }
    }

    fn finish_if(&mut self, builder: &mut GreenNodeBuilder, pos: usize) {
        if matches!(self.iter.peek(), Some(s) if pos >= s.end) {
            builder.finish_node();
            self.iter.next();
        }
    }
}

struct SpanCursors<'a> {
    cursors: [SpanCursor<'a>; 7],
}

impl<'a> SpanCursors<'a> {
    fn new(spans: &'a ParsedSpans) -> Self {
        Self {
            cursors: [
                SpanCursor::new(spans.imports(), SyntaxKind::N_IMPORT_STMT),
                SpanCursor::new(spans.typedefs(), SyntaxKind::N_TYPE_DEF),
                SpanCursor::new(spans.relations(), SyntaxKind::N_RELATION_DECL),
                SpanCursor::new(spans.indexes(), SyntaxKind::N_INDEX),
                SpanCursor::new(spans.functions(), SyntaxKind::N_FUNCTION),
                SpanCursor::new(spans.transformers(), SyntaxKind::N_TRANSFORMER),
                SpanCursor::new(spans.rules(), SyntaxKind::N_RULE),
            ],
        }
    }

    fn advance_and_start(&mut self, builder: &mut GreenNodeBuilder, pos: usize) {
        for cur in &mut self.cursors {
            cur.advance_to(pos);
            cur.start_if(builder, pos);
        }
    }

    fn finish(&mut self, builder: &mut GreenNodeBuilder, pos: usize) {
        for cur in &mut self.cursors {
            cur.finish_if(builder, pos);
        }
    }
}

/// Construct the CST from the token stream and recorded statement spans.
pub(crate) fn build_green_tree(
    tokens: &[(SyntaxKind, Span)],
    src: &str,
    spans: &ParsedSpans,
) -> GreenNode {
    let mut builder = GreenNodeBuilder::new();
    builder.start_node(DdlogLanguage::kind_to_raw(SyntaxKind::N_DATALOG_PROGRAM));

    let mut cursors = SpanCursors::new(spans);

    for &(kind, ref span) in tokens {
        cursors.advance_and_start(&mut builder, span.start);

        push_token(&mut builder, kind, span.clone(), src);

        cursors.finish(&mut builder, span.end);
    }

    builder.finish_node();
    builder.finish()
}

fn push_token(builder: &mut GreenNodeBuilder, kind: SyntaxKind, span: Span, src: &str) {
    let text = src.get(span.clone()).map_or_else(
        || {
            warn!(
                "token span {:?} out of bounds for source of length {}",
                span,
                src.len()
            );
            ""
        },
        |t| t,
    );

    let raw = DdlogLanguage::kind_to_raw(kind);
    if kind == SyntaxKind::N_ERROR {
        builder.start_node(DdlogLanguage::kind_to_raw(SyntaxKind::N_ERROR));
    }
    builder.token(raw, text);
    if kind == SyntaxKind::N_ERROR {
        builder.finish_node();
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::span_scanner::parse_tokens;
    use crate::tokenize;

    #[test]
    fn build_green_tree_round_trip() {
        let src = "import foo::bar;";
        let tokens = tokenize(src);
        let (spans, errors) = parse_tokens(&tokens, src);
        assert!(errors.is_empty());
        let green = build_green_tree(&tokens, src, &spans);
        let root = crate::parser::ast::Root::from_green(green);
        assert_eq!(root.text(), src);
    }
}
