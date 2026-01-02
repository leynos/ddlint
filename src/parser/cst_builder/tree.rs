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

fn build_span_cursors<'a>(spans: &'a ParsedSpans) -> Vec<SpanCursor<'a>> {
    vec![
        (spans.imports(), SyntaxKind::N_IMPORT_STMT),
        (spans.typedefs(), SyntaxKind::N_TYPE_DEF),
        (spans.relations(), SyntaxKind::N_RELATION_DECL),
        (spans.indexes(), SyntaxKind::N_INDEX),
        (spans.functions(), SyntaxKind::N_FUNCTION),
        (spans.transformers(), SyntaxKind::N_TRANSFORMER),
        (spans.rules(), SyntaxKind::N_RULE),
        (spans.expressions(), SyntaxKind::N_EXPR_NODE),
    ]
    .into_iter()
    .map(|(slice, kind)| SpanCursor::new(slice, kind))
    .collect()
}

fn validate_token_span(span: &Span, src_len: usize) -> bool {
    if span.start <= span.end && span.end <= src_len {
        true
    } else {
        #[cfg(debug_assertions)]
        {
            panic!("token span {span:?} out of bounds for source of length {src_len}");
        }

        #[cfg(not(debug_assertions))]
        {
            warn!("token span {span:?} out of bounds for source of length {src_len}");
            false
        }
    }
}

/// Construct the CST from the token stream and recorded statement spans.
///
/// # Examples
///
/// ```ignore
/// use ddlint::test_util::tokenize;
/// use ddlint::parser::{cst_builder::{build_green_tree, ParsedSpans}, span_scanner::parse_tokens, ast::Root};
///
/// let src = "import foo::bar;";
/// let tokens = tokenize(src);
/// let (spans, errors) = parse_tokens(&tokens, src);
/// assert!(errors.is_empty());
/// let green = build_green_tree(&tokens, src, &spans);
/// let root = Root::from_green(green);
/// assert_eq!(root.text(), src);
/// ```
pub(crate) fn build_green_tree(
    tokens: &[(SyntaxKind, Span)],
    src: &str,
    spans: &ParsedSpans,
) -> GreenNode {
    let mut builder = GreenNodeBuilder::new();
    builder.start_node(DdlogLanguage::kind_to_raw(SyntaxKind::N_DATALOG_PROGRAM));

    let mut cursors = build_span_cursors(spans);

    for &(kind, ref span) in tokens {
        if !validate_token_span(span, src.len()) {
            continue;
        }
        let start = span.start;
        for cursor in &mut cursors {
            cursor.advance_to(start);
            cursor.start_if(&mut builder, start);
        }

        push_token(&mut builder, kind, span.clone(), src);

        let end = span.end;
        for cursor in &mut cursors {
            cursor.finish_if(&mut builder, end);
        }
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
        push_error_wrapped(builder, raw, text);
    } else {
        builder.token(raw, text);
    }
}

fn push_error_wrapped(builder: &mut GreenNodeBuilder, raw: rowan::SyntaxKind, text: &str) {
    builder.start_node(DdlogLanguage::kind_to_raw(SyntaxKind::N_ERROR));
    builder.token(raw, text);
    builder.finish_node();
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::span_scanner::parse_tokens;
    use crate::test_util::tokenize;

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

    #[cfg(debug_assertions)]
    #[test]
    #[should_panic(expected = "token span")]
    fn build_green_tree_panics_on_oob_token_span() {
        let src = "";
        let spans = ParsedSpans::builder().build();
        let tokens = vec![(SyntaxKind::T_IDENT, 0..1)];

        let _ = build_green_tree(&tokens, src, &spans);
    }

    #[cfg(not(debug_assertions))]
    #[test]
    fn build_green_tree_skips_oob_token_span_in_release() {
        let src = "import foo::bar;";
        let mut tokens = tokenize(src);
        let (spans, errors) = parse_tokens(&tokens, src);
        assert!(errors.is_empty());

        tokens.push((SyntaxKind::K_IMPORT, src.len() + 1..src.len() + 2));
        let green = build_green_tree(&tokens, src, &spans);
        let root = crate::parser::ast::Root::from_green(green);
        assert_eq!(root.text(), src);
    }
}
