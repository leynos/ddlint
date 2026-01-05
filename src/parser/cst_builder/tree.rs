//! Build a `rowan` green tree from tokens and spans.

use log::warn;
use rowan::{GreenNode, GreenNodeBuilder, Language};
use smallvec::SmallVec;

use crate::{DdlogLanguage, Span, SyntaxKind};

use super::spans::ParsedSpans;

type SpanSliceGetter = fn(&ParsedSpans) -> &[Span];

const SPAN_CURSOR_KINDS: &[(SpanSliceGetter, SyntaxKind)] = &[
    (ParsedSpans::imports, SyntaxKind::N_IMPORT_STMT),
    (ParsedSpans::typedefs, SyntaxKind::N_TYPE_DEF),
    (ParsedSpans::relations, SyntaxKind::N_RELATION_DECL),
    (ParsedSpans::indexes, SyntaxKind::N_INDEX),
    (ParsedSpans::functions, SyntaxKind::N_FUNCTION),
    (ParsedSpans::transformers, SyntaxKind::N_TRANSFORMER),
    (ParsedSpans::rules, SyntaxKind::N_RULE),
    (ParsedSpans::expressions, SyntaxKind::N_EXPR_NODE),
];
const SPAN_CURSOR_INLINE: usize = SPAN_CURSOR_KINDS.len();

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
    cursors: SmallVec<[SpanCursor<'a>; SPAN_CURSOR_INLINE]>,
}

impl<'a> SpanCursors<'a> {
    fn new(spans: &'a ParsedSpans) -> Self {
        let mut cursors = SmallVec::with_capacity(SPAN_CURSOR_KINDS.len());
        for (getter, kind) in SPAN_CURSOR_KINDS {
            cursors.push(SpanCursor::new((*getter)(spans), *kind));
        }
        Self { cursors }
    }

    fn advance_and_start(&mut self, builder: &mut GreenNodeBuilder, pos: usize) {
        for cursor in &mut self.cursors {
            cursor.advance_to(pos);
            cursor.start_if(builder, pos);
        }
    }

    fn finish(&mut self, builder: &mut GreenNodeBuilder, pos: usize) {
        for cursor in &mut self.cursors {
            cursor.finish_if(builder, pos);
        }
    }
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

    let mut cursors = SpanCursors::new(spans);

    for &(kind, ref span) in tokens {
        if !validate_token_span(span, src.len()) {
            continue;
        }
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
    use crate::parser::ast::Root;
    use crate::parser::ast::rule::text_range_to_span;
    use crate::parser::span_scanner::parse_tokens;
    use crate::test_util::tokenize;
    use rowan::SyntaxNode;

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
        let spans = match ParsedSpans::builder().build() {
            Ok(spans) => spans,
            Err(err) => panic!("expected valid spans, got: {err}"),
        };
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

    fn collect_kind_spans(root: &SyntaxNode<DdlogLanguage>, kind: SyntaxKind) -> Vec<Span> {
        root.descendants()
            .filter(|node| node.kind() == kind)
            .map(|node| text_range_to_span(node.text_range()))
            .collect()
    }

    fn assert_span_nodes_match(root: &SyntaxNode<DdlogLanguage>, kind: SyntaxKind, spans: &[Span]) {
        let nodes = collect_kind_spans(root, kind);
        assert_eq!(nodes, spans.to_vec());
    }

    #[test]
    fn build_green_tree_matches_span_positions() {
        let src = concat!(
            "import foo::bar\n",
            "typedef UserId = u64\n",
            "input relation User(id: UserId, name: string) primary key (id)\n",
            "index Idx_User_name on User(name)\n",
            "function greet(name: string): string {\n",
            "}\n",
            "extern transformer normalise(input: User): Normalized\n",
            "User(id, name) :- name == \"a\", id > 0.\n"
        );
        let tokens = tokenize(src);
        let (spans, errors) = parse_tokens(&tokens, src);
        assert!(errors.is_empty());
        let green = build_green_tree(&tokens, src, &spans);
        let root = Root::from_green(green);
        let syntax = root.syntax();

        assert_span_nodes_match(syntax, SyntaxKind::N_IMPORT_STMT, spans.imports());
        assert_span_nodes_match(syntax, SyntaxKind::N_TYPE_DEF, spans.typedefs());
        assert_span_nodes_match(syntax, SyntaxKind::N_RELATION_DECL, spans.relations());
        assert_span_nodes_match(syntax, SyntaxKind::N_INDEX, spans.indexes());
        assert_span_nodes_match(syntax, SyntaxKind::N_FUNCTION, spans.functions());
        assert_span_nodes_match(syntax, SyntaxKind::N_TRANSFORMER, spans.transformers());
        assert_span_nodes_match(syntax, SyntaxKind::N_RULE, spans.rules());
        assert_span_nodes_match(syntax, SyntaxKind::N_EXPR_NODE, spans.expressions());
    }
}
