//! CST construction utilities.
//!
//! This module builds a `rowan::GreenNode` from a sequence of tokens and
//! statement spans. It validates span ordering and provides the [`Parsed`] and
//! [`ParsedSpans`] types used by the parser entry point.

use chumsky::error::Simple;
use log::warn;
use rowan::{GreenNode, GreenNodeBuilder, Language};

use crate::{DdlogLanguage, Span, SyntaxKind};

/// Result of a parse operation.
#[derive(Debug)]
pub struct Parsed {
    green: GreenNode,
    root: super::ast::Root,
    errors: Vec<Simple<SyntaxKind>>,
}

impl Parsed {
    pub(super) fn new(
        green: GreenNode,
        root: super::ast::Root,
        errors: Vec<Simple<SyntaxKind>>,
    ) -> Self {
        Self {
            green,
            root,
            errors,
        }
    }

    /// Access the `rowan` green tree.
    #[must_use]
    pub fn green(&self) -> &GreenNode {
        &self.green
    }

    /// Access the typed AST root.
    #[must_use]
    pub fn root(&self) -> &super::ast::Root {
        &self.root
    }

    /// Access parser errors collected during recovery.
    #[must_use]
    pub fn errors(&self) -> &[Simple<SyntaxKind>] {
        &self.errors
    }
}

/// Spans for each parsed statement category.
///
/// Instances are constructed via [`ParsedSpans::new`] to ensure span lists are
/// sorted and non-overlapping in debug builds.
#[non_exhaustive]
#[derive(Debug, Default, Clone, PartialEq)]
pub struct ParsedSpans {
    /// `import` statement spans.
    imports: Vec<Span>,
    /// `typedef` statement spans.
    typedefs: Vec<Span>,
    /// `relation` declaration spans.
    relations: Vec<Span>,
    /// `index` declaration spans.
    indexes: Vec<Span>,
    /// `function` definition spans.
    functions: Vec<Span>,
    /// `transformer` declaration spans.
    transformers: Vec<Span>,
    /// Rule spans.
    rules: Vec<Span>,
}

impl ParsedSpans {
    /// Construct a new [`ParsedSpans`].
    ///
    /// The caller must provide span lists that are sorted and free from
    /// overlaps. In debug builds every list is validated and the function will
    /// panic if any ordering violation is detected.
    #[must_use]
    pub fn new(
        imports: Vec<Span>,
        typedefs: Vec<Span>,
        relations: Vec<Span>,
        indexes: Vec<Span>,
        functions: Vec<Span>,
        transformers: Vec<Span>,
        rules: Vec<Span>,
    ) -> Self {
        if cfg!(debug_assertions) {
            ensure_span_lists_sorted(&[
                ("imports", &imports),
                ("typedefs", &typedefs),
                ("relations", &relations),
                ("indexes", &indexes),
                ("functions", &functions),
                ("transformers", &transformers),
                ("rules", &rules),
            ]);
        }

        Self {
            imports,
            typedefs,
            relations,
            indexes,
            functions,
            transformers,
            rules,
        }
    }

    /// Access `import` statement spans.
    #[must_use]
    pub fn imports(&self) -> &[Span] {
        &self.imports
    }

    /// Access `typedef` statement spans.
    #[must_use]
    pub fn typedefs(&self) -> &[Span] {
        &self.typedefs
    }

    /// Access `relation` declaration spans.
    #[must_use]
    pub fn relations(&self) -> &[Span] {
        &self.relations
    }

    /// Access `index` declaration spans.
    #[must_use]
    pub fn indexes(&self) -> &[Span] {
        &self.indexes
    }

    /// Access `function` definition spans.
    #[must_use]
    pub fn functions(&self) -> &[Span] {
        &self.functions
    }

    /// Access `transformer` declaration spans.
    #[must_use]
    pub fn transformers(&self) -> &[Span] {
        &self.transformers
    }

    /// Access rule spans.
    #[must_use]
    pub fn rules(&self) -> &[Span] {
        &self.rules
    }
}

/// Construct the CST from the token stream and recorded statement spans.
///
/// Span lists must be sorted and non-overlapping so that tokens are wrapped
/// into well-formed nodes. Validation occurs in debug builds when
/// [`ParsedSpans`] is created.
pub(super) fn build_green_tree(
    tokens: &[(SyntaxKind, Span)],
    src: &str,
    spans: &ParsedSpans,
) -> GreenNode {
    let mut builder = GreenNodeBuilder::new();
    builder.start_node(DdlogLanguage::kind_to_raw(SyntaxKind::N_DATALOG_PROGRAM));

    let mut import_iter = spans.imports().iter().peekable();
    let mut typedef_iter = spans.typedefs().iter().peekable();
    let mut relation_iter = spans.relations().iter().peekable();
    let mut index_iter = spans.indexes().iter().peekable();
    let mut function_iter = spans.functions().iter().peekable();
    let mut transformer_iter = spans.transformers().iter().peekable();
    let mut rule_iter = spans.rules().iter().peekable();

    for &(kind, ref span) in tokens {
        advance_span_iter(&mut import_iter, span.start);
        advance_span_iter(&mut typedef_iter, span.start);
        advance_span_iter(&mut relation_iter, span.start);
        advance_span_iter(&mut index_iter, span.start);
        advance_span_iter(&mut function_iter, span.start);
        advance_span_iter(&mut transformer_iter, span.start);
        advance_span_iter(&mut rule_iter, span.start);

        start_nodes(
            &mut builder,
            &mut [
                (&mut import_iter, SyntaxKind::N_IMPORT_STMT),
                (&mut typedef_iter, SyntaxKind::N_TYPE_DEF),
                (&mut relation_iter, SyntaxKind::N_RELATION_DECL),
                (&mut index_iter, SyntaxKind::N_INDEX),
                (&mut function_iter, SyntaxKind::N_FUNCTION),
                (&mut transformer_iter, SyntaxKind::N_TRANSFORMER),
                (&mut rule_iter, SyntaxKind::N_RULE),
            ],
            span.start,
        );

        push_token(&mut builder, kind, span, src);

        finish_nodes(
            &mut builder,
            &mut [
                &mut import_iter,
                &mut typedef_iter,
                &mut relation_iter,
                &mut index_iter,
                &mut function_iter,
                &mut transformer_iter,
                &mut rule_iter,
            ],
            span.end,
        );
    }

    builder.finish_node();
    builder.finish()
}

fn advance_span_iter(iter: &mut std::iter::Peekable<std::slice::Iter<'_, Span>>, pos: usize) {
    while let Some(next) = iter.peek() {
        if pos >= next.end {
            iter.next();
        } else {
            break;
        }
    }
}

fn maybe_start(
    builder: &mut GreenNodeBuilder,
    iter: &mut std::iter::Peekable<std::slice::Iter<Span>>,
    pos: usize,
    kind: SyntaxKind,
) {
    if iter.peek().is_some_and(|current| pos == current.start) {
        builder.start_node(DdlogLanguage::kind_to_raw(kind));
    }
}

fn maybe_finish(
    builder: &mut GreenNodeBuilder,
    iter: &mut std::iter::Peekable<std::slice::Iter<Span>>,
    pos: usize,
) {
    if iter.peek().is_some_and(|current| pos >= current.end) {
        builder.finish_node();
        iter.next();
    }
}

type SpanIter<'a> = std::iter::Peekable<std::slice::Iter<'a, Span>>;

fn start_nodes(
    builder: &mut GreenNodeBuilder,
    pairs: &mut [(&mut SpanIter<'_>, SyntaxKind)],
    pos: usize,
) {
    for (iter, kind) in pairs.iter_mut() {
        maybe_start(builder, iter, pos, *kind);
    }
}

fn finish_nodes(builder: &mut GreenNodeBuilder, iters: &mut [&mut SpanIter<'_>], pos: usize) {
    for iter in iters.iter_mut() {
        maybe_finish(builder, iter, pos);
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct SpanOrderError {
    prev: Span,
    next: Span,
}

impl std::fmt::Display for SpanOrderError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "spans overlap or are unsorted: {:?} then {:?}",
            self.prev, self.next
        )
    }
}

impl std::error::Error for SpanOrderError {}

fn validate_spans_sorted(spans: &[Span]) -> Result<(), SpanOrderError> {
    for pair in spans.windows(2) {
        let [first, second] = pair else { continue };
        if first.end > second.start {
            return Err(SpanOrderError {
                prev: first.clone(),
                next: second.clone(),
            });
        }
    }
    Ok(())
}

fn ensure_span_lists_sorted(lists: &[(&str, &[Span])]) {
    let mut errors = Vec::new();
    for (name, spans) in lists {
        if let Err(e) = validate_spans_sorted(spans) {
            errors.push(format!("{name} not sorted: {e}"));
        }
    }
    assert!(errors.is_empty(), "{}", errors.join("\n"));
}

fn push_token(builder: &mut GreenNodeBuilder, kind: SyntaxKind, span: &Span, src: &str) {
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
    use crate::tokenize;
    use rstest::rstest;

    #[test]
    fn validate_spans_sorted_err_on_overlap() {
        let spans = vec![0..5, 4..8];
        let result = validate_spans_sorted(&spans);
        assert!(result.is_err());
    }

    #[test]
    fn validate_spans_sorted_err_on_unsorted() {
        let spans = vec![5..10, 0..2];
        let result = validate_spans_sorted(&spans);
        assert!(result.is_err());
    }

    #[test]
    fn validate_spans_sorted_ok_on_empty() {
        let spans: Vec<Span> = Vec::new();
        assert!(validate_spans_sorted(&spans).is_ok());
    }

    #[test]
    fn validate_spans_sorted_ok_on_single() {
        let spans: Vec<Span> = vec![std::ops::Range { start: 0, end: 3 }];
        assert!(validate_spans_sorted(&spans).is_ok());
    }

    #[test]
    fn validate_spans_sorted_ok_on_sorted() {
        let spans = vec![0..2, 3..5, 5..8];
        assert!(validate_spans_sorted(&spans).is_ok());
    }

    #[test]
    fn build_green_tree_panics_on_misordered_spans() {
        let unsorted = vec![1..2, 0..1];
        let result = std::panic::catch_unwind(|| {
            let _ = ParsedSpans::new(
                unsorted,
                Vec::new(),
                Vec::new(),
                Vec::new(),
                Vec::new(),
                Vec::new(),
                Vec::new(),
            );
        });
        let Err(msg) = result else {
            panic!("expected panic")
        };
        let text = msg.downcast_ref::<String>().map_or_else(
            || {
                msg.downcast_ref::<&str>()
                    .map_or(String::new(), |s| (*s).to_string())
            },
            Clone::clone,
        );
        assert!(text.contains("imports not sorted"));
        assert!(text.contains("0..1"));
    }

    #[test]
    fn build_green_tree_reports_all_errors() {
        let imports = vec![1..2, 0..1];
        let typedefs = vec![4..5, 3..4];
        let result = std::panic::catch_unwind(|| {
            let _ = ParsedSpans::new(
                imports,
                typedefs,
                Vec::new(),
                Vec::new(),
                Vec::new(),
                Vec::new(),
                Vec::new(),
            );
        });
        let Err(msg) = result else {
            panic!("expected panic")
        };
        let text = msg.downcast_ref::<String>().map_or_else(
            || {
                msg.downcast_ref::<&str>()
                    .map_or(String::new(), |s| (*s).to_string())
            },
            Clone::clone,
        );
        assert!(text.contains("imports not sorted"));
        assert!(text.contains("typedefs not sorted"));
    }

    #[rstest]
    fn build_green_tree_round_trip() {
        let src = "import foo::bar;";
        let tokens = tokenize(src);
        let (spans, errors) = super::super::span_scanner::parse_tokens(&tokens, src);
        assert!(errors.is_empty());
        let green = build_green_tree(&tokens, src, &spans);
        let root = super::super::ast::Root::from_green(green);
        assert_eq!(root.text(), src);
    }
}
