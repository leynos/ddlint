//! Chumsky-based parser producing a rowan CST.
//!
//! This module contains the entry point for parsing `DDlog` source code.
//! The parser tokenises the input and wraps tokens into a `rowan::GreenNode`,
//! with support for parsing imports, typedefs, relations, indexes, functions,
//! and rules. It lays down the framework for integrating `chumsky` combinators
//! and error recovery in later stages.

use chumsky::prelude::*;
use log::warn;
use rowan::{GreenNode, GreenNodeBuilder, Language};

use crate::{DdlogLanguage, Span, SyntaxKind, tokenize};

#[macro_use]
mod lexer_helpers;

mod token_stream;

mod span_collector;

mod span_scanner;
use span_scanner::parse_tokens;

/// Result of a parse operation.
#[derive(Debug)]
pub struct Parsed {
    green: GreenNode,
    root: ast::Root,
    errors: Vec<Simple<SyntaxKind>>,
}

/// Spans for each parsed statement category.
///
/// Instances are constructed via [`ParsedSpans::new`] to guarantee the
/// internal lists remain sorted and non-overlapping.
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
    /// Construct a new `ParsedSpans`.
    ///
    /// The caller must ensure each span list is sorted and does not overlap
    /// with itself.
    ///
    /// # Panics
    ///
    /// Panics in debug builds if any span list is unsorted or contains
    /// overlapping spans.
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

impl Parsed {
    /// Access the `rowan` green tree.
    #[must_use]
    pub fn green(&self) -> &GreenNode {
        &self.green
    }

    /// Access the typed AST root.
    #[must_use]
    pub fn root(&self) -> &ast::Root {
        &self.root
    }

    /// Access parser errors collected during recovery.
    #[must_use]
    pub fn errors(&self) -> &[Simple<SyntaxKind>] {
        &self.errors
    }
}

/// Parse the provided source string.
///
/// The function tokenises the source using [`tokenize`], then uses a minimal
/// `chumsky` parser to wrap those tokens into a CST. Syntactic error recovery
/// will insert `N_ERROR` nodes when grammar rules fail once they exist.
///
/// # Examples
///
/// ```rust,no_run
/// use ddlint::parse;
///
/// let parsed = parse("input relation R(x: u32);");
/// assert!(parsed.errors().is_empty());
/// assert_eq!(parsed.root().relations().len(), 1);
/// ```
#[must_use]
pub fn parse(src: &str) -> Parsed {
    let tokens = tokenize(src);
    let (spans, errors) = parse_tokens(&tokens, src);

    let green = build_green_tree(&tokens, src, &spans);
    let root = ast::Root::from_green(green.clone());

    Parsed {
        green,
        root,
        errors,
    }
}

/// Construct the CST from the token stream and recorded statement spans.
///
/// `spans.imports()` and `spans.typedefs()` must be sorted and non-overlapping so
/// that tokens are wrapped into well-formed nodes during tree construction.
/// Spans are validated in debug builds when [`ParsedSpans`] is constructed.
fn build_green_tree(tokens: &[(SyntaxKind, Span)], src: &str, spans: &ParsedSpans) -> GreenNode {
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

/// Move the iterator forward past any spans that end before `pos`.
///
/// This keeps the peeked span aligned with the current token position.
fn advance_span_iter(iter: &mut std::iter::Peekable<std::slice::Iter<'_, Span>>, pos: usize) {
    while let Some(next) = iter.peek() {
        if pos >= next.end {
            iter.next();
        } else {
            break;
        }
    }
}

/// Start a new syntax node if the current position matches the start of a span.
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

/// Finish the active syntax node when the current position reaches its end.
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

/// Validate that spans are sorted and non-overlapping.
///
/// Returns an error describing the offending pair if any two consecutive spans
/// overlap or are out of order. This helps callers diagnose invalid span lists
/// before corrupting the CST.
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

/// Panics if any span list is misordered, aggregating all violations.
fn ensure_span_lists_sorted(lists: &[(&str, &[Span])]) {
    let mut errors = Vec::new();
    for (name, spans) in lists {
        if let Err(e) = validate_spans_sorted(spans) {
            errors.push(format!("{name} not sorted: {e}"));
        }
    }
    assert!(errors.is_empty(), "{}", errors.join("\n"));
}

/// Push a token to the tree, wrapping `N_ERROR` tokens in an error node.
fn push_token(builder: &mut GreenNodeBuilder, kind: SyntaxKind, span: &Span, src: &str) {
    // `Span` is cloned because `str::get` takes the range by value.
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

pub mod ast {
    //! Minimal typed AST wrappers used by the parser.
    //!
    //! This layer will expand as grammar rules are implemented. For now it
    //! exposes only the root node so tests and higher layers can navigate the
    //! parsed CST.

    use rowan::{GreenNode, SyntaxElement, SyntaxNode};

    use crate::{DdlogLanguage, SyntaxKind};

    /// Internal trait implemented by all AST wrappers.
    #[doc(hidden)]
    pub trait AstNode {
        /// Access the underlying syntax node.
        fn syntax(&self) -> &SyntaxNode<DdlogLanguage>;
    }

    /// The root of a parsed `DDlog` file.
    ///
    /// Provides typed access to the syntax tree root node with methods
    /// for navigation and introspection.
    #[derive(Debug, Clone)]
    pub struct Root {
        pub(crate) syntax: SyntaxNode<DdlogLanguage>,
    }

    impl Root {
        /// Obtain the underlying syntax node.
        #[must_use]
        pub fn syntax(&self) -> &SyntaxNode<DdlogLanguage> {
            &self.syntax
        }

        /// Create a new `Root` from a green node.
        #[must_use]
        pub fn from_green(green: GreenNode) -> Self {
            Self {
                syntax: SyntaxNode::<DdlogLanguage>::new_root(green),
            }
        }

        /// The kind of this root node. Provided for completeness.
        #[must_use]
        pub fn kind(&self) -> SyntaxKind {
            self.syntax.kind()
        }

        /// Get the text range covered by this root node.
        #[must_use]
        pub fn text_range(&self) -> rowan::TextRange {
            self.syntax.text_range()
        }

        /// Get the text content of this root node.
        #[must_use]
        pub fn text(&self) -> String {
            self.syntax.text().to_string()
        }

        /// Collect all `import` statements under this root.
        ///
        /// # Examples
        ///
        /// ```no_run
        /// # use crate::parser;
        /// let parsed = parser::parse("import foo::bar as baz;");
        /// let imports = parsed.root().imports();
        /// assert_eq!(imports.len(), 1);
        /// ```
        #[must_use]
        pub fn imports(&self) -> Vec<Import> {
            self.syntax
                .children()
                .filter(|n| n.kind() == SyntaxKind::N_IMPORT_STMT)
                .map(|syntax| Import { syntax })
                .collect()
        }

        /// Collect all `typedef` declarations under this root.
        ///
        /// # Examples
        ///
        /// ```no_run
        /// # use crate::parser;
        /// let parsed = parser::parse("typedef Foo = Bar;");
        /// let defs = parsed.root().type_defs();
        /// assert_eq!(defs.len(), 1);
        /// ```
        #[must_use]
        pub fn type_defs(&self) -> Vec<TypeDef> {
            self.syntax
                .children()
                .filter(|n| n.kind() == SyntaxKind::N_TYPE_DEF)
                .map(|syntax| TypeDef { syntax })
                .collect()
        }

        /// Collect all relation declarations under this root.
        ///
        /// # Examples
        ///
        /// ```no_run
        /// # use crate::parser;
        /// let parsed = parser::parse("relation User(id: u32)");
        /// let relations = parsed.root().relations();
        /// assert_eq!(relations.len(), 1);
        /// ```
        #[must_use]
        pub fn relations(&self) -> Vec<Relation> {
            self.syntax
                .children()
                .filter(|n| n.kind() == SyntaxKind::N_RELATION_DECL)
                .map(|syntax| Relation { syntax })
                .collect()
        }

        /// Collect all index declarations under this root.
        ///
        /// # Examples
        ///
        /// ```no_run
        /// # use crate::parser;
        /// let parsed = parser::parse("index Idx on User(id)");
        /// let indexes = parsed.root().indexes();
        /// assert_eq!(indexes.len(), 1);
        /// ```
        #[must_use]
        pub fn indexes(&self) -> Vec<Index> {
            self.syntax
                .children()
                .filter(|n| n.kind() == SyntaxKind::N_INDEX)
                .map(|syntax| Index { syntax })
                .collect()
        }

        /// Collect all function declarations under this root.
        ///
        /// # Examples
        ///
        /// ```no_run
        /// # use crate::parser;
        /// let parsed = parser::parse("function f() {}");
        /// let funcs = parsed.root().functions();
        /// assert_eq!(funcs.len(), 1);
        /// ```
        #[must_use]
        pub fn functions(&self) -> Vec<Function> {
            self.syntax
                .children()
                .filter(|n| n.kind() == SyntaxKind::N_FUNCTION)
                .map(|syntax| Function { syntax })
                .collect()
        }

        /// Collect all transformer declarations under this root.
        ///
        /// # Examples
        ///
        /// ```no_run
        /// # use crate::parser;
        /// let parsed = parser::parse("extern transformer t(i: In): Out");
        /// let ts = parsed.root().transformers();
        /// assert_eq!(ts.len(), 1);
        /// ```
        #[must_use]
        pub fn transformers(&self) -> Vec<Transformer> {
            self.syntax
                .children()
                .filter(|n| n.kind() == SyntaxKind::N_TRANSFORMER)
                .map(|syntax| Transformer { syntax })
                .collect()
        }

        /// Collect all rule declarations under this root.
        ///
        /// # Examples
        ///
        /// ```no_run
        /// # use crate::parser;
        /// let parsed = parser::parse("A(x) :- B(x).");
        /// let rules = parsed.root().rules();
        /// assert_eq!(rules.len(), 1);
        /// ```
        #[must_use]
        pub fn rules(&self) -> Vec<Rule> {
            self.syntax
                .children()
                .filter(|n| n.kind() == SyntaxKind::N_RULE)
                .map(|syntax| Rule { syntax })
                .collect()
        }
    }

    /// Typed wrapper for an `import` statement.
    #[derive(Debug, Clone)]
    pub struct Import {
        pub(crate) syntax: SyntaxNode<DdlogLanguage>,
    }

    impl Import {
        /// The module path text as written in the source.
        ///
        /// # Examples
        ///
        /// ```no_run
        /// # use crate::parser;
        /// let parsed = parser::parse("import foo::bar as baz;");
        /// let import = parsed.root().imports().first().unwrap();
        /// assert_eq!(import.path(), "foo::bar");
        /// ```
        #[must_use]
        pub fn path(&self) -> String {
            self.syntax
                .children_with_tokens()
                .skip_while(|e| !matches!(e.kind(), SyntaxKind::K_IMPORT))
                .skip(1)
                .take_while(|e| !matches!(e.kind(), SyntaxKind::K_AS))
                .filter_map(|e| match e {
                    rowan::NodeOrToken::Token(t) if t.kind() == SyntaxKind::T_IDENT => {
                        Some(t.text().to_string())
                    }
                    rowan::NodeOrToken::Token(t) if t.kind() == SyntaxKind::T_COLON_COLON => {
                        Some("::".to_string())
                    }
                    _ => None,
                })
                .collect::<String>()
        }

        /// The alias assigned with `as`, if any.
        ///
        /// # Examples
        ///
        /// ```no_run
        /// # use crate::parser;
        /// let parsed = parser::parse("import foo as bar;");
        /// let import = parsed.root().imports().first().unwrap();
        /// assert_eq!(import.alias(), Some("bar".to_string()));
        /// ```
        #[must_use]
        pub fn alias(&self) -> Option<String> {
            self.syntax
                .children_with_tokens()
                .skip_while(|e| !matches!(e.kind(), SyntaxKind::K_AS))
                .skip(1)
                .find_map(|e| match e {
                    rowan::NodeOrToken::Token(t) if t.kind() == SyntaxKind::T_IDENT => {
                        Some(t.text().to_string())
                    }
                    _ => None,
                })
        }
    }

    impl AstNode for Import {
        fn syntax(&self) -> &SyntaxNode<DdlogLanguage> {
            &self.syntax
        }
    }

    /// Typed wrapper for a `typedef` or `extern type` declaration.
    #[derive(Debug, Clone)]
    pub struct TypeDef {
        pub(crate) syntax: SyntaxNode<DdlogLanguage>,
    }

    impl TypeDef {
        /// Name of the defined type.
        ///
        /// # Examples
        ///
        /// ```no_run
        /// # use crate::parser;
        /// let parsed = parser::parse("typedef Foo = Bar;");
        /// let td = parsed.root().type_defs().first().unwrap();
        /// assert_eq!(td.name(), Some("Foo".to_string()));
        /// ```
        #[must_use]
        pub fn name(&self) -> Option<String> {
            let mut iter = self.syntax.children_with_tokens().peekable();
            if !skip_to_typedef_keyword(&mut iter) {
                return None;
            }
            take_first_ident(iter)
        }

        /// Whether this declaration is `extern`.
        ///
        /// # Examples
        ///
        /// ```no_run
        /// # use crate::parser;
        /// let parsed = parser::parse("extern type Baz;");
        /// let td = parsed.root().type_defs().first().unwrap();
        /// assert!(td.is_extern());
        /// ```
        #[must_use]
        pub fn is_extern(&self) -> bool {
            self.syntax
                .children_with_tokens()
                .any(|e| e.kind() == SyntaxKind::K_EXTERN)
        }
    }

    impl AstNode for TypeDef {
        fn syntax(&self) -> &SyntaxNode<DdlogLanguage> {
            &self.syntax
        }
    }

    /// Advance the iterator until `predicate` returns `true` for a token kind.
    fn skip_to_match(
        iter: &mut impl Iterator<Item = rowan::SyntaxElement<DdlogLanguage>>,
        predicate: impl Fn(SyntaxKind) -> bool,
    ) -> bool {
        for e in iter.by_ref() {
            let kind = e.kind();
            if kind == SyntaxKind::K_EXTERN {
                continue;
            }
            if predicate(kind) {
                return true;
            }
        }
        false
    }

    /// Advance the iterator until `typedef` or `type` is encountered.
    fn skip_to_typedef_keyword(
        iter: &mut impl Iterator<Item = rowan::SyntaxElement<DdlogLanguage>>,
    ) -> bool {
        skip_to_match(iter, |k| {
            matches!(k, SyntaxKind::K_TYPEDEF | SyntaxKind::K_TYPE)
        })
    }

    /// Advance the iterator until `transformer` is encountered.
    fn skip_to_transformer_keyword(
        iter: &mut impl Iterator<Item = rowan::SyntaxElement<DdlogLanguage>>,
    ) -> bool {
        skip_to_match(iter, |k| k == SyntaxKind::K_TRANSFORMER)
    }

    fn take_first_ident(
        iter: impl Iterator<Item = rowan::SyntaxElement<DdlogLanguage>>,
    ) -> Option<String> {
        use rowan::NodeOrToken;
        for e in iter {
            match e {
                NodeOrToken::Token(t) if t.kind() == SyntaxKind::T_IDENT => {
                    return Some(t.text().to_string());
                }
                NodeOrToken::Token(t)
                    if matches!(t.kind(), SyntaxKind::T_WHITESPACE | SyntaxKind::T_COMMENT) => {}
                _ => return None,
            }
        }
        None
    }

    pub(super) fn skip_whitespace_and_comments<I>(iter: &mut std::iter::Peekable<I>)
    where
        I: Iterator<Item = SyntaxElement<DdlogLanguage>>,
    {
        while matches!(
            iter.peek().map(SyntaxElement::kind),
            Some(SyntaxKind::T_WHITESPACE | SyntaxKind::T_COMMENT)
        ) {
            iter.next();
        }
    }

    pub(super) mod parse_utils;
    use parse_utils::{parse_name_type_pairs, parse_output_list, parse_type_after_colon};

    /// Typed wrapper for a relation declaration.
    #[derive(Debug, Clone)]
    pub struct Relation {
        pub(crate) syntax: SyntaxNode<DdlogLanguage>,
    }

    impl Relation {
        /// Name of the relation if present.
        ///
        /// # Examples
        ///
        /// ```no_run
        /// # use crate::parser;
        /// let parsed = parser::parse("relation Person(name: string)");
        /// let rel = parsed.root().relations().first().unwrap();
        /// assert_eq!(rel.name(), Some("Person".to_string()));
        /// ```
        #[must_use]
        pub fn name(&self) -> Option<String> {
            self.syntax
                .children_with_tokens()
                .skip_while(|e| !matches!(e.kind(), SyntaxKind::K_RELATION))
                .skip(1)
                .find_map(|e| match e {
                    rowan::NodeOrToken::Token(t) if t.kind() == SyntaxKind::T_IDENT => {
                        Some(t.text().to_string())
                    }
                    _ => None,
                })
        }

        /// Whether the relation is declared as `input`.
        ///
        /// # Examples
        ///
        /// ```no_run
        /// # use crate::parser;
        /// let parsed = parser::parse("input relation Data(x: i32)");
        /// let rel = parsed.root().relations().first().unwrap();
        /// assert!(rel.is_input());
        /// ```
        #[must_use]
        pub fn is_input(&self) -> bool {
            self.syntax
                .children_with_tokens()
                .any(|e| e.kind() == SyntaxKind::K_INPUT)
        }

        /// Returns `true` if the relation is declared with the `output` keyword.
        ///
        /// # Examples
        ///
        /// ```no_run
        /// # use crate::parser::ast::{Relation, Root};
        /// # let src = "output MyRel(x: u32)";
        /// # let parsed = crate::parser::parse(src);
        /// # let rel = parsed.root.relations().next().unwrap();
        /// assert!(rel.is_output());
        /// ```
        #[must_use]
        pub fn is_output(&self) -> bool {
            self.syntax
                .children_with_tokens()
                .any(|e| e.kind() == SyntaxKind::K_OUTPUT)
        }

        /// Returns the columns declared for the relation as name/type pairs.
        ///
        /// Delimiter errors encountered during parsing are currently ignored,
        /// but this behaviour may change in future versions to surface such diagnostics.
        ///
        /// # Examples
        ///
        /// ```no_run
        /// # use your_crate::parser::ast::{Relation, Root};
        /// # let src = "relation Person(name: string, age: integer)";
        /// # let parsed = your_crate::parser::parse(src);
        /// # let root = parsed.root();
        /// # let relation = root.relations().next().unwrap();
        /// let columns = relation.columns();
        /// assert_eq!(columns, vec![("name".to_string(), "string".to_string()), ("age".to_string(), "integer".to_string())]);
        /// ```
        #[must_use]
        pub fn columns(&self) -> Vec<(String, String)> {
            let (pairs, errors) = parse_name_type_pairs(self.syntax.children_with_tokens());
            // Delimiter errors are ignored for now. Future versions may surface them.
            let _ = errors;
            pairs
        }

        /// Primary key column names if specified.
        ///
        /// # Examples
        ///
        /// ```no_run
        /// # use crate::parser;
        /// let src = "relation Person(name: string, age: u32) primary key(name)";
        /// let rel = parser::parse(src).root().relations().first().unwrap();
        /// assert_eq!(rel.primary_key(), Some(vec!["name".to_string()]));
        /// ```
        #[must_use]
        pub fn primary_key(&self) -> Option<Vec<String>> {
            use rowan::NodeOrToken;

            let mut iter = self.syntax.children_with_tokens().peekable();
            // Skip to the first '(' and consume column list
            for e in &mut iter {
                if e.kind() == SyntaxKind::T_LPAREN {
                    break;
                }
            }
            let mut depth = 1usize;
            for e in iter.by_ref() {
                match e.kind() {
                    SyntaxKind::T_LPAREN => depth += 1,
                    SyntaxKind::T_RPAREN => {
                        depth -= 1;
                        if depth == 0 {
                            break;
                        }
                    }
                    _ => {}
                }
            }

            // Skip whitespace/comments
            skip_whitespace_and_comments(&mut iter);

            match iter.next() {
                Some(NodeOrToken::Token(t))
                    if t.kind() == SyntaxKind::T_IDENT && t.text() == "primary" => {}
                _ => return None,
            }

            skip_whitespace_and_comments(&mut iter);

            match iter.next() {
                Some(NodeOrToken::Token(t))
                    if t.kind() == SyntaxKind::T_IDENT && t.text() == "key" => {}
                _ => return None,
            }

            skip_whitespace_and_comments(&mut iter);

            if !matches!(
                iter.peek().map(rowan::SyntaxElement::kind),
                Some(SyntaxKind::T_LPAREN)
            ) {
                return None;
            }
            iter.next(); // consume '('
            depth = 1;
            let mut buf = String::new();
            for e in iter.by_ref() {
                match e {
                    NodeOrToken::Token(t) => match t.kind() {
                        SyntaxKind::T_LPAREN => {
                            depth += 1;
                            buf.push_str(t.text());
                        }
                        SyntaxKind::T_RPAREN => {
                            if depth == 1 {
                                break;
                            }
                            depth -= 1;
                            buf.push_str(t.text());
                        }
                        _ => buf.push_str(t.text()),
                    },
                    NodeOrToken::Node(n) => buf.push_str(&n.text().to_string()),
                }
                if depth == 0 {
                    break;
                }
            }

            let keys = buf
                .split(',')
                .map(|s| s.trim().to_string())
                .filter(|s| !s.is_empty())
                .collect::<Vec<_>>();
            if keys.is_empty() { None } else { Some(keys) }
        }
    }

    impl AstNode for Relation {
        fn syntax(&self) -> &SyntaxNode<DdlogLanguage> {
            &self.syntax
        }
    }

    /// Typed wrapper for an index declaration.
    #[derive(Debug, Clone)]
    pub struct Index {
        pub(crate) syntax: SyntaxNode<DdlogLanguage>,
    }

    impl Index {
        /// Name of the index if present.
        ///
        /// # Examples
        ///
        /// ```no_run
        /// # use crate::parser;
        /// let parsed = parser::parse("index Idx on User(id)");
        /// let idx = parsed.root().indexes().first().unwrap();
        /// assert_eq!(idx.name(), Some("Idx".to_string()));
        /// ```
        #[must_use]
        pub fn name(&self) -> Option<String> {
            self.syntax
                .children_with_tokens()
                .skip_while(|e| !matches!(e.kind(), SyntaxKind::K_INDEX))
                .skip(1)
                .find_map(|e| match e {
                    rowan::NodeOrToken::Token(t) if t.kind() == SyntaxKind::T_IDENT => {
                        Some(t.text().to_string())
                    }
                    _ => None,
                })
        }

        /// Target relation name.
        ///
        /// # Examples
        ///
        /// ```no_run
        /// # use crate::parser;
        /// let parsed = parser::parse("index Idx on User(id)");
        /// let idx = parsed.root().indexes().first().unwrap();
        /// assert_eq!(idx.relation(), Some("User".to_string()));
        /// ```
        #[must_use]
        pub fn relation(&self) -> Option<String> {
            self.syntax
                .children_with_tokens()
                .skip_while(|e| !matches!(e.kind(), SyntaxKind::K_ON))
                .skip(1)
                .find_map(|e| match e {
                    rowan::NodeOrToken::Token(t) if t.kind() == SyntaxKind::T_IDENT => {
                        Some(t.text().to_string())
                    }
                    _ => None,
                })
        }

        /// Column expressions included in the index.
        ///
        /// The method collects the text of each column expression, allowing for
        /// nested parentheses in cases like `lower(name)` or
        /// `func(col, other(col2))`.
        ///
        /// # Examples
        ///
        /// ```no_run
        /// # use crate::parser;
        /// let parsed = parser::parse("index Idx on User(lower(name))");
        /// let idx = parsed.root().indexes().first().unwrap();
        /// assert_eq!(idx.columns(), vec!["lower(name)".to_string()]);
        /// ```
        #[must_use]
        pub fn columns(&self) -> Vec<String> {
            use rowan::NodeOrToken;

            let mut iter = self.syntax.children_with_tokens().peekable();

            // Skip tokens up to the opening parenthesis after the relation name
            for e in &mut iter {
                if e.kind() == SyntaxKind::T_LPAREN {
                    break;
                }
            }

            let mut cols = Vec::new();
            let mut buf = String::new();
            let mut depth = 0usize;

            for e in iter {
                match e {
                    NodeOrToken::Token(t) => match t.kind() {
                        SyntaxKind::T_LPAREN => {
                            depth += 1;
                            buf.push_str(t.text());
                        }
                        SyntaxKind::T_RPAREN => {
                            if depth == 0 {
                                let col = buf.trim();
                                if !col.is_empty() {
                                    cols.push(col.to_string());
                                }
                                break;
                            }
                            depth -= 1;
                            buf.push_str(t.text());
                        }
                        SyntaxKind::T_COMMA if depth == 0 => {
                            let col = buf.trim();
                            if !col.is_empty() {
                                cols.push(col.to_string());
                            }
                            buf.clear();
                        }
                        _ => buf.push_str(t.text()),
                    },
                    NodeOrToken::Node(n) => buf.push_str(&n.text().to_string()),
                }
            }

            cols
        }
    }

    impl AstNode for Index {
        fn syntax(&self) -> &SyntaxNode<DdlogLanguage> {
            &self.syntax
        }
    }

    /// Typed wrapper for a rule declaration.
    #[derive(Debug, Clone)]
    pub struct Rule {
        pub(crate) syntax: SyntaxNode<DdlogLanguage>,
    }

    impl Rule {
        /// Text of the rule head atom.
        ///
        /// # Examples
        ///
        /// ```no_run
        /// # use crate::parser;
        /// let parsed = parser::parse("A(x) :- B(x).");
        /// let rule = parsed.root().rules().first().unwrap();
        /// assert_eq!(rule.head(), Some("A(x)".to_string()));
        /// ```
        #[must_use]
        pub fn head(&self) -> Option<String> {
            use rowan::NodeOrToken;

            let mut buf = String::new();
            for e in self.syntax.children_with_tokens() {
                match e {
                    NodeOrToken::Token(t) => match t.kind() {
                        SyntaxKind::T_IMPLIES | SyntaxKind::T_DOT => break,
                        _ => buf.push_str(t.text()),
                    },
                    NodeOrToken::Node(n) => buf.push_str(&n.text().to_string()),
                }
            }

            let text = buf.trim();
            if text.is_empty() {
                None
            } else {
                Some(text.to_string())
            }
        }

        /// Text of each body literal in order of appearance.
        ///
        /// # Examples
        ///
        /// ```no_run
        /// # use crate::parser;
        /// let parsed = parser::parse("A(x) :- B(x), C(y).");
        /// let rule = parsed.root().rules().first().unwrap();
        /// assert_eq!(rule.body_literals(), vec!["B(x)".to_string(), "C(y)".to_string()]);
        /// ```
        #[must_use]
        pub fn body_literals(&self) -> Vec<String> {
            use rowan::NodeOrToken;

            let mut iter = self
                .syntax
                .children_with_tokens()
                .skip_while(|e| e.kind() != SyntaxKind::T_IMPLIES);

            // Skip the ':-' token if present
            if matches!(iter.next().map(|e| e.kind()), Some(SyntaxKind::T_IMPLIES)) {
                let mut buf = String::new();
                let mut lits = Vec::new();
                for e in iter {
                    match e {
                        NodeOrToken::Token(t) => match t.kind() {
                            SyntaxKind::T_COMMA => {
                                let lit = buf.trim();
                                if !lit.is_empty() {
                                    lits.push(lit.to_string());
                                }
                                buf.clear();
                            }
                            SyntaxKind::T_DOT => {
                                let lit = buf.trim();
                                if !lit.is_empty() {
                                    lits.push(lit.to_string());
                                }
                                break;
                            }
                            _ => buf.push_str(t.text()),
                        },
                        NodeOrToken::Node(n) => buf.push_str(&n.text().to_string()),
                    }
                }
                lits
            } else {
                Vec::new()
            }
        }
    }

    impl AstNode for Rule {
        fn syntax(&self) -> &SyntaxNode<DdlogLanguage> {
            &self.syntax
        }
    }

    /// Typed wrapper for a function declaration or definition.
    #[derive(Debug, Clone)]
    pub struct Function {
        pub(crate) syntax: SyntaxNode<DdlogLanguage>,
    }

    impl Function {
        /// Name of the function if present.
        ///
        /// # Examples
        ///
        /// ```no_run
        /// # use crate::parser;
        /// let parsed = parser::parse("function foo() {}");
        /// let func = parsed.root().functions().first().unwrap();
        /// assert_eq!(func.name(), Some("foo".to_string()));
        /// ```
        #[must_use]
        pub fn name(&self) -> Option<String> {
            self.syntax
                .children_with_tokens()
                .skip_while(|e| !matches!(e.kind(), SyntaxKind::K_FUNCTION))
                .skip(1)
                .find_map(|e| match e {
                    rowan::NodeOrToken::Token(t) if t.kind() == SyntaxKind::T_IDENT => {
                        Some(t.text().to_string())
                    }
                    _ => None,
                })
        }

        /// Returns `true` if the function is declared with the `extern` keyword.
        ///
        /// # Examples
        ///
        /// ```no_run
        /// # use crate::parser::ast::{Function, Root};
        /// # let src = "extern function foo(x: i32): i32;";
        /// # let parsed = crate::parser::parse(src);
        /// # let root = parsed.root();
        /// # let func = root.functions().next().unwrap();
        /// assert!(func.is_extern());
        /// ```
        #[must_use]
        pub fn is_extern(&self) -> bool {
            self.syntax
                .children_with_tokens()
                .any(|e| e.kind() == SyntaxKind::K_EXTERN)
        }

        /// Returns the function parameters as pairs of parameter name and type.
        ///
        /// Delimiter errors encountered during parsing are currently ignored,
        /// but this behaviour may change in future versions to surface such diagnostics.
        ///
        /// # Examples
        ///
        /// ```no_run
        /// # use crate::parser::ast::{Function, Root};
        /// # let src = "function foo(x: i32, y: string): bool { ... }";
        /// # let parsed = crate::parser::parse(src);
        /// # let root = parsed.root();
        /// # let func = root.functions().next().unwrap();
        /// let params = func.parameters();
        /// assert_eq!(params, vec![("x".to_string(), "i32".to_string()), ("y".to_string(), "string".to_string())]);
        /// ```
        #[must_use]
        pub fn parameters(&self) -> Vec<(String, String)> {
            let (pairs, errors) = parse_name_type_pairs(self.syntax.children_with_tokens());
            // Delimiter errors are ignored for now. Future versions may surface them.
            let _ = errors;
            pairs
        }

        /// Return type text if specified.
        ///
        /// # Examples
        ///
        /// ```no_run
        /// # use crate::parser;
        /// let parsed = parser::parse("function f(): bool {}");
        /// let func = parsed.root().functions().first().unwrap();
        /// assert_eq!(func.return_type(), Some("bool".to_string()));
        /// ```
        #[must_use]
        pub fn return_type(&self) -> Option<String> {
            let mut iter = self.syntax.children_with_tokens().peekable();
            // Skip to ')' after parameters
            let mut depth = 0usize;
            for e in &mut iter {
                match e.kind() {
                    SyntaxKind::T_LPAREN => depth += 1,
                    SyntaxKind::T_RPAREN => {
                        depth -= 1;
                        if depth == 0 {
                            break;
                        }
                    }
                    _ => {}
                }
            }

            parse_type_after_colon(&mut iter)
        }
    }

    impl AstNode for Function {
        fn syntax(&self) -> &SyntaxNode<DdlogLanguage> {
            &self.syntax
        }
    }

    /// Typed wrapper for a transformer declaration.
    #[derive(Debug, Clone)]
    pub struct Transformer {
        pub(crate) syntax: SyntaxNode<DdlogLanguage>,
    }

    impl Transformer {
        /// Name of the transformer if present.
        ///
        /// # Examples
        ///
        /// ```no_run
        /// # use crate::parser;
        /// let parsed = parser::parse("extern transformer t(x: In): Out");
        /// let tr = parsed.root().transformers().first().unwrap();
        /// assert_eq!(tr.name(), Some("t".to_string()));
        /// ```
        #[must_use]
        pub fn name(&self) -> Option<String> {
            let mut iter = self.syntax.children_with_tokens();
            if !skip_to_transformer_keyword(&mut iter) {
                return None;
            }
            take_first_ident(iter)
        }

        /// Input relations as pairs of name and type.
        ///
        /// # Examples
        ///
        /// ```no_run
        /// # use crate::parser;
        /// let parsed = parser::parse("extern transformer t(x: In): Out");
        /// let tr = parsed.root().transformers().first().unwrap();
        /// assert_eq!(tr.inputs(), vec![("x".to_string(), "In".to_string())]);
        /// ```
        #[must_use]
        pub fn inputs(&self) -> Vec<(String, String)> {
            let (pairs, _errors) = parse_name_type_pairs(self.syntax.children_with_tokens());
            pairs
        }

        /// Output relation names.
        ///
        /// # Examples
        ///
        /// ```no_run
        /// # use crate::parser;
        /// let parsed = parser::parse("extern transformer t(x: In): Out1, Out2");
        /// let tr = parsed.root().transformers().first().unwrap();
        /// assert_eq!(tr.outputs(), vec!["Out1".to_string(), "Out2".to_string()]);
        /// ```
        #[must_use]
        pub fn outputs(&self) -> Vec<String> {
            parse_output_list(self.syntax.children_with_tokens())
        }
    }

    impl AstNode for Transformer {
        fn syntax(&self) -> &SyntaxNode<DdlogLanguage> {
            &self.syntax
        }
    }
}

#[cfg(test)]
mod tests {
    use super::token_stream::TokenStream;
    use super::*;
    use crate::tokenize;
    use rstest::rstest;

    /// Tests that `skip_until` advances the token stream cursor past the specified span end.
    ///
    /// # Examples
    ///
    /// ```no_run
    /// let src = "import foo\n";
    /// let tokens = tokenize(src);
    /// let mut stream = TokenStream::new(&tokens, src);
    /// let end = stream.line_end(0);
    /// stream.skip_until(end);
    /// assert_eq!(stream.cursor(), tokens.len());
    /// ```
    #[rstest]
    fn skip_until_advances_past_span() {
        let src = "import foo\n";
        let tokens = tokenize(src);
        let mut stream = TokenStream::new(&tokens, src);
        let end = stream.line_end(0);
        stream.skip_until(end);
        assert_eq!(stream.cursor(), tokens.len());
    }

    /// Tests that `TokenStream::line_end` returns the position immediately after the end of the current line.
    ///
    /// # Examples
    ///
    /// ```no_run
    /// let src = "typedef A = string\nnext";
    /// let tokens = tokenize(src);
    /// let stream = TokenStream::new(&tokens, src);
    /// let start = 1; // token after 'typedef'
    /// let end = stream.line_end(start);
    /// let newline = src.find('\n').unwrap_or_else(|| panic!("newline missing"));
    /// assert_eq!(end, newline + 1);
    /// ```
    #[rstest]
    fn line_end_returns_span_end() {
        let src = "typedef A = string\nnext";
        let tokens = tokenize(src);
        let stream = TokenStream::new(&tokens, src);
        let start = 1; // token after 'typedef'
        let end = stream.line_end(start);
        let newline = src.find('\n').unwrap_or_else(|| panic!("newline missing"));
        assert_eq!(end, newline + 1);
    }

    /// Tests that `skip_ws_inline` correctly skips inline whitespace tokens in the token stream.
    ///
    /// # Examples
    ///
    /// ```no_run
    /// let src = "extern    type Foo";
    /// let tokens = tokenize(src);
    /// let mut stream = TokenStream::new(&tokens, src);
    /// stream.advance();
    /// stream.skip_ws_inline();
    /// assert!(matches!(
    ///     stream.peek().map(|t| t.0),
    ///     Some(SyntaxKind::K_TYPE)
    /// ));
    /// ```
    #[rstest]
    fn skip_ws_inline_skips_spaces() {
        let src = "extern    type Foo";
        let tokens = tokenize(src);
        let mut stream = TokenStream::new(&tokens, src);
        stream.advance();
        stream.skip_ws_inline();
        assert!(matches!(
            stream.peek().map(|t| t.0),
            Some(SyntaxKind::K_TYPE)
        ));
    }

    /// Tests that `line_end` returns the length of the source string when called with an out-of-bounds index.
    ///
    /// # Examples
    ///
    /// ```no_run
    /// let src = "typedef A = string\n";
    /// let tokens = tokenize(src);
    /// let stream = TokenStream::new(&tokens, src);
    /// let start = tokens.len();
    /// assert_eq!(stream.line_end(start), src.len());
    /// ```
    #[rstest]
    fn line_end_out_of_bounds_returns_len() {
        let src = "typedef A = string\n";
        let tokens = tokenize(src);
        let stream = TokenStream::new(&tokens, src);
        let start = tokens.len();
        assert_eq!(stream.line_end(start), src.len());
    }

    #[test]
    fn validate_spans_sorted_err_on_overlap() {
        let spans = vec![0..5, 4..8];
        let result = super::validate_spans_sorted(&spans);
        assert!(result.is_err());
    }

    #[test]
    fn validate_spans_sorted_err_on_unsorted() {
        let spans = vec![5..10, 0..2];
        let result = super::validate_spans_sorted(&spans);
        assert!(result.is_err());
    }

    #[test]
    fn validate_spans_sorted_ok_on_empty() {
        let spans: Vec<Span> = Vec::new();
        assert!(super::validate_spans_sorted(&spans).is_ok());
    }

    #[test]
    fn validate_spans_sorted_ok_on_single() {
        let spans: Vec<Span> = vec![std::ops::Range { start: 0, end: 3 }];
        assert!(super::validate_spans_sorted(&spans).is_ok());
    }

    #[test]
    fn validate_spans_sorted_ok_on_sorted() {
        let spans = vec![0..2, 3..5, 5..8];
        assert!(super::validate_spans_sorted(&spans).is_ok());
    }

    #[test]
    fn build_green_tree_panics_on_misordered_spans() {
        let unsorted = vec![1..2, 0..1];
        let result = std::panic::catch_unwind(|| {
            let _ = super::ParsedSpans::new(
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
            let _ = super::ParsedSpans::new(
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
}
