//! Chumsky-based parser producing a rowan CST.
//!
//! This module contains the entry point for parsing `DDlog` source code.
//! The grammar rules are not implemented yet. The parser currently tokenises
//! the input and wraps the tokens into a `rowan::GreenNode`. It lays down the
//! framework for integrating `chumsky` combinators and error recovery in later
//! stages.

use chumsky::Stream;
use chumsky::prelude::*;
use log::warn;
use rowan::{GreenNode, GreenNodeBuilder, Language};

use crate::{DdlogLanguage, Span, SyntaxKind, tokenize};

/// Iterate over tokens and dispatch to a handler based on the token kind.
///
/// The macro loops until the token slice is exhausted, invoking the matching
/// handler for each recognised `SyntaxKind`. Any token kinds not provided in the
/// pattern cause the state's index to advance with no other action.
/// Iterate over tokens and call a handler for each recognised kind.
///
/// The macro expects a state object that contains a `cursor` field tracking the
/// current position in the token slice. Handlers mutate the state to consume
/// tokens. Any unhandled kind simply advances the cursor by one.
///
/// # Examples
///
/// ```
/// dispatch_tokens!(st, tokens, {
///     SyntaxKind::K_IMPORT => handle_import,
///     SyntaxKind::K_TYPEDEF => handle_typedef,
/// });
/// ```
macro_rules! dispatch_tokens {
    ( $state:ident, $token_list:ident, {
        $( $kind:path => $handler:ident ),* $(,)?
    } ) => {{
        while let Some((kind, span)) = $token_list.get($state.cursor).cloned() {
            match kind {
                $( $kind => $handler(&mut $state, span), )*
                _ => $state.cursor += 1,
            }
        }
    }};
}

/// Result of a parse operation.
#[derive(Debug)]
pub struct Parsed {
    green: GreenNode,
    root: ast::Root,
    errors: Vec<Simple<SyntaxKind>>,
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
#[must_use]
pub fn parse(src: &str) -> Parsed {
    let tokens = tokenize(src);
    let (import_spans, typedef_spans, errors) = parse_tokens(&tokens, src);

    let green = build_green_tree(tokens, src, &import_spans, &typedef_spans);
    let root = ast::Root::from_green(green.clone());

    Parsed {
        green,
        root,
        errors,
    }
}

fn parse_tokens(
    tokens: &[(SyntaxKind, Span)],
    src: &str,
) -> (Vec<Span>, Vec<Span>, Vec<Simple<SyntaxKind>>) {
    let (import_spans, errors) = collect_import_spans(tokens, src);
    let typedef_spans = collect_typedef_spans(tokens, src);

    (import_spans, typedef_spans, errors)
}

/// Advance `offset` past tokens whose span ends before or at `end`.
///
/// This helper is used when a parser has consumed a known span and the token
/// iterator should skip ahead to resume scanning from the end of that span.
fn skip_tokens_until(offset: &mut usize, tokens: &[(SyntaxKind, Span)], end: usize) {
    while let Some(span) = tokens.get(*offset).map(|t| &t.1) {
        if span.end <= end {
            *offset += 1;
        } else {
            break;
        }
    }
}

/// Return the position one past the newline after `start` or the source length.
///
/// The `start` index refers to a position within `tokens`. If it lies beyond the
/// token slice the function returns `src.len()`.
fn line_end(tokens: &[(SyntaxKind, Span)], src: &str, start: usize) -> usize {
    let mut end = tokens.get(start).map_or(src.len(), |t| t.1.end);
    for tok in tokens.iter().skip(start) {
        end = tok.1.end;
        let text = src.get(tok.1.clone()).unwrap_or("");
        if text.contains('\n') {
            break;
        }
    }
    end
}

/// Skip whitespace and comments that do not contain newlines.
///
/// Used when parsing single-line constructs to ignore inline whitespace without
/// crossing line boundaries.
fn skip_ws_no_newline(tokens: &[(SyntaxKind, Span)], src: &str, index: &mut usize) {
    while let Some(tok) = tokens.get(*index) {
        if matches!(tok.0, SyntaxKind::T_WHITESPACE | SyntaxKind::T_COMMENT)
            && !src.get(tok.1.clone()).unwrap_or("").contains('\n')
        {
            *index += 1;
            continue;
        }
        break;
    }
}

/// Scan the token stream for `import` statements and record their spans.
///
/// Returns the list of spans and any parse errors encountered while
/// recovering from malformed import statements.
fn collect_import_spans(
    tokens: &[(SyntaxKind, Span)],
    src: &str,
) -> (Vec<Span>, Vec<Simple<SyntaxKind>>) {
    struct State<'a> {
        cursor: usize,
        spans: Vec<Span>,
        errors: Vec<Simple<SyntaxKind>>,
        tokens: &'a [(SyntaxKind, Span)],
        src: &'a str,
    }

    fn handle_import(st: &mut State<'_>, span: Span) {
        let ws = filter(|kind: &SyntaxKind| {
            matches!(kind, SyntaxKind::T_WHITESPACE | SyntaxKind::T_COMMENT)
        })
        .ignored();

        let ident = just(SyntaxKind::T_IDENT).ignored().padded_by(ws.repeated());

        let module_path = ident
            .then(
                just(SyntaxKind::T_COLON_COLON)
                    .padded_by(ws.repeated())
                    .ignore_then(ident)
                    .repeated(),
            )
            .ignored();

        let alias = just(SyntaxKind::K_AS)
            .padded_by(ws.repeated())
            .ignore_then(ident);

        let imprt = just(SyntaxKind::K_IMPORT)
            .padded_by(ws.repeated())
            .ignore_then(module_path)
            .then(alias.or_not())
            .padded_by(ws.repeated())
            .map_with_span(|_, sp: Span| sp);

        let iter = st.tokens.iter().skip(st.cursor).cloned();
        let sub_stream = Stream::from_iter(span.start..st.src.len(), iter);
        let (res, err) = imprt.parse_recovery(sub_stream);
        if let Some(sp) = res {
            let end = sp.end;
            st.spans.push(sp);
            skip_tokens_until(&mut st.cursor, st.tokens, end);
        } else {
            st.errors.extend(err);
            let end = line_end(st.tokens, st.src, st.cursor);
            skip_tokens_until(&mut st.cursor, st.tokens, end);
        }
    }

    let mut st = State {
        cursor: 0,
        spans: Vec::new(),
        errors: Vec::new(),
        tokens,
        src,
    };

    dispatch_tokens!(st, tokens, {
        SyntaxKind::K_IMPORT => handle_import,
    });

    (st.spans, st.errors)
}

/// Collect the spans of `typedef` and `extern type` declarations.
///
/// Spans cover the full declaration line so tokens can be grouped into
/// `N_TYPE_DEF` nodes later when building the CST.
fn collect_typedef_spans(tokens: &[(SyntaxKind, Span)], src: &str) -> Vec<Span> {
    struct State<'a> {
        cursor: usize,
        spans: Vec<Span>,
        tokens: &'a [(SyntaxKind, Span)],
        src: &'a str,
    }

    fn handle_typedef(st: &mut State<'_>, span: Span) {
        let start = span.start;
        st.cursor += 1;
        let end = line_end(st.tokens, st.src, st.cursor);
        skip_tokens_until(&mut st.cursor, st.tokens, end);
        st.spans.push(start..end);
    }

    fn handle_extern(st: &mut State<'_>, span: Span) {
        let start = span.start;
        st.cursor += 1;
        skip_ws_no_newline(st.tokens, st.src, &mut st.cursor);
        if let Some((SyntaxKind::K_TYPE, _)) = st.tokens.get(st.cursor).cloned() {
            st.cursor += 1;
            let end = line_end(st.tokens, st.src, st.cursor);
            skip_tokens_until(&mut st.cursor, st.tokens, end);
            st.spans.push(start..end);
        } else {
            // Currently only `extern type` is recognised. Skip the remainder
            // of this line so parsing can continue.
            let end = line_end(st.tokens, st.src, st.cursor);
            skip_tokens_until(&mut st.cursor, st.tokens, end);
        }
    }

    let mut st = State {
        cursor: 0,
        spans: Vec::new(),
        tokens,
        src,
    };

    dispatch_tokens!(st, tokens, {
        SyntaxKind::K_TYPEDEF => handle_typedef,
        SyntaxKind::K_EXTERN => handle_extern,
    });

    st.spans
}

/// Construct the CST from the token stream and recorded statement spans.
///
/// `imports` and `typedefs` must be sorted and non-overlapping so that tokens
/// are wrapped into well-formed nodes during tree construction.
/// Spans are checked with debug assertions.
fn build_green_tree(
    tokens: Vec<(SyntaxKind, Span)>,
    src: &str,
    imports: &[Span],
    typedefs: &[Span],
) -> GreenNode {
    assert_spans_sorted(imports);
    assert_spans_sorted(typedefs);
    let mut builder = GreenNodeBuilder::new();
    builder.start_node(DdlogLanguage::kind_to_raw(SyntaxKind::N_DATALOG_PROGRAM));

    let mut import_iter = imports.iter().peekable();
    let mut typedef_iter = typedefs.iter().peekable();

    for (kind, span) in tokens {
        advance_span_iter(&mut import_iter, span.start);
        advance_span_iter(&mut typedef_iter, span.start);

        maybe_start(
            &mut builder,
            &mut import_iter,
            span.start,
            SyntaxKind::N_IMPORT_STMT,
        );
        maybe_start(
            &mut builder,
            &mut typedef_iter,
            span.start,
            SyntaxKind::N_TYPE_DEF,
        );

        push_token(&mut builder, kind, &span, src);

        maybe_finish(&mut builder, &mut import_iter, span.end);
        maybe_finish(&mut builder, &mut typedef_iter, span.end);
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

/// Assert that spans are sorted and non-overlapping.
fn assert_spans_sorted(spans: &[Span]) {
    for pair in spans.windows(2) {
        let [first, second] = pair else { continue };
        debug_assert!(first.end <= second.start, "spans overlap or are unsorted");
    }
}

/// Push a token to the tree, wrapping `N_ERROR` tokens in an error node.
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

pub mod ast {
    //! Minimal typed AST wrappers used by the parser.
    //!
    //! This layer will expand as grammar rules are implemented. For now it
    //! exposes only the root node so tests and higher layers can navigate the
    //! parsed CST.

    use rowan::{GreenNode, SyntaxNode};

    use crate::{DdlogLanguage, SyntaxKind};

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
        #[must_use]
        pub fn imports(&self) -> Vec<Import> {
            self.syntax
                .children()
                .filter(|n| n.kind() == SyntaxKind::N_IMPORT_STMT)
                .map(|syntax| Import { syntax })
                .collect()
        }

        /// Collect all `typedef` declarations under this root.
        #[must_use]
        pub fn type_defs(&self) -> Vec<TypeDef> {
            self.syntax
                .children()
                .filter(|n| n.kind() == SyntaxKind::N_TYPE_DEF)
                .map(|syntax| TypeDef { syntax })
                .collect()
        }
    }

    /// Typed wrapper for an `import` statement.
    #[derive(Debug, Clone)]
    pub struct Import {
        pub(crate) syntax: SyntaxNode<DdlogLanguage>,
    }

    impl Import {
        /// Access the underlying syntax node.
        #[must_use]
        pub fn syntax(&self) -> &SyntaxNode<DdlogLanguage> {
            &self.syntax
        }

        /// The module path text as written in the source.
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

    /// Typed wrapper for a `typedef` or `extern type` declaration.
    #[derive(Debug, Clone)]
    pub struct TypeDef {
        pub(crate) syntax: SyntaxNode<DdlogLanguage>,
    }

    impl TypeDef {
        /// Access the underlying syntax node.
        #[must_use]
        pub fn syntax(&self) -> &SyntaxNode<DdlogLanguage> {
            &self.syntax
        }

        /// Name of the defined type.
        #[must_use]
        pub fn name(&self) -> Option<String> {
            let mut iter = self.syntax.children_with_tokens();
            if !skip_to_typedef_keyword(&mut iter) {
                return None;
            }
            take_first_ident(iter)
        }

        /// Whether this declaration is `extern`.
        #[must_use]
        pub fn is_extern(&self) -> bool {
            self.syntax
                .children_with_tokens()
                .any(|e| e.kind() == SyntaxKind::K_EXTERN)
        }
    }

    /// Advance the iterator until `typedef` or `type` is encountered.
    fn skip_to_typedef_keyword(
        iter: &mut impl Iterator<Item = rowan::SyntaxElement<DdlogLanguage>>,
    ) -> bool {
        for e in iter.by_ref() {
            let kind = e.kind();
            if kind == SyntaxKind::K_EXTERN {
                continue;
            }
            if matches!(kind, SyntaxKind::K_TYPEDEF | SyntaxKind::K_TYPE) {
                return true;
            }
        }
        false
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
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tokenize;
    use rstest::rstest;

    #[rstest]
    fn skip_tokens_until_advances_past_span() {
        let src = "import foo\n";
        let tokens = tokenize(src);
        let mut idx = 0;
        let end = line_end(&tokens, src, 0);
        skip_tokens_until(&mut idx, &tokens, end);
        assert_eq!(idx, tokens.len());
    }

    #[rstest]
    fn line_end_returns_span_end() {
        let src = "typedef A = string\nnext";
        let tokens = tokenize(src);
        let start = 1; // token after 'typedef'
        let end = line_end(&tokens, src, start);
        let newline = src.find('\n').unwrap_or_else(|| panic!("newline missing"));
        assert_eq!(end, newline + 1);
    }

    #[rstest]
    fn skip_ws_no_newline_skips_spaces() {
        let src = "extern    type Foo";
        let tokens = tokenize(src);
        let mut idx = 1; // after 'extern'
        skip_ws_no_newline(&tokens, src, &mut idx);
        assert!(matches!(
            tokens.get(idx).map(|t| t.0),
            Some(SyntaxKind::K_TYPE)
        ));
    }

    #[rstest]
    fn line_end_out_of_bounds_returns_len() {
        let src = "typedef A = string\n";
        let tokens = tokenize(src);
        let start = tokens.len();
        assert_eq!(line_end(&tokens, src, start), src.len());
    }
}
