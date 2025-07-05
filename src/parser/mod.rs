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

fn token_display(kind: SyntaxKind) -> &'static str {
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

mod token_stream;

mod span_collector;
use span_collector::SpanCollector;

/// Iterate over the token stream and dispatch handlers by [`SyntaxKind`].
///
/// The macro expects a parsing context `ctx` with a `stream` field. It loops
/// until the stream is exhausted, invoking the handler associated with each
/// recognised kind. Handlers must advance the stream to consume the tokens they
/// process. Any unhandled kind is skipped.
///
/// # Examples
///
///
/// ```
/// struct State<'a> {
///     stream: TokenStream<'a>,
/// }
///
/// fn handle_kw(st: &mut State<'_>, _span: Span) {
///     st.stream.advance();
/// }
///
/// let mut st = State { stream: TokenStream::new(&tokens, src) };
/// token_dispatch!(st, {
///     SyntaxKind::K_IMPORT => handle_kw,
/// });
/// ```
macro_rules! token_dispatch {
    ( $ctx:ident, {
        $( $kind:path => $handler:ident ),* $(,)?
    } ) => {{
        while let Some(&(kind, ref span_ref)) = $ctx.stream.peek() {
            let span = span_ref.clone();
            match kind {
                $( $kind => $handler(&mut $ctx, span.clone()), )*
                _ => $ctx.stream.advance(),
            }
        }
    }};
}

/// Parser recognising whitespace and comment tokens.
///
/// The combinator is useful for padding other parsers where whitespace is
/// allowed. It matches [`SyntaxKind::T_WHITESPACE`] and
/// [`SyntaxKind::T_COMMENT`] tokens and discards them.
///
/// # Examples
///
/// ```no_run
/// use ddlint::parser::inline_ws;
/// let parser = inline_ws().repeated();
/// ```
fn inline_ws() -> impl Parser<SyntaxKind, (), Error = Simple<SyntaxKind>> + Clone {
    filter(|kind: &SyntaxKind| matches!(kind, SyntaxKind::T_WHITESPACE | SyntaxKind::T_COMMENT))
        .ignored()
}

/// Parser for an identifier padded by optional inline whitespace.
///
/// # Examples
///
/// ```no_run
/// use ddlint::parser::ident;
/// let parser = ident();
/// ```
fn ident() -> impl Parser<SyntaxKind, (), Error = Simple<SyntaxKind>> + Clone {
    just(SyntaxKind::T_IDENT)
        .ignored()
        .padded_by(inline_ws().repeated())
}

/// Parser for a rule atom: `Ident` with optional argument list.
///
/// # Examples
///
/// ```no_run
/// use ddlint::parser::atom;
/// let parser = atom();
/// ```
fn atom() -> impl Parser<SyntaxKind, (), Error = Simple<SyntaxKind>> + Clone {
    ident()
        .clone()
        .then(
            just(SyntaxKind::T_LPAREN)
                .padded_by(inline_ws().repeated())
                .ignore_then(
                    filter(|kind: &SyntaxKind| *kind != SyntaxKind::T_RPAREN)
                        .ignored()
                        .padded_by(inline_ws().repeated())
                        .repeated(),
                )
                .then_ignore(just(SyntaxKind::T_RPAREN))
                .or_not(),
        )
        .ignored()
        .padded_by(inline_ws().repeated())
}

/// Parser for a balanced token block such as parentheses or braces.
///
/// The parser consumes the opening delimiter, then all tokens until the
/// matching closing delimiter while tracking nested pairs. Whitespace and
/// comments are permitted between tokens. An error is produced if a closing
/// token appears without a corresponding opener.
fn balanced_block_with_min(
    open: SyntaxKind,
    close: SyntaxKind,
    min: usize,
) -> impl Parser<SyntaxKind, (), Error = Simple<SyntaxKind>> + Clone {
    use std::cell::Cell;

    let depth = Cell::new(0usize);
    just(open)
        .padded_by(inline_ws().repeated())
        .ignore_then(
            filter_map(move |span, kind| match kind {
                k if k == open => {
                    depth.set(depth.get() + 1);
                    Ok(())
                }
                k if k == close => {
                    if depth.get() == 0 {
                        Err(Simple::custom(
                            span,
                            format!("unexpected '{}'", token_display(close)),
                        ))
                    } else {
                        depth.set(depth.get() - 1);
                        Ok(())
                    }
                }
                _ => Ok(()),
            })
            .padded_by(inline_ws().repeated())
            .repeated()
            .at_least(min),
        )
        .then_ignore(just(close))
        .ignored()
}

fn balanced_block(
    open: SyntaxKind,
    close: SyntaxKind,
) -> impl Parser<SyntaxKind, (), Error = Simple<SyntaxKind>> + Clone {
    balanced_block_with_min(open, close, 0)
}

/// As [`balanced_block`] but requires at least one token inside the delimiters.
fn balanced_block_nonempty(
    open: SyntaxKind,
    close: SyntaxKind,
) -> impl Parser<SyntaxKind, (), Error = Simple<SyntaxKind>> + Clone {
    balanced_block_with_min(open, close, 1)
}

/// Result of a parse operation.
#[derive(Debug)]
pub struct Parsed {
    green: GreenNode,
    root: ast::Root,
    errors: Vec<Simple<SyntaxKind>>,
}

/// Spans for each parsed statement category.
#[derive(Debug, Default, Clone, PartialEq)]
pub struct ParsedSpans {
    /// `import` statement spans.
    pub imports: Vec<Span>,
    /// `typedef` statement spans.
    pub typedefs: Vec<Span>,
    /// `relation` declaration spans.
    pub relations: Vec<Span>,
    /// `index` declaration spans.
    pub indexes: Vec<Span>,
    /// `function` definition spans.
    pub functions: Vec<Span>,
    /// Rule spans.
    pub rules: Vec<Span>,
}

impl ParsedSpans {
    /// Assert that every span list is sorted and non-overlapping.
    fn assert_sorted(&self) {
        ensure_span_lists_sorted(&[
            ("imports", &self.imports),
            ("typedefs", &self.typedefs),
            ("relations", &self.relations),
            ("indexes", &self.indexes),
            ("functions", &self.functions),
            ("rules", &self.rules),
        ]);
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
#[must_use]
pub fn parse(src: &str) -> Parsed {
    let tokens = tokenize(src);
    let (spans, errors) = parse_tokens(&tokens, src);

    let green = build_green_tree(tokens, src, &spans);
    let root = ast::Root::from_green(green.clone());

    Parsed {
        green,
        root,
        errors,
    }
}

/// Identifies and collects the spans of `import`, `typedef`, `relation`,
/// `index`, and `function` statements in a token stream.
///
/// Returns a [`ParsedSpans`] struct containing the spans of each statement type
/// alongside any parse errors encountered during span collection.
///
/// # Examples
///
/// ```no_run
/// let (spans, errors) = parse_tokens(&tokens, src);
/// assert!(spans.imports.iter().all(|span| span.start < span.end));
/// ```
fn parse_tokens(
    tokens: &[(SyntaxKind, Span)],
    src: &str,
) -> (ParsedSpans, Vec<Simple<SyntaxKind>>) {
    let (import_spans, errors) = collect_import_spans(tokens, src);
    let typedef_spans = collect_typedef_spans(tokens, src);
    let relation_spans = collect_relation_spans(tokens, src);
    let (index_spans, index_errors) = collect_index_spans(tokens, src);
    let (function_spans, function_errors) = collect_function_spans(tokens, src);
    let (rule_spans, rule_errors) = collect_rule_spans(tokens, src);

    let mut all_errors = errors;
    all_errors.extend(index_errors);
    all_errors.extend(function_errors);
    all_errors.extend(rule_errors);

    (
        ParsedSpans {
            imports: import_spans,
            typedefs: typedef_spans,
            relations: relation_spans,
            indexes: index_spans,
            functions: function_spans,
            rules: rule_spans,
        },
        all_errors,
    )
}

/// Scans the token stream for `import` statements and collects their spans.
///
/// Parses the token stream to identify well-formed `import` statements, recording the
/// corresponding spans. If a malformed `import` statement is encountered, attempts to
/// recover by skipping to the end of the line and records any parse errors encountered
/// during recovery.
///
/// # Returns
///
/// A tuple containing a vector of spans for valid `import` statements and a vector of
/// parse errors for malformed statements.
///
/// # Examples
///
/// ```no_run
/// use parser::{collect_import_spans, SyntaxKind, Span};
///
/// let tokens: Vec<(SyntaxKind, Span)> = /* tokenized source */;
/// let src = "import foo::bar as baz;";
/// let (import_spans, errors) = collect_import_spans(&tokens, src);
/// assert!(!import_spans.is_empty());
/// ```
fn collect_import_spans(
    tokens: &[(SyntaxKind, Span)],
    src: &str,
) -> (Vec<Span>, Vec<Simple<SyntaxKind>>) {
    type State<'a> = SpanCollector<'a, Vec<Simple<SyntaxKind>>>;

    /// Attempts to parse an `import` statement at the given span, recording its span or collecting errors.
    ///
    /// If parsing succeeds, the span of the `import` statement is added to the state's span list and the token stream is advanced past it.
    /// On failure, errors are collected and the stream is advanced to the end of the current line.
    fn handle_import(st: &mut State<'_>, span: Span) {
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

        let imprt = just(SyntaxKind::K_IMPORT)
            .padded_by(ws.clone())
            .ignore_then(module_path)
            .then(alias.or_not())
            .padded_by(ws)
            .map_with_span(|_, sp: Span| sp);

        let (res, err) = st.parse_span(imprt, span.start);
        if let Some(sp) = res {
            let end = sp.end;
            st.spans.push(sp);
            st.stream.skip_until(end);
        } else {
            st.extra.extend(err);
            let end = st.stream.line_end(st.stream.cursor());
            st.stream.skip_until(end);
        }
    }

    let mut st = State::new(tokens, src, Vec::new());

    token_dispatch!(st, {
        SyntaxKind::K_IMPORT => handle_import,
    });

    st.into_parts()
}

/// Collects the spans of `typedef` and `extern type` declarations in the token stream.
///
/// Each span covers the entire line of the declaration, enabling grouping of tokens into
/// `N_TYPE_DEF` nodes during CST construction. Only `extern type` declarations are recognised
/// for `extern` statements; other forms are skipped.
///
/// # Returns
///
/// A vector of spans, each representing a `typedef` or `extern type` declaration.
///
/// # Examples
///
/// ```no_run
/// let tokens = tokenize("typedef Foo = Bar;\nextern type Baz;\n", None);
/// let spans = collect_typedef_spans(&tokens, "typedef Foo = Bar;\nextern type Baz;\n");
/// assert_eq!(spans.len(), 2);
/// ```
fn collect_typedef_spans(tokens: &[(SyntaxKind, Span)], src: &str) -> Vec<Span> {
    type State<'a> = SpanCollector<'a, ()>;

    /// Handles a `typedef` token by advancing the token stream to the end of the line and recording the span.
    ///
    /// Records the span from the start of the `typedef` token to the end of the line in the state's span list.
    ///
    /// # Examples
    ///
    /// ```no_run
    /// // Given a State positioned at a typedef token:
    /// handle_typedef(&mut state, typedef_span);
    /// // The span from the typedef to the line end is recorded in state.spans.
    /// ```
    fn handle_typedef(st: &mut State<'_>, span: Span) {
        let start = span.start;
        st.stream.advance();
        st.push_line_span(start);
    }

    /// Handles an `extern` declaration, collecting the span if it is an `extern type` statement.
    ///
    /// Advances the token stream past the `extern` keyword and any inline whitespace. If the next
    /// token is `type`, advances past it and collects the span up to the end of the line. Otherwise,
    /// skips the remainder of the line without collecting a span.
    ///
    /// # Examples
    ///
    /// ```no_run
    /// // Used internally during typedef span collection:
    /// handle_extern(&mut state, span);
    /// ```
    fn handle_extern(st: &mut State<'_>, span: Span) {
        let start = span.start;
        st.stream.advance();
        st.stream.skip_ws_inline();
        if st
            .stream
            .peek()
            .is_some_and(|(kind, _)| *kind == SyntaxKind::K_TYPE)
        {
            st.stream.advance();
            st.push_line_span(start);
        } else {
            st.skip_line();
        }
    }

    let mut st = State::new(tokens, src, ());

    token_dispatch!(st, {
        SyntaxKind::K_TYPEDEF => handle_typedef,
        SyntaxKind::K_EXTERN => handle_extern,
    });

    st.spans
}

/// Collects the spans of relation declarations in the token stream.
///
/// A relation declaration may start with the optional keywords `input` or
/// `output` followed by `relation`. The span extends to the end of the line
/// containing the declaration. No validation of the declaration contents is
/// performed at this stage.
fn collect_relation_spans(tokens: &[(SyntaxKind, Span)], src: &str) -> Vec<Span> {
    type State<'a> = SpanCollector<'a, &'a str>;

    fn skip_relation_columns(st: &mut State<'_>) {
        st.stream.skip_ws_inline();
        if matches!(st.stream.peek().map(|t| t.0), Some(SyntaxKind::T_IDENT)) {
            st.stream.advance();
        }
        st.stream.skip_ws_inline();
        if matches!(st.stream.peek().map(|t| t.0), Some(SyntaxKind::T_LPAREN)) {
            st.stream.advance();
            let mut depth = 1usize;
            while let Some((kind, _)) = st.stream.peek() {
                match kind {
                    SyntaxKind::T_LPAREN => depth += 1,
                    SyntaxKind::T_RPAREN => {
                        depth -= 1;
                        if depth == 0 {
                            st.stream.advance();
                            break;
                        }
                    }
                    _ => {}
                }
                st.stream.advance();
            }
        }
    }

    fn skip_primary_key_clause(st: &mut State<'_>) {
        st.stream.skip_ws_inline();
        if let Some((SyntaxKind::T_IDENT, span)) = st.stream.peek().cloned()
            && st.extra.get(span.clone()) == Some("primary")
        {
            st.stream.advance();
            st.stream.skip_ws_inline();
            if let Some((SyntaxKind::T_IDENT, sp)) = st.stream.peek().cloned()
                && st.extra.get(sp.clone()) == Some("key")
            {
                st.stream.advance();
                st.stream.skip_ws_inline();
                if matches!(st.stream.peek().map(|t| t.0), Some(SyntaxKind::T_LPAREN)) {
                    st.stream.advance();
                    let mut depth = 1usize;
                    while let Some((kind, _)) = st.stream.peek() {
                        match kind {
                            SyntaxKind::T_LPAREN => depth += 1,
                            SyntaxKind::T_RPAREN => {
                                depth -= 1;
                                if depth == 0 {
                                    st.stream.advance();
                                    break;
                                }
                            }
                            _ => {}
                        }
                        st.stream.advance();
                    }
                }
            }
        }
    }

    fn record_relation(st: &mut State<'_>, start: usize) {
        // Consume columns and optional primary key clause so multi-line
        // declarations are captured fully.
        skip_relation_columns(st);
        skip_primary_key_clause(st);

        let end = st.stream.line_end(st.stream.cursor());
        st.stream.skip_until(end);
        st.spans.push(start..end);
    }

    fn handle_input(st: &mut State<'_>, span: Span) {
        let start = span.start;
        st.stream.advance();
        st.stream.skip_ws_inline();
        if st
            .stream
            .peek()
            .is_some_and(|(kind, _)| *kind == SyntaxKind::K_RELATION)
        {
            st.stream.advance();
            record_relation(st, start);
        } else {
            let end = st.stream.line_end(st.stream.cursor());
            st.stream.skip_until(end);
        }
    }

    /// Output relations follow the same pattern as input relations.
    fn handle_output(st: &mut State<'_>, span: Span) {
        handle_input(st, span);
    }

    fn handle_relation(st: &mut State<'_>, span: Span) {
        let start = span.start;
        st.stream.advance();
        record_relation(st, start);
    }

    let mut st = State::new(tokens, src, src);

    token_dispatch!(st, {
        SyntaxKind::K_INPUT => handle_input,
        SyntaxKind::K_OUTPUT => handle_output,
        SyntaxKind::K_RELATION => handle_relation,
    });

    st.spans
}

/// Collects the spans of index declarations in the token stream.
///
/// The parser recognises the pattern `index <name> on <relation>(<cols>)` and
/// records the span covering the entire statement. Any syntax errors are
/// collected for later reporting and the cursor skips to the end of the line.
fn collect_index_spans(
    tokens: &[(SyntaxKind, Span)],
    src: &str,
) -> (Vec<Span>, Vec<Simple<SyntaxKind>>) {
    type State<'a> = SpanCollector<'a, Vec<Simple<SyntaxKind>>>;

    /// Parser for the column list of an index declaration.
    ///
    /// The parser consumes tokens between the parentheses, tracking nested
    /// parentheses so expressions like `lower(name)` parse correctly. It returns
    /// an error when encountering a closing parenthesis that balances the outer
    /// one, signalling the end of the column list.
    fn index_columns() -> impl Parser<SyntaxKind, (), Error = Simple<SyntaxKind>> {
        balanced_block_nonempty(SyntaxKind::T_LPAREN, SyntaxKind::T_RPAREN)
    }

    /// Parser for an entire index declaration.
    /// Returns the span of the declaration if parsing succeeds.
    fn index_decl() -> impl Parser<SyntaxKind, Span, Error = Simple<SyntaxKind>> {
        let ident = ident();

        let columns = index_columns();

        just(SyntaxKind::K_INDEX)
            .padded_by(inline_ws().repeated())
            .ignore_then(ident.clone())
            .then_ignore(just(SyntaxKind::K_ON).padded_by(inline_ws().repeated()))
            .then(ident)
            .then(columns)
            .padded_by(inline_ws().repeated())
            .map_with_span(|_, sp: Span| sp)
    }

    fn handle_index(st: &mut State<'_>, span: Span) {
        let idx = index_decl();

        let (res, err) = st.parse_span(idx, span.start);
        if let Some(sp) = res {
            let end = sp.end;
            st.spans.push(sp);
            st.stream.skip_until(end);
        } else {
            st.extra.extend(err);
            let end = st.stream.line_end(st.stream.cursor());
            st.stream.skip_until(end);
        }
    }

    let mut st = State::new(tokens, src, Vec::new());

    token_dispatch!(st, {
        SyntaxKind::K_INDEX => handle_index,
    });

    st.into_parts()
}

/// Collects the spans of function declarations in the token stream.
///
/// The function supports both `extern function` declarations without a body and
/// regular `function` definitions enclosed in braces. The span covers the
/// entire declaration or definition.
fn collect_function_spans(
    tokens: &[(SyntaxKind, Span)],
    src: &str,
) -> (Vec<Span>, Vec<Simple<SyntaxKind>>) {
    type State<'a> = SpanCollector<'a, Vec<Simple<SyntaxKind>>>;
    use chumsky::prelude::*;

    fn params() -> impl Parser<SyntaxKind, (), Error = Simple<SyntaxKind>> {
        balanced_block(SyntaxKind::T_LPAREN, SyntaxKind::T_RPAREN)
    }

    fn return_ty() -> impl Parser<SyntaxKind, (), Error = Simple<SyntaxKind>> {
        just(SyntaxKind::T_COLON)
            .padded_by(inline_ws().repeated())
            .ignore_then(
                filter(|kind: &SyntaxKind| {
                    !matches!(kind, SyntaxKind::T_LBRACE | SyntaxKind::T_SEMI)
                })
                .ignored()
                .padded_by(inline_ws().repeated())
                .repeated(),
            )
            .ignored()
            .or_not()
            .ignored()
    }

    fn body_block() -> impl Parser<SyntaxKind, (), Error = Simple<SyntaxKind>> {
        balanced_block(SyntaxKind::T_LBRACE, SyntaxKind::T_RBRACE)
    }

    fn body_optional() -> impl Parser<SyntaxKind, (), Error = Simple<SyntaxKind>> {
        body_block().or_not().ignored()
    }
    fn func_decl(with_body: bool) -> BoxedParser<'static, SyntaxKind, Span, Simple<SyntaxKind>> {
        let ident = ident();
        let parser = just(SyntaxKind::K_FUNCTION)
            .padded_by(inline_ws().repeated())
            .ignore_then(ident.clone())
            .then(params())
            .then(return_ty())
            .then(if with_body {
                body_block().boxed()
            } else {
                body_optional().boxed()
            })
            .padded_by(inline_ws().repeated())
            .map_with_span(|_, sp: Span| sp);
        parser.boxed()
    }

    fn handle_func(st: &mut State<'_>, span: Span, is_extern: bool) {
        if is_extern
            && !matches!(
                st.stream.peek_after_ws_inline().map(|t| t.0),
                Some(SyntaxKind::K_FUNCTION)
            )
        {
            st.skip_line();
            return;
        }

        let parser = if is_extern {
            just(SyntaxKind::K_EXTERN)
                .padded_by(inline_ws().repeated())
                .ignore_then(func_decl(false))
                .boxed()
        } else {
            func_decl(true)
        };
        let (res, err) = st.parse_span(parser, span.start);
        if let Some(sp) = res {
            let full = if is_extern {
                span.start..sp.end
            } else {
                sp.clone()
            };
            st.spans.push(full.clone());
            st.stream.skip_until(full.end);
        } else {
            st.extra.extend(err);
            st.skip_line();
        }
    }

    fn handle_extern(st: &mut State<'_>, span: Span) {
        handle_func(st, span, true);
    }

    fn handle_function(st: &mut State<'_>, span: Span) {
        handle_func(st, span, false);
    }

    let mut st = State::new(tokens, src, Vec::new());

    token_dispatch!(st, {
        SyntaxKind::K_EXTERN => handle_extern,
        SyntaxKind::K_FUNCTION => handle_function,
    });

    st.into_parts()
}

/// Collects the spans of rule declarations in the token stream.
///
/// A rule has the form `Head :- Body.` where the body is optional. The parser
/// records the span from the start of the head atom to the terminating dot.
fn collect_rule_spans(
    tokens: &[(SyntaxKind, Span)],
    src: &str,
) -> (Vec<Span>, Vec<Simple<SyntaxKind>>) {
    type State<'a> = SpanCollector<'a, Vec<Simple<SyntaxKind>>>;

    fn rule_decl() -> impl Parser<SyntaxKind, Span, Error = Simple<SyntaxKind>> {
        let ws = inline_ws().repeated();

        let atom_p = atom();

        let literal = atom_p.clone();

        let body = literal
            .clone()
            .separated_by(just(SyntaxKind::T_COMMA).padded_by(ws.clone()))
            .allow_trailing()
            .at_least(1);

        atom_p
            .then(
                just(SyntaxKind::T_IMPLIES)
                    .padded_by(ws.clone())
                    .ignore_then(body)
                    .or_not(),
            )
            .padded_by(ws.clone())
            .then_ignore(just(SyntaxKind::T_DOT))
            .padded_by(ws)
            .map_with_span(|_, sp: Span| sp)
    }

    fn parse_rule_at_line_start(st: &mut State<'_>, span: Span) {
        // Parse a rule only when the current token begins a new line. This avoids
        // misinterpreting identifiers inside other statements as rule heads.
        let prev_end = if st.stream.cursor() == 0 {
            0
        } else {
            st.stream
                .tokens()
                .get(st.stream.cursor() - 1)
                .map_or(0, |t| t.1.end)
        };
        let is_new_line = if st.stream.cursor() == 0 {
            true
        } else {
            st.stream
                .src()
                .get(prev_end..span.start)
                .is_some_and(|text| text.contains('\n'))
        };
        if !is_new_line {
            st.stream.advance();
            return;
        }

        let parser = rule_decl();
        let (res, err) = st.parse_span(parser, span.start);
        if let Some(sp) = res {
            let end = sp.end;
            st.spans.push(sp);
            st.stream.skip_until(end);
        } else {
            st.extra.extend(err);
            let end = st.stream.line_end(st.stream.cursor());
            st.stream.skip_until(end);
        }
    }

    fn handle_ident(st: &mut State<'_>, span: Span) {
        parse_rule_at_line_start(st, span);
    }

    fn handle_implies(st: &mut State<'_>, span: Span) {
        // Treat a leading ':-' as a rule with a missing head.
        parse_rule_at_line_start(st, span);
    }

    let mut st = State::new(tokens, src, Vec::new());

    token_dispatch!(st, {
        SyntaxKind::T_IDENT => handle_ident,
        SyntaxKind::T_IMPLIES => handle_implies,
    });

    st.into_parts()
}

/// Construct the CST from the token stream and recorded statement spans.
///
/// `spans.imports` and `spans.typedefs` must be sorted and non-overlapping so
/// that tokens are wrapped into well-formed nodes during tree construction.
/// Spans are checked with debug assertions.
fn build_green_tree(tokens: Vec<(SyntaxKind, Span)>, src: &str, spans: &ParsedSpans) -> GreenNode {
    spans.assert_sorted();
    let mut builder = GreenNodeBuilder::new();
    builder.start_node(DdlogLanguage::kind_to_raw(SyntaxKind::N_DATALOG_PROGRAM));

    let mut import_iter = spans.imports.iter().peekable();
    let mut typedef_iter = spans.typedefs.iter().peekable();
    let mut relation_iter = spans.relations.iter().peekable();
    let mut index_iter = spans.indexes.iter().peekable();
    let mut function_iter = spans.functions.iter().peekable();
    let mut rule_iter = spans.rules.iter().peekable();

    for (kind, span) in tokens {
        advance_span_iter(&mut import_iter, span.start);
        advance_span_iter(&mut typedef_iter, span.start);
        advance_span_iter(&mut relation_iter, span.start);
        advance_span_iter(&mut index_iter, span.start);
        advance_span_iter(&mut function_iter, span.start);
        advance_span_iter(&mut rule_iter, span.start);

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
        maybe_start(
            &mut builder,
            &mut relation_iter,
            span.start,
            SyntaxKind::N_RELATION_DECL,
        );
        maybe_start(
            &mut builder,
            &mut index_iter,
            span.start,
            SyntaxKind::N_INDEX,
        );
        maybe_start(
            &mut builder,
            &mut function_iter,
            span.start,
            SyntaxKind::N_FUNCTION,
        );
        maybe_start(&mut builder, &mut rule_iter, span.start, SyntaxKind::N_RULE);

        push_token(&mut builder, kind, &span, src);

        maybe_finish(&mut builder, &mut import_iter, span.end);
        maybe_finish(&mut builder, &mut typedef_iter, span.end);
        maybe_finish(&mut builder, &mut relation_iter, span.end);
        maybe_finish(&mut builder, &mut index_iter, span.end);
        maybe_finish(&mut builder, &mut function_iter, span.end);
        maybe_finish(&mut builder, &mut rule_iter, span.end);
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

        /// Collect all relation declarations under this root.
        #[must_use]
        pub fn relations(&self) -> Vec<Relation> {
            self.syntax
                .children()
                .filter(|n| n.kind() == SyntaxKind::N_RELATION_DECL)
                .map(|syntax| Relation { syntax })
                .collect()
        }

        /// Collect all index declarations under this root.
        #[must_use]
        pub fn indexes(&self) -> Vec<Index> {
            self.syntax
                .children()
                .filter(|n| n.kind() == SyntaxKind::N_INDEX)
                .map(|syntax| Index { syntax })
                .collect()
        }

        /// Collect all function declarations under this root.
        #[must_use]
        pub fn functions(&self) -> Vec<Function> {
            self.syntax
                .children()
                .filter(|n| n.kind() == SyntaxKind::N_FUNCTION)
                .map(|syntax| Function { syntax })
                .collect()
        }

        /// Collect all rule declarations under this root.
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
            let mut iter = self.syntax.children_with_tokens().peekable();
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

    mod parse_utils;
    use parse_utils::{parse_name_type_pairs, parse_type_after_colon};

    /// Typed wrapper for a relation declaration.
    #[derive(Debug, Clone)]
    pub struct Relation {
        pub(crate) syntax: SyntaxNode<DdlogLanguage>,
    }

    impl Relation {
        /// Access the underlying syntax node.
        #[must_use]
        pub fn syntax(&self) -> &SyntaxNode<DdlogLanguage> {
            &self.syntax
        }

        /// Name of the relation if present.
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
        #[must_use]
        pub fn is_input(&self) -> bool {
            self.syntax
                .children_with_tokens()
                .any(|e| e.kind() == SyntaxKind::K_INPUT)
        }

        /// Whether the relation is declared as `output`.
        #[must_use]
        pub fn is_output(&self) -> bool {
            self.syntax
                .children_with_tokens()
                .any(|e| e.kind() == SyntaxKind::K_OUTPUT)
        }

        /// Columns declared for the relation.
        ///
        /// Delimiter errors detected during parsing are ignored.
        /// This may change in future to surface these diagnostics.
        #[must_use]
        pub fn columns(&self) -> Vec<(String, String)> {
            let (pairs, errors) = parse_name_type_pairs(self.syntax.children_with_tokens());
            // Delimiter errors are ignored for now. Future versions may surface them.
            let _ = errors;
            pairs
        }

        /// Primary key column names if specified.
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

    /// Typed wrapper for an index declaration.
    #[derive(Debug, Clone)]
    pub struct Index {
        pub(crate) syntax: SyntaxNode<DdlogLanguage>,
    }

    impl Index {
        /// Access the underlying syntax node.
        #[must_use]
        pub fn syntax(&self) -> &SyntaxNode<DdlogLanguage> {
            &self.syntax
        }

        /// Name of the index if present.
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

    /// Typed wrapper for a rule declaration.
    #[derive(Debug, Clone)]
    pub struct Rule {
        pub(crate) syntax: SyntaxNode<DdlogLanguage>,
    }

    impl Rule {
        /// Access the underlying syntax node.
        #[must_use]
        pub fn syntax(&self) -> &SyntaxNode<DdlogLanguage> {
            &self.syntax
        }

        /// Text of the rule head atom.
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

    /// Typed wrapper for a function declaration or definition.
    #[derive(Debug, Clone)]
    pub struct Function {
        pub(crate) syntax: SyntaxNode<DdlogLanguage>,
    }

    impl Function {
        /// Access the underlying syntax node.
        #[must_use]
        pub fn syntax(&self) -> &SyntaxNode<DdlogLanguage> {
            &self.syntax
        }

        /// Name of the function if present.
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

        /// Whether the function is declared as `extern`.
        #[must_use]
        pub fn is_extern(&self) -> bool {
            self.syntax
                .children_with_tokens()
                .any(|e| e.kind() == SyntaxKind::K_EXTERN)
        }

        /// Function parameters as name/type pairs.
        ///
        /// Delimiter errors detected during parsing are ignored.
        /// This may change in future to surface these diagnostics.
        #[must_use]
        pub fn parameters(&self) -> Vec<(String, String)> {
            let (pairs, errors) = parse_name_type_pairs(self.syntax.children_with_tokens());
            // Delimiter errors are ignored for now. Future versions may surface them.
            let _ = errors;
            pairs
        }

        /// Return type text if specified.
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
        let src = "import Foo";
        let tokens = tokenize(src);
        let unsorted = vec![1..2, 0..1];
        let spans = super::ParsedSpans {
            imports: unsorted,
            ..Default::default()
        };
        let result = std::panic::catch_unwind(|| {
            super::build_green_tree(tokens, src, &spans);
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
        let src = "import Foo; type T = string";
        let tokens = tokenize(src);
        let imports = vec![1..2, 0..1];
        let typedefs = vec![4..5, 3..4];
        let spans = super::ParsedSpans {
            imports,
            typedefs,
            ..Default::default()
        };
        let result = std::panic::catch_unwind(|| {
            super::build_green_tree(tokens, src, &spans);
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
