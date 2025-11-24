//! Utilities for scanning top-level statement spans.
//!
//! This module walks a token stream to identify top-level statement spans.
//! It groups the ranges for imports, typedefs, relations, indexes,
//! functions, transformers and rules. The spans are later used to build the
//! CST without needing full parsing.

use chumsky::prelude::*;

use crate::parser::expression_span::{rule_body_literal_spans, validate_expression};
use crate::{Span, SyntaxKind};

use super::{
    ParsedSpans,
    lexer_helpers::{atom, balanced_block, balanced_block_nonempty, ident, inline_ws},
    span_collector::SpanCollector,
};
use crate::parser::ast::parse_utils::{primary_key_clause, relation_columns};

/// Scan the token stream and collect spans for each statement category.
pub(super) fn parse_tokens(
    tokens: &[(SyntaxKind, Span)],
    src: &str,
) -> (ParsedSpans, Vec<Simple<SyntaxKind>>) {
    let (import_spans, errors) = collect_import_spans(tokens, src);
    let typedef_spans = collect_typedef_spans(tokens, src);
    let (relation_spans, relation_errors) = collect_relation_spans(tokens, src);
    let (index_spans, index_errors) = collect_index_spans(tokens, src);
    let (function_spans, function_errors) = collect_function_spans(tokens, src);
    let (transformer_spans, transformer_errors) = collect_transformer_spans(tokens, src);

    let non_rule_span_capacity = import_spans.len()
        + typedef_spans.len()
        + relation_spans.len()
        + index_spans.len()
        + function_spans.len()
        + transformer_spans.len();

    let mut non_rule_spans = Vec::with_capacity(non_rule_span_capacity);
    non_rule_spans.extend(import_spans.iter().cloned());
    non_rule_spans.extend(typedef_spans.iter().cloned());
    non_rule_spans.extend(relation_spans.iter().cloned());
    non_rule_spans.extend(index_spans.iter().cloned());
    non_rule_spans.extend(function_spans.iter().cloned());
    non_rule_spans.extend(transformer_spans.iter().cloned());
    let non_rule_spans = merge_spans(non_rule_spans);

    let (rule_spans, expr_spans, rule_errors) = collect_rule_spans(tokens, src, &non_rule_spans);

    let mut all_errors = errors;
    all_errors.extend(relation_errors);
    all_errors.extend(index_errors);
    all_errors.extend(function_errors);
    all_errors.extend(transformer_errors);
    all_errors.extend(rule_errors);

    (
        ParsedSpans::builder()
            .imports(import_spans)
            .typedefs(typedef_spans)
            .relations(relation_spans)
            .indexes(index_spans)
            .functions(function_spans)
            .transformers(transformer_spans)
            .rules(rule_spans)
            .expressions(expr_spans)
            .build(),
        all_errors,
    )
}

/// Return a sorted, merged copy of the provided spans.
///
/// Adjacent or overlapping spans are coalesced to minimise skip checks during
/// rule scanning.
fn merge_spans(mut spans: Vec<Span>) -> Vec<Span> {
    spans.sort_by_key(|sp| sp.start);
    let mut merged: Vec<Span> = Vec::with_capacity(spans.len());
    for span in spans {
        if let Some(last) = merged.last_mut()
            && span.start <= last.end
        {
            last.end = last.end.max(span.end);
            continue;
        }
        merged.push(span);
    }
    merged
}

/// Parse a statement and record its span on success.
///
/// The `parser` **must** consume the entire statement and return a `Span`
/// covering it, including any trailing inline whitespace. Any parse errors
/// produced by `parser` are appended to `st.extra`. On success the span is
/// recorded and the token stream advanced to the end of the statement;
/// otherwise the current line is skipped.
///
/// # Examples
///
/// ```rust,ignore
/// use chumsky::prelude::*;
/// use crate::{parser::span_scanner::parse_and_record, SyntaxKind, Span};
/// use crate::parser::span_collector::SpanCollector;
///
/// let src = "import foo\n";
/// let tokens = crate::test_util::tokenize(src);
/// let mut st = SpanCollector::new(&tokens, src, Vec::new());
/// let ident = just(SyntaxKind::T_IDENT).map_with_span(|_, sp: Span| sp);
/// let (span, errs) = parse_and_record(&mut st, 0, ident);
/// assert!(span.is_some());
/// assert!(errs.is_empty());
/// assert_eq!(st.extra, errs);
/// ```
fn parse_and_record<P, E>(
    st: &mut SpanCollector<'_, E>,
    start: usize,
    parser: P,
) -> (Option<Span>, Vec<Simple<SyntaxKind>>)
where
    P: Parser<SyntaxKind, Span, Error = Simple<SyntaxKind>>,
    E: Extend<Simple<SyntaxKind>>,
{
    let (res, errs) = st.parse_span(parser, start);
    if let Some(sp) = res.clone() {
        st.stream.skip_until(sp.end);
        st.spans.push(sp);
    } else {
        st.skip_line();
    }
    st.extra.extend(errs.clone());
    (res, errs)
}

fn collect_import_spans(
    tokens: &[(SyntaxKind, Span)],
    src: &str,
) -> (Vec<Span>, Vec<Simple<SyntaxKind>>) {
    type State<'a> = SpanCollector<'a, Vec<Simple<SyntaxKind>>>;

    let mut st = State::new(tokens, src, Vec::new());

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

fn collect_extern_declarations<F, P>(
    tokens: &[(SyntaxKind, Span)],
    src: &str,
    decl_kind: SyntaxKind,
    decl_parser: F,
) -> (Vec<Span>, Vec<Simple<SyntaxKind>>)
where
    F: Fn() -> P,
    P: Parser<SyntaxKind, Span, Error = Simple<SyntaxKind>>,
{
    type State<'a> = SpanCollector<'a, Vec<Simple<SyntaxKind>>>;

    let mut st = State::new(tokens, src, Vec::new());

    let handler = |st: &mut State<'_>, span: Span| {
        let is_decl = st
            .stream
            .peek_after_ws_inline()
            .is_some_and(|(k, _)| *k == decl_kind);
        if !is_decl {
            st.skip_line();
            return;
        }

        let ws = inline_ws().repeated();
        let start = span.start;
        let parser = just(SyntaxKind::K_EXTERN)
            .padded_by(ws.clone())
            .ignore_then(decl_parser())
            .map(move |sp: Span| start..sp.end);

        parse_and_record(st, start, parser);
    };

    token_dispatch!(st, { SyntaxKind::K_EXTERN => handler });
    st.into_parts()
}

fn collect_typedef_spans(tokens: &[(SyntaxKind, Span)], src: &str) -> Vec<Span> {
    type State<'a> = SpanCollector<'a, ()>;

    fn handle_typedef(st: &mut State<'_>, span: Span) {
        let start = span.start;
        st.stream.advance();
        st.push_line_span(start);
    }

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

/// Parse a relation name followed by a non-empty column list.
///
/// # Examples
///
/// Valid input:
///
/// ```rust,ignore
/// use chumsky::Parser as _;
/// let src = "User(id: u32)";
/// let tokens = crate::test_util::tokenize(src);
/// let stream = chumsky::Stream::from_iter(0..src.len(), tokens.into_iter());
/// assert!(relation_columns().parse(stream).is_ok());
/// ```
fn collect_relation_spans(
    tokens: &[(SyntaxKind, Span)],
    src: &str,
) -> (Vec<Span>, Vec<Simple<SyntaxKind>>) {
    #[derive(Debug)]
    struct Extras<'a> {
        src: &'a str,
        errors: Vec<Simple<SyntaxKind>>,
    }

    type State<'a> = SpanCollector<'a, Extras<'a>>;

    fn record_relation(st: &mut State<'_>, start: usize) {
        let parser = relation_columns()
            .then(primary_key_clause(st.extra.src).or_not())
            .map_with_span(|_, sp: Span| sp);

        let (res, errs) = st.parse_span(parser, start);
        st.extra.errors.extend(errs.clone());

        if let Some(sp) = res {
            st.stream.skip_until(sp.end);
            st.stream.skip_ws_inline();
            if errs.is_empty() {
                if let Some((_, span)) = st.stream.peek().cloned()
                    && st.extra.src.get(span.clone()) == Some("primary")
                {
                    st.extra
                        .errors
                        .push(Simple::custom(span, "invalid primary key clause"));
                    st.skip_line();
                } else {
                    let end = st.stream.line_end(st.stream.cursor());
                    st.stream.skip_until(end);
                    st.spans.push(start..end);
                }
            } else {
                st.skip_line();
            }
        } else {
            st.skip_line();
        }
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

    fn handle_output(st: &mut State<'_>, span: Span) {
        handle_input(st, span);
    }

    fn handle_relation(st: &mut State<'_>, span: Span) {
        let start = span.start;
        st.stream.advance();
        record_relation(st, start);
    }

    let mut st = State::new(
        tokens,
        src,
        Extras {
            src,
            errors: Vec::new(),
        },
    );

    token_dispatch!(st, {
        SyntaxKind::K_INPUT => handle_input,
        SyntaxKind::K_OUTPUT => handle_output,
        SyntaxKind::K_RELATION => handle_relation,
    });

    let (spans, extras) = st.into_parts();
    (spans, extras.errors)
}

fn collect_index_spans(
    tokens: &[(SyntaxKind, Span)],
    src: &str,
) -> (Vec<Span>, Vec<Simple<SyntaxKind>>) {
    type State<'a> = SpanCollector<'a, Vec<Simple<SyntaxKind>>>;

    fn index_columns() -> impl Parser<SyntaxKind, (), Error = Simple<SyntaxKind>> {
        balanced_block_nonempty(SyntaxKind::T_LPAREN, SyntaxKind::T_RPAREN)
    }

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

    let mut st = State::new(tokens, src, Vec::new());

    let handle_index = |st: &mut State<'_>, span: Span| {
        let parser = index_decl();
        parse_and_record(st, span.start, parser);
    };

    token_dispatch!(st, {
        SyntaxKind::K_INDEX => handle_index,
    });

    st.into_parts()
}

fn collect_function_spans(
    tokens: &[(SyntaxKind, Span)],
    src: &str,
) -> (Vec<Span>, Vec<Simple<SyntaxKind>>) {
    type State<'a> = SpanCollector<'a, Vec<Simple<SyntaxKind>>>;

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

        let start = span.start;
        let parser = if is_extern {
            let s = start;
            just(SyntaxKind::K_EXTERN)
                .padded_by(inline_ws().repeated())
                .ignore_then(func_decl(false))
                .map(move |sp: Span| s..sp.end)
                .boxed()
        } else {
            func_decl(true)
        };
        parse_and_record(st, start, parser);
    }

    let mut st = State::new(tokens, src, Vec::new());

    let handle_extern = |st: &mut State<'_>, span: Span| {
        handle_func(st, span, true);
    };
    let handle_function = |st: &mut State<'_>, span: Span| {
        handle_func(st, span, false);
    };

    token_dispatch!(st, {
        SyntaxKind::K_EXTERN => handle_extern,
        SyntaxKind::K_FUNCTION => handle_function,
    });

    st.into_parts()
}

fn collect_transformer_spans(
    tokens: &[(SyntaxKind, Span)],
    src: &str,
) -> (Vec<Span>, Vec<Simple<SyntaxKind>>) {
    fn transformer_decl() -> impl Parser<SyntaxKind, Span, Error = Simple<SyntaxKind>> {
        let ws = inline_ws().repeated();
        let ident_p = ident();

        let args = balanced_block(SyntaxKind::T_LPAREN, SyntaxKind::T_RPAREN);
        let outputs = ident_p
            .clone()
            .separated_by(just(SyntaxKind::T_COMMA).padded_by(ws.clone()))
            .at_least(1)
            .ignored();

        just(SyntaxKind::K_TRANSFORMER)
            .padded_by(ws.clone())
            .ignore_then(ident_p)
            .then(args)
            .then_ignore(just(SyntaxKind::T_COLON).padded_by(ws.clone()))
            .then(outputs)
            .padded_by(ws)
            .map_with_span(|_, sp: Span| sp)
    }

    collect_extern_declarations(tokens, src, SyntaxKind::K_TRANSFORMER, transformer_decl)
}

type State<'a> = SpanCollector<'a, Vec<Simple<SyntaxKind>>>;

fn rule_decl() -> impl Parser<SyntaxKind, Span, Error = Simple<SyntaxKind>> {
    let ws = inline_ws().repeated().ignored();

    let atom_p = atom();

    let statement = rule_statement(ws.clone());

    let body = statement
        .separated_by(just(SyntaxKind::T_COMMA).padded_by(ws.clone()))
        .allow_trailing()
        .at_least(1)
        .ignored();

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

fn rule_statement(
    ws: impl Parser<SyntaxKind, (), Error = Simple<SyntaxKind>> + Clone + 'static,
) -> impl Parser<SyntaxKind, (), Error = Simple<SyntaxKind>> + Clone {
    recursive(|stmt| {
        let ws = ws.clone();
        let block = balanced_block(SyntaxKind::T_LBRACE, SyntaxKind::T_RBRACE);
        let skip_stmt = just(SyntaxKind::K_SKIP).ignored();
        let atom_stmt = atom();

        let condition = balanced_block(SyntaxKind::T_LPAREN, SyntaxKind::T_RPAREN).or(filter(
            |kind: &SyntaxKind| *kind != SyntaxKind::T_LBRACE,
        )
        .ignored()
        .padded_by(ws.clone())
        .repeated()
        .at_least(1)
        .ignored());

        let if_stmt = just(SyntaxKind::K_IF)
            .padded_by(ws.clone())
            .ignore_then(condition.clone())
            .then(stmt.clone().padded_by(ws.clone()))
            .then(
                just(SyntaxKind::K_ELSE)
                    .padded_by(ws.clone())
                    .ignore_then(stmt.clone())
                    .or_not(),
            )
            .ignored();

        let header_expr = for_header(ws.clone());

        let for_stmt = just(SyntaxKind::K_FOR)
            .padded_by(ws.clone())
            .ignore_then(header_expr)
            .then(stmt.clone().padded_by(ws.clone()))
            .ignored();

        let expr_token = choice((
            balanced_block(SyntaxKind::T_LPAREN, SyntaxKind::T_RPAREN),
            balanced_block(SyntaxKind::T_LBRACE, SyntaxKind::T_RBRACE),
            balanced_block(SyntaxKind::T_LBRACKET, SyntaxKind::T_RBRACKET),
            filter(|kind: &SyntaxKind| {
                !matches!(
                    kind,
                    SyntaxKind::T_COMMA
                        | SyntaxKind::T_DOT
                        | SyntaxKind::K_IF
                        | SyntaxKind::K_FOR
                        | SyntaxKind::K_SKIP
                )
            })
            .ignored()
            .padded_by(ws.clone()),
        ));
        let expr_stmt = expr_token.repeated().at_least(1).ignored();

        // Keep `expr_stmt` before `atom_stmt` so general expressions (including
        // assignments/aggregations) win over the more restrictive atom grammar.
        // This ordering guards future refactors from silently changing rule-body
        // semantics.
        choice((for_stmt, if_stmt, block, skip_stmt, expr_stmt, atom_stmt))
            .padded_by(ws.clone())
            .ignored()
    })
}

fn for_header(
    ws: impl Parser<SyntaxKind, (), Error = Simple<SyntaxKind>> + Clone,
) -> impl Parser<SyntaxKind, (), Error = Simple<SyntaxKind>> + Clone {
    just(SyntaxKind::T_LPAREN)
        .padded_by(ws.clone())
        .ignore_then(for_binding_complete(ws.clone()))
        .then_ignore(just(SyntaxKind::T_RPAREN))
        .padded_by(ws)
        .ignored()
}

fn for_binding_complete(
    ws: impl Parser<SyntaxKind, (), Error = Simple<SyntaxKind>> + Clone,
) -> impl Parser<SyntaxKind, (), Error = Simple<SyntaxKind>> + Clone {
    let nested = choice((
        balanced_block(SyntaxKind::T_LPAREN, SyntaxKind::T_RPAREN),
        balanced_block(SyntaxKind::T_LBRACKET, SyntaxKind::T_RBRACKET),
        balanced_block(SyntaxKind::T_LBRACE, SyntaxKind::T_RBRACE),
    ));

    let binding_token = nested.clone().or(filter(|kind: &SyntaxKind| {
        matches!(
            kind,
            SyntaxKind::T_IDENT
                | SyntaxKind::K_UNDERSCORE
                | SyntaxKind::T_COMMA
                | SyntaxKind::T_COLON
                | SyntaxKind::T_COLON_COLON
                | SyntaxKind::T_DOT
                | SyntaxKind::T_AT
                | SyntaxKind::T_HASH
                | SyntaxKind::T_NUMBER
                | SyntaxKind::T_STRING
                | SyntaxKind::T_APOSTROPHE
        )
    })
    .ignored());

    let header_token =
        nested.or(filter(|kind: &SyntaxKind| *kind != SyntaxKind::T_RPAREN).ignored());

    binding_token
        .padded_by(ws.clone())
        .repeated()
        .at_least(1)
        .then_ignore(just(SyntaxKind::K_IN).padded_by(ws.clone()))
        .then(header_token.padded_by(ws).repeated().at_least(1))
        .ignored()
}

/// Return `true` if `span` begins a new logical line in the source.
///
/// Multiple rules on one physical line are recognised by treating a preceding
/// `.` as a line start boundary. Otherwise, only trivia containing a newline
/// counts; inline whitespace or comments keep the current line “open” so
/// continuations are not misclassified.
fn is_at_line_start(st: &State<'_>, span: Span) -> bool {
    if st.stream.cursor() == 0 {
        return true;
    }

    line_start_boundary(
        st.stream.tokens(),
        st.stream.src(),
        st.stream.cursor(),
        span.start,
    )
}

fn line_start_boundary(
    tokens: &[(SyntaxKind, Span)],
    src: &str,
    mut cursor: usize,
    span_start: usize,
) -> bool {
    while cursor > 0 {
        cursor -= 1;
        let Some((kind, prev_span)) = tokens.get(cursor) else {
            break;
        };
        if matches!(kind, SyntaxKind::T_WHITESPACE | SyntaxKind::T_COMMENT) {
            if trivia_contains_newline(src, prev_span) {
                return true;
            }
            continue;
        }

        if gap_contains_newline(src, prev_span.end, span_start) {
            return true;
        }
        return *kind == SyntaxKind::T_DOT;
    }
    false
}

fn trivia_contains_newline(src: &str, span: &Span) -> bool {
    src.get(span.clone())
        .is_some_and(|text| text.contains('\n'))
}

fn gap_contains_newline(src: &str, start: usize, end: usize) -> bool {
    src.get(start..end).is_some_and(|text| text.contains('\n'))
}

/// Parse a rule starting at `span`, returning the span (if successful) and end
/// position to advance the token stream.
fn parse_and_handle_rule(st: &mut State<'_>, span: Span) -> (Option<Span>, usize) {
    let parser = rule_decl();
    let (res, err) = st.parse_span(parser, span.start);
    if let Some(sp) = res {
        let end = sp.end;
        let parsed_span = sp.clone();
        st.spans.push(sp);
        (Some(parsed_span), end)
    } else {
        st.extra.extend(err);
        (None, st.stream.line_end(st.stream.cursor()))
    }
}

/// Orchestrate rule parsing and expression collection when at line start.
fn parse_rule_at_line_start(st: &mut State<'_>, span: Span, exprs: &mut Vec<Span>) {
    if !is_at_line_start(st, span.clone()) {
        st.stream.advance();
        return;
    }

    let start_idx = st.stream.cursor();
    let (rule_span, end) = parse_and_handle_rule(st, span);

    if rule_span.is_none() {
        st.stream.skip_until(end);
        return;
    }

    for span in rule_body_literal_spans(st.stream.tokens(), start_idx, end) {
        if let Err(err) = validate_expression(st.stream.src(), span.clone()) {
            match err {
                crate::parser::expression_span::ExpressionError::Parse(errs) => {
                    st.extra.extend(errs);
                }
                crate::parser::expression_span::ExpressionError::OutOfBounds { span: sp } => {
                    st.extra
                        .push(Simple::custom(sp, "expression span out of bounds"));
                }
            }
        }
        exprs.push(span);
    }

    st.stream.skip_until(end);
}

fn handle_ident(st: &mut State<'_>, span: Span, exprs: &mut Vec<Span>) {
    parse_rule_at_line_start(st, span, exprs);
}

fn handle_implies(st: &mut State<'_>, span: Span, exprs: &mut Vec<Span>) {
    parse_rule_at_line_start(st, span, exprs);
}

/// Collect rule spans while skipping any tokens covered by `exclusions`.
///
/// The `exclusions` slice **must** be sorted by start position and contain no
/// overlapping spans. Adjacent spans are also expected to be merged to avoid
/// redundant skips. Spans are treated as half-open ranges `[start, end)`, and
/// callers must ensure `skip_until(ex.end)` advances to at least `ex.end` so
/// excluded tokens are never revisited.
fn collect_rule_spans(
    tokens: &[(SyntaxKind, Span)],
    src: &str,
    exclusions: &[Span],
) -> (Vec<Span>, Vec<Span>, Vec<Simple<SyntaxKind>>) {
    debug_assert!(
        exclusions
            .windows(2)
            .all(|w| matches!((w.first(), w.get(1)), (Some(a), Some(b)) if a.end <= b.start)),
        "exclusions must be sorted, non-overlapping, and merged"
    );
    let mut st = State::new(tokens, src, Vec::new());
    let mut expr_spans = Vec::new();

    let mut exclude_idx = 0usize;

    while let Some(&(kind, ref span_ref)) = st.stream.peek() {
        let span = span_ref.clone();

        while let Some(ex) = exclusions.get(exclude_idx) {
            if ex.end > span.start {
                break;
            }
            exclude_idx += 1;
        }

        if let Some(ex) = exclusions.get(exclude_idx)
            && ex.start <= span.start
            && span.start < ex.end
        {
            st.stream.skip_until(ex.end);
            continue;
        }

        match kind {
            SyntaxKind::T_IDENT => handle_ident(&mut st, span, &mut expr_spans),
            SyntaxKind::T_IMPLIES => handle_implies(&mut st, span, &mut expr_spans),
            _ => st.stream.advance(),
        }
    }

    let (spans, errors) = st.into_parts();
    (spans, expr_spans, errors)
}

#[cfg(test)]
mod tests {
    //! Tests for the span scanner helper utilities.
    use super::*;
    use crate::test_util::tokenize;
    use chumsky::Stream;
    use rstest::rstest;

    /// Configuration for asserting rule span collection in tests.
    struct RuleSpanAssertConfig<'a> {
        expected_count: usize,
        count_message: &'a str,
        select_last: bool,
        trim_before_check: bool,
        expected_prefix: &'a str,
    }

    impl<'a> RuleSpanAssertConfig<'a> {
        fn new(expected_count: usize, count_message: &'a str, expected_prefix: &'a str) -> Self {
            Self {
                expected_count,
                count_message,
                select_last: false,
                trim_before_check: false,
                expected_prefix,
            }
        }

        fn select_last(self) -> Self {
            Self {
                select_last: true,
                ..self
            }
        }

        fn trim_before_check(self) -> Self {
            Self {
                trim_before_check: true,
                ..self
            }
        }
    }

    /// Assert rule span collection for a given source, with configurable span selection.
    fn assert_rule_span_collection(src: &str, config: &RuleSpanAssertConfig<'_>) {
        let tokens = tokenize(src);
        let (rule_spans, _, errors) = collect_rule_spans(&tokens, src, &[]);
        assert!(errors.is_empty());
        assert_eq!(
            rule_spans.len(),
            config.expected_count,
            "{}",
            config.count_message
        );
        let span = if config.select_last {
            rule_spans
                .last()
                .cloned()
                .unwrap_or_else(|| panic!("expected at least one rule span"))
        } else {
            rule_spans
                .first()
                .cloned()
                .unwrap_or_else(|| panic!("expected at least one rule span"))
        };
        let rule_text = src
            .get(span.clone())
            .unwrap_or_else(|| panic!("rule span {span:?} out of bounds"));
        let checked = if config.trim_before_check {
            rule_text.trim_start()
        } else {
            rule_text
        };
        assert!(
            checked.starts_with(config.expected_prefix),
            "unexpected rule text: {rule_text}"
        );
    }

    fn parse_rule_statement_input(src: &str) -> (Option<()>, Vec<Simple<SyntaxKind>>) {
        let tokens = tokenize(src);
        let ws = inline_ws().repeated().ignored();
        let parser = rule_statement(ws);
        let stream = Stream::from_iter(0..src.len(), tokens.into_iter());
        parser.parse_recovery(stream)
    }

    #[rstest]
    #[case("import foo\n", vec![0..11], true)]
    #[case("import\n", vec![], false)]
    fn collect_import_spans_cases(
        #[case] src: &str,
        #[case] expected: Vec<Span>,
        #[case] errs_empty: bool,
    ) {
        let tokens = tokenize(src);
        let (spans, errs) = collect_import_spans(&tokens, src);
        assert_eq!(spans, expected);
        assert_eq!(errs.is_empty(), errs_empty);
    }

    #[rstest]
    #[case("if (cond) if (nested) Process(nested) else Skip() else Handle()")]
    #[case("for (a in A(a)) for (b in B(b)) ProcessPair(a, b)")]
    #[case("for (item in if cond { Items(item) } else { Others(item) }) Process(item)")]
    #[case("for (item in Items(item)) if (item > 10) Process(item)")]
    #[case("for (item in Items(item) if item.active) Process(item)")]
    #[case(
        "if (outer) { for (item in Items(item)) if (should(item)) Process(item) } else { Skip() }"
    )]
    #[case("Call(x)")]
    #[case("foo = bar(baz)")]
    fn rule_statement_parses_control_flow(#[case] src: &str) {
        let (res, errs) = parse_rule_statement_input(src);
        assert!(res.is_some(), "expected successful parse for {src}");
        assert!(errs.is_empty(), "unexpected errors for {src:?}: {errs:?}");
    }

    #[rstest]
    #[case("if { Process(cond) }")]
    #[case("for (item Items(item)) Process(item)")]
    #[case("for (item in Items(item) Process(item)")]
    fn rule_statement_reports_errors(#[case] src: &str) {
        let (res, errs) = parse_rule_statement_input(src);
        if res.is_some() {
            assert!(
                !errs.is_empty(),
                "expected errors for {src}, but parser recovered without diagnostics",
            );
        } else {
            assert!(
                !errs.is_empty(),
                "expected errors for {src}, but parser reported none",
            );
        }
    }

    #[expect(
        clippy::expect_used,
        reason = "tests unwrap spans to provide crisp failure messages"
    )]
    #[test]
    fn collects_expression_spans_for_each_literal() {
        let src = "\n  R(x) :- Atom(x), if ready(x) { Accept(x) } else { Skip() }.";
        let tokens = tokenize(src);
        let (_rule_spans, expr_spans, errors) = collect_rule_spans(&tokens, src, &[]);
        assert!(errors.is_empty());
        assert_eq!(expr_spans.len(), 2);
        let first = expr_spans
            .first()
            .and_then(|sp| src.get(sp.clone()))
            .expect("first span text missing");
        assert_eq!(first, "Atom(x)");
        let second = expr_spans
            .get(1)
            .cloned()
            .and_then(|sp| src.get(sp))
            .expect("second span text missing");
        assert_eq!(second.trim(), "if ready(x) { Accept(x) } else { Skip() }");
    }

    #[test]
    fn merge_spans_merges_overlapping_and_adjacent() {
        let spans = vec![10..12, 0..5, 5..7, 11..15];
        let merged = merge_spans(spans);
        assert_eq!(merged, vec![0..7, 10..15]);
    }

    #[test]
    fn collect_rule_spans_handles_adjacent_exclusions() {
        let src = "R1(x) :- A(x). R2(x) :- B(x).";
        let tokens = tokenize(src);

        let first_rule = "R1(x) :- A(x).";
        let start = src
            .find(first_rule)
            .unwrap_or_else(|| panic!("missing first rule in {src:?}"));
        let end = start + first_rule.len();
        // Two adjacent exclusion spans covering the first rule.
        let exclusions = vec![start..end - 1, end - 1..end];

        let (rule_spans, _, errors) = collect_rule_spans(&tokens, src, &exclusions);
        assert!(errors.is_empty());
        assert_eq!(rule_spans.len(), 1, "second rule should remain");
        let remaining = rule_spans
            .first()
            .and_then(|sp| src.get(sp.clone()))
            .unwrap_or_else(|| panic!("remaining rule span out of bounds"));
        assert!(
            remaining.starts_with("R2("),
            "unexpected remaining rule: {remaining}"
        );
    }

    #[test]
    fn collect_rule_spans_skips_multiple_disjoint_exclusions() {
        let src = "\
import foo
extern transformer normalize(input: Data): Data
R1(x) :- A(x).
function greet(name: string): string { }
R2(x) :- B(x).
";
        let tokens = tokenize(src);

        let (import_spans, import_errors) = collect_import_spans(&tokens, src);
        assert!(import_errors.is_empty());
        let typedef_spans = collect_typedef_spans(&tokens, src);
        let (relation_spans, relation_errors) = collect_relation_spans(&tokens, src);
        assert!(relation_errors.is_empty());
        let (index_spans, index_errors) = collect_index_spans(&tokens, src);
        assert!(index_errors.is_empty());
        let (function_spans, function_errors) = collect_function_spans(&tokens, src);
        assert!(function_errors.is_empty());
        let (transformer_spans, transformer_errors) = collect_transformer_spans(&tokens, src);
        assert!(transformer_errors.is_empty());

        let mut non_rule_spans = Vec::new();
        non_rule_spans.extend(import_spans);
        non_rule_spans.extend(typedef_spans);
        non_rule_spans.extend(relation_spans);
        non_rule_spans.extend(index_spans);
        non_rule_spans.extend(function_spans);
        non_rule_spans.extend(transformer_spans);
        let non_rule_spans = merge_spans(non_rule_spans);

        let (rule_spans, expr_spans, rule_errors) =
            collect_rule_spans(&tokens, src, &non_rule_spans);
        assert!(rule_errors.is_empty());
        assert_eq!(rule_spans.len(), 2, "expected two rules only");
        assert_eq!(expr_spans.len(), 2, "expected one expression per rule");

        let first_rule_span = rule_spans
            .first()
            .cloned()
            .unwrap_or_else(|| panic!("expected first rule span in {src:?}"));
        let first_rule = src
            .get(first_rule_span.clone())
            .unwrap_or_else(|| panic!("first rule span {first_rule_span:?} out of bounds"));
        assert!(
            first_rule.starts_with("R1("),
            "unexpected first rule text: {first_rule}"
        );
        let second_rule_span = rule_spans
            .get(1)
            .cloned()
            .unwrap_or_else(|| panic!("expected second rule span in {src:?}"));
        let second_rule = src
            .get(second_rule_span.clone())
            .unwrap_or_else(|| panic!("second rule span {second_rule_span:?} out of bounds"));
        assert!(
            second_rule.starts_with("R2("),
            "unexpected second rule text: {second_rule}"
        );
    }

    #[test]
    fn collect_rule_spans_respects_exclusions() {
        let src = "import foo\nextern function f(): void {}\nR(x) :- Foo(x).\n";
        let tokens = tokenize(src);
        let (imports, errors) = collect_import_spans(&tokens, src);
        assert!(errors.is_empty());
        let (functions, fn_errors) = collect_function_spans(&tokens, src);
        assert!(fn_errors.is_empty());

        let imports = merge_spans([imports, functions].concat());

        let (rule_spans, expr_spans, rule_errors) = collect_rule_spans(&tokens, src, &imports);
        assert!(rule_errors.is_empty());
        assert_eq!(rule_spans.len(), 1);
        assert_eq!(expr_spans.len(), 1);

        let rule_span = rule_spans
            .first()
            .cloned()
            .unwrap_or_else(|| panic!("rule span missing for {src:?}"));
        let rule_text = src
            .get(rule_span.clone())
            .unwrap_or_else(|| panic!("rule span {rule_span:?} out of bounds"));
        assert!(
            rule_text.starts_with("R("),
            "unexpected rule text: {rule_text}"
        );

        let expr_span = expr_spans
            .first()
            .cloned()
            .unwrap_or_else(|| panic!("expression span missing for {src:?}"));
        let expr_text = src
            .get(expr_span.clone())
            .unwrap_or_else(|| panic!("expression span {expr_span:?} out of bounds"))
            .trim();
        assert_eq!(expr_text, "Foo(x)");
    }

    #[test]
    fn rule_not_treated_as_line_start_after_inline_spaces() {
        let src = "   R(x) :- A(x).";
        let tokens = tokenize(src);
        let (rule_spans, _, errors) = collect_rule_spans(&tokens, src, &[]);
        assert!(errors.is_empty());
        assert!(
            rule_spans.is_empty(),
            "rule should not start after inline spaces with no boundary"
        );
    }

    #[test]
    fn rule_treated_as_line_start_after_dot_same_line() {
        assert_rule_span_collection(
            "Fact().   R(x) :- A(x).",
            &RuleSpanAssertConfig::new(2, "both fact and trailing rule should parse", "R(")
                .select_last()
                .trim_before_check(),
        );
    }

    #[test]
    fn rule_treated_as_line_start_after_newline_trivia() {
        assert_rule_span_collection(
            "/*c*/\n   // inline comment\nR(x) :- A(x).",
            &RuleSpanAssertConfig::new(1, "newline trivia should start rule", "R("),
        );
    }

    #[test]
    fn parse_tokens_skips_non_rule_constructs_when_scanning_rules() {
        let src = concat!(
            "import foo::bar\n",
            "typedef T = string\n",
            "input relation Log(id: u32) primary key (id)\n",
            "index I_Log on Log(id)\n",
            "function f(): u32 { return 1; }\n",
            "extern transformer normalise(input: Log): Log\n",
            "Rule(x) :- Log(x).\n"
        );

        let tokens = tokenize(src);
        let (spans, errors) = parse_tokens(&tokens, src);

        assert!(errors.is_empty());

        assert_eq!(spans.rules().len(), 1);
        let rule_span = spans
            .rules()
            .first()
            .cloned()
            .unwrap_or_else(|| panic!("missing rule span for {src:?}"));
        let rule_text = src
            .get(rule_span.clone())
            .unwrap_or_else(|| panic!("rule span {rule_span:?} out of bounds"));
        assert_eq!(rule_text.trim_end(), "Rule(x) :- Log(x).");

        assert_eq!(
            spans.expressions().len(),
            1,
            "expected one expression span for rule body"
        );
        let expr_span = spans
            .expressions()
            .first()
            .cloned()
            .unwrap_or_else(|| panic!("expression span missing for {src:?}"));
        let expr_text = src
            .get(expr_span.clone())
            .unwrap_or_else(|| panic!("expression span {expr_span:?} out of bounds"))
            .trim();
        assert_eq!(expr_text, "Log(x)");
    }
}
