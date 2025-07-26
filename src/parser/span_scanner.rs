//! Statement span scanning utilities.
//!
//! This module walks a token stream to identify top-level statement spans.
//! It groups the ranges for imports, typedefs, relations, indexes,
//! functions, transformers and rules. The spans are later used to build the
//! CST without needing full parsing.

use chumsky::prelude::*;

use crate::parser::expression::parse_expression;
use crate::{Span, SyntaxKind};

use super::{
    ParsedSpans,
    lexer_helpers::{atom, balanced_block, balanced_block_nonempty, ident, inline_ws},
    span_collector::SpanCollector,
};

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
    let (rule_spans, expr_spans, rule_errors) = collect_rule_spans(tokens, src);

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

fn collect_import_spans(
    tokens: &[(SyntaxKind, Span)],
    src: &str,
) -> (Vec<Span>, Vec<Simple<SyntaxKind>>) {
    type State<'a> = SpanCollector<'a, Vec<Simple<SyntaxKind>>>;

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

    let handler = move |st: &mut State<'_>, span: Span| {
        let mut idx = st.stream.cursor() + 1;
        while let Some(tok) = st.stream.tokens().get(idx) {
            let text = st.stream.src().get(tok.1.clone()).unwrap_or("");
            if matches!(tok.0, SyntaxKind::T_WHITESPACE | SyntaxKind::T_COMMENT)
                && !text.contains('\n')
            {
                idx += 1;
            } else {
                break;
            }
        }
        if st.stream.tokens().get(idx).map(|t| t.0) != Some(decl_kind) {
            st.skip_line();
            return;
        }

        let ws = inline_ws().repeated();
        let parser = just(SyntaxKind::K_EXTERN)
            .padded_by(ws.clone())
            .ignore_then(decl_parser());

        let (res, err) = st.parse_span(parser, span.start);
        if let Some(sp) = res {
            st.spans.push(span.start..sp.end);
            st.stream.skip_until(sp.end);
        } else {
            st.extra.extend(err);
            st.skip_line();
        }
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

    fn consume_paren_block(st: &mut State<'_>) -> bool {
        if let Some((SyntaxKind::T_LPAREN, span)) = st.stream.peek().cloned() {
            let (res, err) = st.parse_span(super::ast::parse_utils::paren_block_span(), span.start);
            st.extra.errors.extend(err);
            if let Some(sp) = res {
                st.stream.skip_until(sp.end);
                true
            } else {
                st.skip_line();
                false
            }
        } else {
            true
        }
    }

    fn skip_relation_columns(st: &mut State<'_>) -> bool {
        st.stream.skip_ws_inline();
        if matches!(st.stream.peek().map(|t| t.0), Some(SyntaxKind::T_IDENT)) {
            st.stream.advance();
        }
        st.stream.skip_ws_inline();
        consume_paren_block(st)
    }

    fn skip_primary_key_clause(st: &mut State<'_>) -> bool {
        st.stream.skip_ws_inline();
        if let Some((SyntaxKind::T_IDENT, span)) = st.stream.peek().cloned()
            && st.extra.src.get(span.clone()) == Some("primary")
        {
            st.stream.advance();
            st.stream.skip_ws_inline();
            if let Some((SyntaxKind::T_IDENT, sp)) = st.stream.peek().cloned()
                && st.extra.src.get(sp.clone()) == Some("key")
            {
                st.stream.advance();
                st.stream.skip_ws_inline();
                return consume_paren_block(st);
            }
        }
        true
    }

    fn record_relation(st: &mut State<'_>, start: usize) {
        let cols_ok = skip_relation_columns(st);
        let pk_ok = skip_primary_key_clause(st);

        if cols_ok && pk_ok {
            let end = st.stream.line_end(st.stream.cursor());
            st.stream.skip_until(end);
            st.spans.push(start..end);
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

fn collect_rule_spans(
    tokens: &[(SyntaxKind, Span)],
    src: &str,
) -> (Vec<Span>, Vec<Span>, Vec<Simple<SyntaxKind>>) {
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

    fn parse_rule_at_line_start(st: &mut State<'_>, span: Span, exprs: &mut Vec<Span>) {
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

        let start_idx = st.stream.cursor();
        let parser = rule_decl();
        let (res, err) = st.parse_span(parser, span.start);
        let end = if let Some(sp) = res {
            st.spans.push(sp.clone());
            sp.end
        } else {
            st.extra.extend(err);
            st.stream.line_end(st.stream.cursor())
        };

        let tokens = st.stream.tokens();
        let mut expr_start = None;
        let mut expr_end = None;
        let mut idx = start_idx;
        while let Some(tok) = tokens.get(idx) {
            if tok.1.start >= end {
                break;
            }
            if tok.0 == SyntaxKind::T_IMPLIES {
                expr_start = tokens.get(idx + 1).map(|t| t.1.start);
            } else if tok.0 == SyntaxKind::T_DOT {
                expr_end = Some(tok.1.start);
                break;
            }
            idx += 1;
        }
        if let (Some(start), Some(end_pos)) = (expr_start, expr_end)
            && start < end_pos
        {
            let span = start..end_pos;
            let text = st.stream.src().get(span.clone()).unwrap_or("");
            if let Err(mut errs) = parse_expression(text) {
                st.extra.append(&mut errs);
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

    let mut st = State::new(tokens, src, Vec::new());
    let mut expr_spans = Vec::new();

    while let Some(&(kind, ref span_ref)) = st.stream.peek() {
        let span = span_ref.clone();
        match kind {
            SyntaxKind::T_IDENT => handle_ident(&mut st, span, &mut expr_spans),
            SyntaxKind::T_IMPLIES => handle_implies(&mut st, span, &mut expr_spans),
            _ => st.stream.advance(),
        }
    }

    let (spans, errors) = st.into_parts();
    (spans, expr_spans, errors)
}
