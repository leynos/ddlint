//! Scanner for rule declarations and embedded expressions.
//!
//! Handles rule statements, control flow, and expression span collection while
//! respecting exclusions produced by other scanners.

use chumsky::prelude::*;

use crate::parser::{
    expression_span::{rule_body_literal_spans, validate_expression, ExpressionError},
    lexer_helpers::{atom, balanced_block, inline_ws},
};
use crate::{Span, SyntaxKind};

use super::utils::State;

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

pub(super) fn rule_statement(
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
                ExpressionError::Parse(errs) => {
                    st.extra.extend(errs);
                }
                ExpressionError::OutOfBounds { span: sp } => {
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

pub(crate) fn collect_rule_spans(
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
