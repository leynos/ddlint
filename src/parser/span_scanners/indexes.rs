//! Scanner for index declarations.
//!
//! Parses `index` statements, capturing their spans and any syntax errors.

use chumsky::prelude::*;

use crate::parser::lexer_helpers::token_display;
use crate::{Span, SyntaxKind};

use super::utils::State;

#[path = "indexes_support.rs"]
mod support;

use support::{
    ScanResult, TypeDepths, adjust_type_depths, consume_kind, consume_kind_or_invalid,
    current_span, expected_closing_for, expected_eof, is_trivia, skip_trivia,
    update_delimiter_stack,
};

pub(crate) const MISSING_INDEX_FIELD_LIST: &str =
    "index declarations require a typed field list `(name: Type, ...)` before `on`";
pub(crate) const INVALID_INDEX_FIELD_LIST: &str =
    "index declarations require one or more typed fields in the form `name: Type`";

pub(crate) fn collect_index_spans(
    tokens: &[(SyntaxKind, Span)],
    src: &str,
) -> (Vec<Span>, Vec<Simple<SyntaxKind>>) {
    let mut st: State<'_> = State::new(tokens, src, Vec::new());

    let handle_index = |st: &mut State<'_>, span: Span| {
        let start = span.start;
        match parse_index_decl(st.stream.tokens(), st.stream.src(), st.stream.cursor()) {
            Ok(end) => {
                st.spans.push(start..end);
                st.stream.skip_until(end);
            }
            Err(error) => {
                st.extra.push(*error);
                st.skip_line();
            }
        }
    };

    token_dispatch!(st, {
        SyntaxKind::K_INDEX => handle_index,
    });

    st.into_parts()
}

fn parse_index_decl(
    tokens: &[(SyntaxKind, Span)],
    src: &str,
    mut cursor: usize,
) -> ScanResult<usize> {
    consume_kind(tokens, src, &mut cursor, SyntaxKind::K_INDEX)?;
    skip_trivia(tokens, &mut cursor);
    consume_kind(tokens, src, &mut cursor, SyntaxKind::T_IDENT)?;
    skip_trivia(tokens, &mut cursor);
    parse_index_fields(tokens, src, &mut cursor)?;
    skip_trivia(tokens, &mut cursor);
    consume_kind(tokens, src, &mut cursor, SyntaxKind::K_ON)?;
    skip_trivia(tokens, &mut cursor);
    let mut end = parse_index_target(tokens, src, &mut cursor)?;
    while let Some((kind, span)) = tokens.get(cursor) {
        if !is_trivia(*kind) {
            break;
        }
        end = span.end;
        cursor += 1;
    }
    Ok(end)
}

fn parse_index_fields(
    tokens: &[(SyntaxKind, Span)],
    src: &str,
    cursor: &mut usize,
) -> ScanResult<()> {
    match tokens.get(*cursor) {
        Some((SyntaxKind::K_ON, span)) => {
            return Err(Box::new(Simple::custom(
                span.clone(),
                MISSING_INDEX_FIELD_LIST,
            )));
        }
        Some((SyntaxKind::T_LPAREN, _)) => {}
        Some((found, span)) => {
            return Err(Box::new(Simple::expected_input_found(
                span.clone(),
                [Some(SyntaxKind::T_LPAREN)],
                Some(*found),
            )));
        }
        None => return Err(Box::new(expected_eof(src, SyntaxKind::T_LPAREN))),
    }

    *cursor += 1;
    skip_trivia(tokens, cursor);

    if matches!(tokens.get(*cursor), Some((SyntaxKind::T_RPAREN, _))) {
        return Err(Box::new(Simple::custom(
            current_span(tokens, src, *cursor),
            INVALID_INDEX_FIELD_LIST,
        )));
    }

    loop {
        consume_kind_or_invalid(tokens, src, cursor, SyntaxKind::T_IDENT)?;
        skip_trivia(tokens, cursor);
        if !matches!(tokens.get(*cursor), Some((SyntaxKind::T_COLON, _))) {
            return Err(Box::new(Simple::custom(
                current_span(tokens, src, *cursor),
                INVALID_INDEX_FIELD_LIST,
            )));
        }
        *cursor += 1;
        skip_trivia(tokens, cursor);
        parse_type_expr(tokens, src, cursor)?;
        skip_trivia(tokens, cursor);

        match tokens.get(*cursor) {
            Some((SyntaxKind::T_COMMA, _)) => {
                *cursor += 1;
                skip_trivia(tokens, cursor);
                if matches!(tokens.get(*cursor), Some((SyntaxKind::T_RPAREN, _))) {
                    return Err(Box::new(Simple::custom(
                        current_span(tokens, src, *cursor),
                        INVALID_INDEX_FIELD_LIST,
                    )));
                }
            }
            Some((SyntaxKind::T_RPAREN, _)) => {
                *cursor += 1;
                return Ok(());
            }
            _ => {
                return Err(Box::new(Simple::custom(
                    current_span(tokens, src, *cursor),
                    INVALID_INDEX_FIELD_LIST,
                )));
            }
        }
    }
}

fn parse_type_expr(tokens: &[(SyntaxKind, Span)], src: &str, cursor: &mut usize) -> ScanResult<()> {
    let mut consumed = false;
    let mut depths = TypeDepths::default();

    while let Some((kind, _span)) = tokens.get(*cursor) {
        if is_trivia(*kind) {
            *cursor += 1;
            continue;
        }

        if depths.at_top_level() && matches!(kind, SyntaxKind::T_COMMA | SyntaxKind::T_RPAREN) {
            break;
        }

        consumed = true;
        adjust_type_depths(*kind, &mut depths);
        *cursor += 1;
    }

    if consumed {
        Ok(())
    } else {
        Err(Box::new(Simple::custom(
            current_span(tokens, src, *cursor),
            INVALID_INDEX_FIELD_LIST,
        )))
    }
}

fn parse_index_target(
    tokens: &[(SyntaxKind, Span)],
    src: &str,
    cursor: &mut usize,
) -> ScanResult<usize> {
    if matches!(tokens.get(*cursor), Some((SyntaxKind::T_AMP, _))) {
        *cursor += 1;
        skip_trivia(tokens, cursor);
    }

    let mut end = consume_kind_or_invalid(tokens, src, cursor, SyntaxKind::T_IDENT)?;
    skip_trivia(tokens, cursor);

    while let Some((kind, _)) = tokens.get(*cursor) {
        if !matches!(kind, SyntaxKind::T_COLON_COLON | SyntaxKind::T_DOT) {
            break;
        }
        *cursor += 1;
        skip_trivia(tokens, cursor);
        end = consume_kind_or_invalid(tokens, src, cursor, SyntaxKind::T_IDENT)?;
        skip_trivia(tokens, cursor);
    }

    if let Some((SyntaxKind::T_APOSTROPHE, span)) = tokens.get(*cursor) {
        end = span.end;
        *cursor += 1;
        skip_trivia(tokens, cursor);
    }

    if let Some((kind, _)) = tokens.get(*cursor)
        && matches!(
            kind,
            SyntaxKind::T_LPAREN | SyntaxKind::T_LBRACKET | SyntaxKind::T_LBRACE
        )
    {
        end = parse_balanced_arguments(tokens, src, cursor)?;
        skip_trivia(tokens, cursor);
    }

    if matches!(tokens.get(*cursor), Some((SyntaxKind::T_MINUS, _))) {
        end = parse_delay_suffix(tokens, src, cursor)?;
    }

    Ok(end)
}

fn parse_delay_suffix(
    tokens: &[(SyntaxKind, Span)],
    src: &str,
    cursor: &mut usize,
) -> ScanResult<usize> {
    consume_kind(tokens, src, cursor, SyntaxKind::T_MINUS)?;
    skip_trivia(tokens, cursor);
    consume_kind(tokens, src, cursor, SyntaxKind::T_LT)?;
    skip_trivia(tokens, cursor);
    consume_kind_or_invalid(tokens, src, cursor, SyntaxKind::T_NUMBER)?;
    skip_trivia(tokens, cursor);
    consume_kind(tokens, src, cursor, SyntaxKind::T_GT)
}

fn parse_balanced_arguments(
    tokens: &[(SyntaxKind, Span)],
    src: &str,
    cursor: &mut usize,
) -> ScanResult<usize> {
    let Some((open_kind, open_span)) = tokens.get(*cursor) else {
        return Err(Box::new(expected_eof(src, SyntaxKind::T_LPAREN)));
    };
    let Some(expected_close) = expected_closing_for(*open_kind) else {
        return Err(Box::new(Simple::custom(
            open_span.clone(),
            "expected index target arguments",
        )));
    };

    let mut stack = vec![expected_close];
    *cursor += 1;

    while let Some((kind, span)) = tokens.get(*cursor) {
        update_delimiter_stack(&mut stack, *kind, span)?;
        *cursor += 1;
        if stack.is_empty() {
            return Ok(span.end);
        }
    }

    Err(Box::new(Simple::custom(
        src.len()..src.len(),
        format!("unclosed '{}'", token_display(expected_close)),
    )))
}
