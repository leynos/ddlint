//! Primary-key suffix parsing and recovery for relation declarations.

use chumsky::error::Simple;

use crate::{Span, SyntaxKind};

use super::cursor::{
    adjust_stack, consume_text, current_span, parse_balanced_block, skip_inline_trivia,
    skip_trivia, skip_trivia_end, token_text, token_text_at,
};
use super::preamble::RelationRole;
use super::{D_REL_004, D_REL_006, D_REL_007, D_REL_008, ParsedRelation, ScanResult, custom_error};
use crate::parser::span_scanners::{BodyForm, RelationSuffixContext};

pub(super) fn parse_relation_suffix(
    tokens: &[(SyntaxKind, Span)],
    src: &str,
    mut cursor: usize,
    context: RelationSuffixContext,
) -> ScanResult<ParsedRelation> {
    let body_end = context.body_end;
    let mut trailing_end = skip_trivia_end(tokens, &mut cursor, body_end);

    if starts_bracket_wrapped_primary_key(tokens, src, cursor) {
        let mut has_content = true;
        let recovery_end = parse_balanced_block(
            tokens,
            src,
            &mut cursor,
            SyntaxKind::T_RBRACKET,
            &mut has_content,
        )?;
        return Ok(recoverable_relation(body_end, recovery_end, D_REL_008));
    }

    if !starts_primary_key(tokens, src, cursor) {
        return Ok(ParsedRelation {
            end: trailing_end,
            recovery_end: trailing_end,
            error: None,
        });
    }

    if context.body == BodyForm::Bracket {
        let recovery_end = parse_primary_key_clause(tokens, src, &mut cursor)?;
        return Ok(recoverable_relation(body_end, recovery_end, D_REL_004));
    }

    if context.role != Some(RelationRole::Input) {
        let recovery_end = parse_primary_key_clause(tokens, src, &mut cursor)?;
        return Ok(recoverable_relation(body_end, recovery_end, D_REL_006));
    }

    let mut end = parse_primary_key_clause(tokens, src, &mut cursor)?;
    trailing_end = skip_trivia_end(tokens, &mut cursor, end);
    end = trailing_end;
    Ok(ParsedRelation {
        end,
        recovery_end: end,
        error: None,
    })
}

fn parse_primary_key_clause(
    tokens: &[(SyntaxKind, Span)],
    src: &str,
    cursor: &mut usize,
) -> ScanResult<usize> {
    consume_text(tokens, src, cursor, "primary")?;
    skip_trivia(tokens, cursor);
    consume_text(tokens, src, cursor, "key")?;
    skip_trivia(tokens, cursor);
    // Capture the clause span before `parse_balanced_block` advances the cursor
    // so an empty-clause D-REL-007 anchors at the `()`, not the next token.
    let empty_key_span = current_span(tokens, src, *cursor);
    let mut has_column = false;
    let mut end = parse_balanced_block(tokens, src, cursor, SyntaxKind::T_RPAREN, &mut has_column)?;
    if !has_column {
        return Err(Box::new(custom_error(&empty_key_span, D_REL_007)));
    }
    skip_inline_trivia(tokens, src, cursor);
    if next_token_is_same_line_expression(tokens, src, *cursor) {
        end = consume_opaque_primary_key_expr(tokens, src, cursor)?;
    }
    Ok(end)
}

fn consume_opaque_primary_key_expr(
    tokens: &[(SyntaxKind, Span)],
    src: &str,
    cursor: &mut usize,
) -> ScanResult<usize> {
    let mut stack = Vec::new();
    let mut end = current_span(tokens, src, *cursor).start;

    while let Some((kind, span)) = tokens.get(*cursor) {
        if stack.is_empty() && token_text(src, span).is_some_and(|text| text.contains('\n')) {
            break;
        }
        adjust_stack(*kind, &mut stack);
        end = span.end;
        *cursor += 1;
    }

    if stack.is_empty() {
        Ok(end)
    } else {
        Err(Box::new(Simple::custom(
            current_span(tokens, src, *cursor),
            D_REL_007,
        )))
    }
}

pub(super) fn starts_primary_key(tokens: &[(SyntaxKind, Span)], src: &str, cursor: usize) -> bool {
    token_text_at(tokens, src, cursor) == Some("primary")
}

fn starts_bracket_wrapped_primary_key(
    tokens: &[(SyntaxKind, Span)],
    src: &str,
    cursor: usize,
) -> bool {
    if !matches!(tokens.get(cursor), Some((SyntaxKind::T_LBRACKET, _))) {
        return false;
    }
    let mut next = cursor + 1;
    skip_trivia(tokens, &mut next);
    token_text_at(tokens, src, next) == Some("primary")
}

fn next_token_is_same_line_expression(
    tokens: &[(SyntaxKind, Span)],
    src: &str,
    cursor: usize,
) -> bool {
    matches!(
        tokens.get(cursor),
        Some((kind, span))
            if !matches!(kind, SyntaxKind::T_IMPLIES | SyntaxKind::T_DOT)
                && token_text(src, span).is_some_and(|text| !text.contains('\n'))
    )
}

fn recoverable_relation(
    span_start: usize,
    recovery_end: usize,
    message: &'static str,
) -> ParsedRelation {
    ParsedRelation {
        end: span_start,
        recovery_end,
        error: Some(Simple::custom(span_start..recovery_end, message)),
    }
}
