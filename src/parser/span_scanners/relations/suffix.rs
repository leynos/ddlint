//! Primary-key suffix parsing and recovery for relation declarations.

use chumsky::error::Simple;

use super::{
    D_REL_004,
    D_REL_006,
    D_REL_007,
    D_REL_008,
    ParsedRelation,
    ScanResult,
    cursor::{
        adjust_stack,
        consume_text,
        current_span,
        inline_trivia_end,
        parse_balanced_block,
        skip_trivia,
        skip_trivia_end,
        token_text,
        token_text_at,
    },
    custom_error,
    preamble::RelationRole,
};
use crate::{
    Span,
    SyntaxKind,
    parser::{
        span_scanners::{BodyForm, RelationSuffixContext},
        token_stream::TokenStream,
    },
};

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
            &TokenStream::new(tokens, src),
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
    // so a malformed-clause D-REL-007 anchors at the binder list, not the next
    // token.
    let binder_list_span = current_span(tokens, src, *cursor);
    let list_start = *cursor;
    let mut has_column = false;
    let mut end = parse_balanced_block(
        &TokenStream::new(tokens, src),
        cursor,
        SyntaxKind::T_RPAREN,
        &mut has_column,
    )?;
    if !has_column || !is_binder_identifier_list(tokens, list_start, *cursor) {
        return Err(Box::new(custom_error(&binder_list_span, D_REL_007)));
    }
    let expression_start = inline_trivia_end(tokens, src, *cursor);
    if next_token_is_same_line_expression(tokens, src, expression_start) {
        *cursor = expression_start;
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
        if stack.is_empty() && starts_statement_boundary(*kind, src, span) {
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

/// Whether a depth-zero token starts a new statement rather than continuing
/// the key expression.
///
/// A `;` terminator or a newline-bearing token ends the declaration. Keeping
/// the test in a helper also keeps the scan loop's condition within Whitaker's
/// branch limit.
fn starts_statement_boundary(kind: SyntaxKind, src: &str, span: &Span) -> bool {
    kind == SyntaxKind::T_SEMI || token_text(src, span).is_some_and(|text| text.contains('\n'))
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
    // `T_IMPLIES`, `T_DOT`, and `T_SEMI` terminate the enclosing statement, so
    // none of them can start a key expression.
    matches!(
        tokens.get(cursor),
        Some((kind, span))
            if !matches!(
                kind,
                SyntaxKind::T_IMPLIES | SyntaxKind::T_DOT | SyntaxKind::T_SEMI
            )
                && token_text(src, span).is_some_and(|text| !text.contains('\n'))
    )
}

/// Return whether `tokens[list_start..list_end]` is a parenthesized,
/// comma-separated, non-empty list of identifiers.
///
/// `list_start` points at the opening `(` and `list_end` just past the matching
/// `)`. Trivia, including comments, is ignored. Anything else, including a
/// trailing comma, is malformed.
fn is_binder_identifier_list(
    tokens: &[(SyntaxKind, Span)],
    list_start: usize,
    list_end: usize,
) -> bool {
    let Some(inner_start) = list_start.checked_add(1) else {
        return false;
    };
    let Some(inner_end) = list_end.checked_sub(1) else {
        return false;
    };
    let Some(inner) = tokens.get(inner_start..inner_end) else {
        return false;
    };

    let mut expect_identifier = true;
    let mut saw_identifier = false;
    for (kind, _) in inner {
        if super::cursor::is_trivia(*kind) {
            continue;
        }
        match (expect_identifier, *kind) {
            (true, SyntaxKind::T_IDENT) => {
                expect_identifier = false;
                saw_identifier = true;
            }
            (false, SyntaxKind::T_COMMA) => expect_identifier = true,
            _ => return false,
        }
    }

    saw_identifier && !expect_identifier
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
