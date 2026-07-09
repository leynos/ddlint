//! Token-walking support for the index declaration scanner.
//!
//! Houses the cursor helpers, delimiter bookkeeping, and error builders
//! shared by the `indexes` scanner module.

use chumsky::prelude::*;

use crate::parser::lexer_helpers::token_display;
use crate::{Span, SyntaxKind};

pub(super) type ScanResult<T> = Result<T, Box<Simple<SyntaxKind>>>;

#[derive(Default)]
pub(super) struct TypeDepths {
    pub(super) paren: usize,
    pub(super) bracket: usize,
    pub(super) brace: usize,
    pub(super) angle: usize,
}

impl TypeDepths {
    pub(super) fn at_top_level(&self) -> bool {
        self.paren == 0 && self.bracket == 0 && self.brace == 0 && self.angle == 0
    }
}

pub(super) fn adjust_type_depths(kind: SyntaxKind, depths: &mut TypeDepths) {
    match kind {
        SyntaxKind::T_LPAREN => depths.paren += 1,
        SyntaxKind::T_RPAREN => depths.paren = depths.paren.saturating_sub(1),
        SyntaxKind::T_LBRACKET => depths.bracket += 1,
        SyntaxKind::T_RBRACKET => depths.bracket = depths.bracket.saturating_sub(1),
        SyntaxKind::T_LBRACE => depths.brace += 1,
        SyntaxKind::T_RBRACE => depths.brace = depths.brace.saturating_sub(1),
        SyntaxKind::T_LT => depths.angle += 1,
        SyntaxKind::T_GT => depths.angle = depths.angle.saturating_sub(1),
        SyntaxKind::T_SHL => depths.angle += 2,
        SyntaxKind::T_SHR => depths.angle = depths.angle.saturating_sub(2),
        _ => {}
    }
}

/// Map an opening delimiter to the closing token it requires.
pub(super) fn expected_closing_for(open_kind: SyntaxKind) -> Option<SyntaxKind> {
    match open_kind {
        SyntaxKind::T_LPAREN => Some(SyntaxKind::T_RPAREN),
        SyntaxKind::T_LBRACKET => Some(SyntaxKind::T_RBRACKET),
        SyntaxKind::T_LBRACE => Some(SyntaxKind::T_RBRACE),
        _ => None,
    }
}

fn matches_closing_token(found: SyntaxKind, expected: SyntaxKind) -> bool {
    match found {
        SyntaxKind::T_SHR => expected == SyntaxKind::T_GT,
        _ => found == expected,
    }
}

/// Pop the delimiter stack entries closed by `found`, validating each.
///
/// `T_SHR` closes two angle brackets in one token; every other closer pops a
/// single entry. Popping past an empty stack stops silently so the caller can
/// treat the surplus closer as the end of the balanced region.
pub(super) fn pop_closing_tokens(
    stack: &mut Vec<SyntaxKind>,
    found: SyntaxKind,
    span: &Span,
) -> ScanResult<()> {
    let closes = if found == SyntaxKind::T_SHR { 2 } else { 1 };
    for _ in 0..closes {
        match stack.pop() {
            Some(expected) if matches_closing_token(found, expected) => {}
            Some(expected) => {
                return Err(Box::new(Simple::custom(
                    span.clone(),
                    format!(
                        "expected '{}' before '{}'",
                        token_display(expected),
                        token_display(found)
                    ),
                )));
            }
            None => break,
        }
    }
    Ok(())
}

/// Push or pop delimiter stack entries for `kind`, if it is a delimiter.
pub(super) fn update_delimiter_stack(
    stack: &mut Vec<SyntaxKind>,
    kind: SyntaxKind,
    span: &Span,
) -> ScanResult<()> {
    match kind {
        SyntaxKind::T_LPAREN => stack.push(SyntaxKind::T_RPAREN),
        SyntaxKind::T_LBRACKET => stack.push(SyntaxKind::T_RBRACKET),
        SyntaxKind::T_LBRACE => stack.push(SyntaxKind::T_RBRACE),
        SyntaxKind::T_LT => stack.push(SyntaxKind::T_GT),
        SyntaxKind::T_SHL => {
            stack.push(SyntaxKind::T_GT);
            stack.push(SyntaxKind::T_GT);
        }
        SyntaxKind::T_RPAREN
        | SyntaxKind::T_RBRACKET
        | SyntaxKind::T_RBRACE
        | SyntaxKind::T_GT
        | SyntaxKind::T_SHR => pop_closing_tokens(stack, kind, span)?,
        _ => {}
    }
    Ok(())
}

pub(super) fn consume_kind(
    tokens: &[(SyntaxKind, Span)],
    src: &str,
    cursor: &mut usize,
    expected: SyntaxKind,
) -> ScanResult<usize> {
    match tokens.get(*cursor) {
        Some((found, span)) if *found == expected => {
            *cursor += 1;
            Ok(span.end)
        }
        Some((found, span)) => Err(Box::new(Simple::expected_input_found(
            span.clone(),
            [Some(expected)],
            Some(*found),
        ))),
        None => Err(Box::new(expected_eof(src, expected))),
    }
}

pub(super) fn consume_kind_or_invalid(
    tokens: &[(SyntaxKind, Span)],
    src: &str,
    cursor: &mut usize,
    expected: SyntaxKind,
) -> ScanResult<usize> {
    match tokens.get(*cursor) {
        Some((found, span)) if *found == expected => {
            *cursor += 1;
            Ok(span.end)
        }
        Some((_, span)) => Err(Box::new(Simple::custom(
            span.clone(),
            super::INVALID_INDEX_FIELD_LIST,
        ))),
        None => Err(Box::new(Simple::custom(
            src.len()..src.len(),
            super::INVALID_INDEX_FIELD_LIST,
        ))),
    }
}

pub(super) fn skip_trivia(tokens: &[(SyntaxKind, Span)], cursor: &mut usize) {
    while let Some((kind, _)) = tokens.get(*cursor) {
        if !is_trivia(*kind) {
            break;
        }
        *cursor += 1;
    }
}

pub(super) fn is_trivia(kind: SyntaxKind) -> bool {
    matches!(kind, SyntaxKind::T_WHITESPACE | SyntaxKind::T_COMMENT)
}

pub(super) fn current_span(tokens: &[(SyntaxKind, Span)], src: &str, cursor: usize) -> Span {
    tokens
        .get(cursor)
        .map_or(src.len()..src.len(), |(_, span)| span.clone())
}

pub(super) fn expected_eof(src: &str, expected: SyntaxKind) -> Simple<SyntaxKind> {
    Simple::expected_input_found(src.len()..src.len(), [Some(expected)], None)
}
