//! Cursor helpers for the relation span scanner.
//!
//! These helpers keep token movement and balanced-block scanning separate from
//! relation grammar decisions in the parent module.

use chumsky::{Error, error::Simple};

use crate::parser::lexer_helpers::token_display;
use crate::{Span, SyntaxKind};

use super::{D_REL_007, ScanResult, custom_error};

/// Parse a balanced block opened by the delimiter matching `close`.
///
/// `has_content` is set when a non-trivia token appears before the matching
/// close at the outermost block depth. Reaching EOF before the stack empties
/// returns an unclosed-delimiter diagnostic anchored at EOF.
pub(super) fn parse_balanced_block(
    tokens: &[(SyntaxKind, Span)],
    src: &str,
    cursor: &mut usize,
    close: SyntaxKind,
    has_content: &mut bool,
) -> ScanResult<usize> {
    consume_open_for_close(tokens, src, cursor, close)?;
    let mut stack = vec![close];
    while let Some((kind, span)) = tokens.get(*cursor) {
        if !is_trivia(*kind) && stack.len() == 1 && *kind != close {
            *has_content = true;
        }
        adjust_stack(*kind, &mut stack);
        *cursor += 1;
        if stack.is_empty() {
            return Ok(span.end);
        }
    }
    Err(Box::new(Simple::custom(
        src.len()..src.len(),
        format!("unclosed '{}'", token_display(close)),
    )))
}

/// Consume a token with the expected syntax kind.
///
/// Returns the consumed token end offset. A mismatched token or EOF produces a
/// structured parser error naming the expected kind.
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
        None => Err(Box::new(Simple::expected_input_found(
            src.len()..src.len(),
            [Some(expected)],
            None,
        ))),
    }
}

/// Consume a token whose source text exactly matches `expected`.
///
/// Returns the consumed token end offset. A mismatch or EOF produces D-REL-007
/// because textual matching is only used inside primary-key clause recovery.
pub(super) fn consume_text(
    tokens: &[(SyntaxKind, Span)],
    src: &str,
    cursor: &mut usize,
    expected: &'static str,
) -> ScanResult<usize> {
    match tokens.get(*cursor) {
        Some((_, span)) if token_text(src, span) == Some(expected) => {
            *cursor += 1;
            Ok(span.end)
        }
        Some((_, span)) => Err(Box::new(custom_error(span, D_REL_007))),
        None => Err(Box::new(custom_error(&(src.len()..src.len()), D_REL_007))),
    }
}

/// Advance the cursor past whitespace and comments, including newlines.
pub(super) fn skip_trivia(tokens: &[(SyntaxKind, Span)], cursor: &mut usize) {
    while let Some((kind, _)) = tokens.get(*cursor) {
        if !is_trivia(*kind) {
            break;
        }
        *cursor += 1;
    }
}

/// Advance past trivia while returning the end offset of the last skipped token.
///
/// This is used when relation spans should include trailing trivia already
/// consumed by the scanner.
pub(super) fn skip_trivia_end(
    tokens: &[(SyntaxKind, Span)],
    cursor: &mut usize,
    mut end: usize,
) -> usize {
    while let Some((kind, span)) = tokens.get(*cursor) {
        if !is_trivia(*kind) {
            break;
        }
        end = span.end;
        *cursor += 1;
    }
    end
}

/// Advance past whitespace and comments until a newline or non-trivia token.
///
/// Unlike [`skip_trivia`], this keeps newline-separated declarations distinct.
pub(super) fn skip_inline_trivia(tokens: &[(SyntaxKind, Span)], src: &str, cursor: &mut usize) {
    while let Some((kind, span)) = tokens.get(*cursor) {
        if !is_trivia(*kind) || token_text(src, span).is_some_and(|text| text.contains('\n')) {
            break;
        }
        *cursor += 1;
    }
}

/// Return whether a token kind is trivia for relation-scanner cursor movement.
pub(super) fn is_trivia(kind: SyntaxKind) -> bool {
    matches!(kind, SyntaxKind::T_WHITESPACE | SyntaxKind::T_COMMENT)
}

/// Return the source text for the token at `cursor`.
///
/// Returns `None` when the cursor is at EOF or the token span is not valid for
/// the provided source text.
pub(super) fn token_text_at<'a>(
    tokens: &'a [(SyntaxKind, Span)],
    src: &'a str,
    cursor: usize,
) -> Option<&'a str> {
    tokens
        .get(cursor)
        .and_then(|(_, span)| token_text(src, span))
}

/// Return the source text covered by `span`.
///
/// Returns `None` when the span is outside the source text bounds.
pub(super) fn token_text<'a>(src: &'a str, span: &Span) -> Option<&'a str> {
    src.get(span.clone())
}

/// Return the current token span or an EOF span when the cursor is exhausted.
pub(super) fn current_span(tokens: &[(SyntaxKind, Span)], src: &str, cursor: usize) -> Span {
    tokens
        .get(cursor)
        .map_or(src.len()..src.len(), |(_, span)| span.clone())
}

/// Update a delimiter stack for the provided token kind.
///
/// Opening delimiters push their matching close token. Closing delimiters pop
/// only when they match the current top, leaving mismatches for the caller's
/// recovery path.
pub(super) fn adjust_stack(kind: SyntaxKind, stack: &mut Vec<SyntaxKind>) {
    match kind {
        SyntaxKind::T_LPAREN => stack.push(SyntaxKind::T_RPAREN),
        SyntaxKind::T_LBRACKET => stack.push(SyntaxKind::T_RBRACKET),
        SyntaxKind::T_LBRACE => stack.push(SyntaxKind::T_RBRACE),
        SyntaxKind::T_RPAREN | SyntaxKind::T_RBRACKET | SyntaxKind::T_RBRACE => {
            if stack.last().copied() == Some(kind) {
                stack.pop();
            }
        }
        _ => {}
    }
}

fn consume_open_for_close(
    tokens: &[(SyntaxKind, Span)],
    src: &str,
    cursor: &mut usize,
    close: SyntaxKind,
) -> ScanResult<()> {
    let open = match close {
        SyntaxKind::T_RPAREN => SyntaxKind::T_LPAREN,
        SyntaxKind::T_RBRACKET => SyntaxKind::T_LBRACKET,
        SyntaxKind::T_RBRACE => SyntaxKind::T_LBRACE,
        _ => {
            return Err(Box::new(custom_error(
                &current_span(tokens, src, *cursor),
                D_REL_007,
            )));
        }
    };
    consume_kind(tokens, src, cursor, open).map(|_| ())
}
