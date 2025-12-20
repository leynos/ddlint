//! Utilities for locating and validating expression spans.
//!
//! The functions in this module help other parser components extract the byte
//! ranges of expressions from a token stream. They also allow validating the
//! text within those ranges using the Pratt expression parser without
//! introducing additional coupling.

use chumsky::error::Simple;
use thiserror::Error;

use crate::parser::delimiter::find_top_level_eq_span;
use crate::parser::expression::parse_expression;
use crate::parser::pattern::parse_pattern;
use crate::parser::span_utils::{shift_errors, trim_byte_range};
use crate::{Span, SyntaxKind};

/// Split a rule body into the spans of its comma-separated literals.
///
/// The function walks the token slice starting at `start_idx` and ending when
/// the byte offset `end` is reached. Each literal span is trimmed to exclude
/// trivia so downstream consumers can parse expressions without re-trimming.
#[must_use]
pub(crate) fn rule_body_literal_spans(
    tokens: &[(SyntaxKind, Span)],
    start_idx: usize,
    end: usize,
) -> Vec<Span> {
    let Some((body_start, body_end)) = locate_body_bounds(tokens, start_idx, end) else {
        return Vec::new();
    };

    split_literals(tokens, body_start, body_end)
}

fn locate_body_bounds(
    tokens: &[(SyntaxKind, Span)],
    start_idx: usize,
    end: usize,
) -> Option<(usize, usize)> {
    let mut idx = start_idx;
    let mut body_start = None;
    let mut body_end = None;

    while let Some((kind, span)) = tokens.get(idx) {
        if span.start >= end {
            break;
        }

        match kind {
            SyntaxKind::T_IMPLIES => body_start = Some(idx + 1),
            SyntaxKind::T_DOT => {
                body_end = Some(idx);
                break;
            }
            _ => {}
        }

        idx += 1;
    }

    let start = body_start?;
    let end_idx = body_end.unwrap_or(idx);
    if start >= end_idx {
        None
    } else {
        Some((start, end_idx))
    }
}

fn split_literals(tokens: &[(SyntaxKind, Span)], start: usize, end: usize) -> Vec<Span> {
    let mut spans = Vec::new();
    let mut literal_start = None;
    let mut paren_depth = 0usize;
    let mut brace_depth = 0usize;
    let mut bracket_depth = 0usize;

    let mut idx = start;
    while idx < end {
        let Some((kind, _)) = tokens.get(idx) else {
            break;
        };

        if literal_start.is_none() {
            literal_start = Some(idx);
        }

        match kind {
            SyntaxKind::T_LPAREN => paren_depth += 1,
            SyntaxKind::T_RPAREN => paren_depth = paren_depth.saturating_sub(1),
            SyntaxKind::T_LBRACE => brace_depth += 1,
            SyntaxKind::T_RBRACE => brace_depth = brace_depth.saturating_sub(1),
            SyntaxKind::T_LBRACKET => bracket_depth += 1,
            SyntaxKind::T_RBRACKET => bracket_depth = bracket_depth.saturating_sub(1),
            _ => {}
        }

        let at_top_level = paren_depth == 0 && brace_depth == 0 && bracket_depth == 0;
        if at_top_level
            && matches!(kind, SyntaxKind::T_COMMA)
            && let Some(start_idx) = literal_start.take()
            && let Some(sp) = trim_literal_span(tokens, start_idx, idx)
        {
            spans.push(sp);
        }

        idx += 1;
    }

    if let Some(start_idx) = literal_start
        && let Some(sp) = trim_literal_span(tokens, start_idx, end)
    {
        spans.push(sp);
    }

    spans
}

fn trim_literal_span(
    tokens: &[(SyntaxKind, Span)],
    start_idx: usize,
    end_idx: usize,
) -> Option<Span> {
    if start_idx >= end_idx {
        return None;
    }

    let mut first = start_idx;
    while first < end_idx {
        match tokens.get(first) {
            Some((kind, _)) if is_trivia(*kind) => first += 1,
            Some(_) => break,
            None => return None,
        }
    }

    if first >= end_idx {
        return None;
    }

    let mut last = end_idx - 1;
    while last > first {
        match tokens.get(last) {
            Some((kind, _)) if is_trivia(*kind) => last -= 1,
            Some(_) => break,
            None => return None,
        }
    }

    let start = tokens.get(first)?.1.start;
    let end = tokens.get(last)?.1.end;
    (start < end).then_some(start..end)
}

fn is_trivia(kind: SyntaxKind) -> bool {
    matches!(kind, SyntaxKind::T_WHITESPACE | SyntaxKind::T_COMMENT)
}

/// Parse the text within `span` using the expression parser.
///
/// Any syntax errors reported by the parser are returned for the caller to
/// aggregate. A successful parse yields `()`.
#[derive(Debug, Error)]
pub(crate) enum ExpressionError {
    /// The provided span falls outside the source text bounds.
    #[error("span {span:?} out of bounds")]
    OutOfBounds { span: Span },
    /// The expression parser reported syntax errors.
    #[error("{0:?}")]
    Parse(Vec<Simple<SyntaxKind>>),
}

pub(crate) fn validate_expression(src: &str, span: Span) -> Result<(), ExpressionError> {
    let base_offset = span.start;
    let text = src
        .get(span.clone())
        .ok_or(ExpressionError::OutOfBounds { span: span.clone() })?;
    // Rule bodies allow pattern assignments that the expression parser on its
    // own would reject. Validate their shape here so syntax errors still
    // surface during span scanning.
    if let Some(eq_span) = find_top_level_eq_span(text) {
        let lhs_full = text.get(..eq_span.start).unwrap_or("");
        let (lhs_start, lhs_end) = trim_byte_range(lhs_full);
        let lhs_trimmed = lhs_full.get(lhs_start..lhs_end).unwrap_or("");
        if lhs_trimmed.is_empty() {
            return Err(ExpressionError::Parse(vec![Simple::custom(
                span.clone(),
                "expected pattern before '=' in rule literal",
            )]));
        }
        if let Err(errs) = parse_pattern(lhs_trimmed) {
            return Err(ExpressionError::Parse(shift_errors(
                errs,
                base_offset.saturating_add(lhs_start),
            )));
        }
        let rhs_full = text.get(eq_span.end..).unwrap_or("");
        let (rhs_start, rhs_end) = trim_byte_range(rhs_full);
        let rhs_trimmed = rhs_full.get(rhs_start..rhs_end).unwrap_or("");
        if rhs_trimmed.is_empty() {
            return Err(ExpressionError::Parse(vec![Simple::custom(
                span.clone(),
                "expected expression after '=' in rule literal",
            )]));
        }
        return parse_expression(rhs_trimmed).map(|_| ()).map_err(|errs| {
            ExpressionError::Parse(shift_errors(
                errs,
                base_offset
                    .saturating_add(eq_span.end)
                    .saturating_add(rhs_start),
            ))
        });
    }
    parse_expression(text)
        .map(|_| ())
        .map_err(|errs| ExpressionError::Parse(shift_errors(errs, base_offset)))
}
