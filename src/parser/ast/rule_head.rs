//! Rule head parsing helpers.
//!
//! Rule heads have adornments that do not appear in general expression
//! positions: head locations (`@ expr`) and the head-only by-reference
//! lowering (`&Rel{...}` → `ref_new(Rel{...})`).

use chumsky::error::Simple;

use crate::parser::ast::{Expr, UnaryOp};
use crate::parser::expression::parse_expression;
use crate::parser::span_utils::{parse_u32_decimal, shift_errors, trim_byte_range};
use crate::{DdlogLanguage, Span, SyntaxKind, tokenize_with_trivia, tokenize_without_trivia};

const REF_NEW_FN: &str = "ref_new";

#[derive(Debug, Default)]
struct DelimiterDepths {
    paren: usize,
    brace: usize,
    bracket: usize,
}

impl DelimiterDepths {
    fn at_top_level(&self) -> bool {
        self.paren == 0 && self.brace == 0 && self.bracket == 0
    }
}

fn adjust_delimiter_depth(kind: SyntaxKind, depths: &mut DelimiterDepths) {
    match kind {
        SyntaxKind::T_LPAREN => depths.paren += 1,
        SyntaxKind::T_RPAREN => depths.paren = depths.paren.saturating_sub(1),
        SyntaxKind::T_LBRACE => depths.brace += 1,
        SyntaxKind::T_RBRACE => depths.brace = depths.brace.saturating_sub(1),
        SyntaxKind::T_LBRACKET => depths.bracket += 1,
        SyntaxKind::T_RBRACKET => depths.bracket = depths.bracket.saturating_sub(1),
        _ => {}
    }
}

fn process_token_for_head_text(
    t: &rowan::SyntaxToken<DdlogLanguage>,
    at_top_level: bool,
    buf: &mut String,
    depths: &mut DelimiterDepths,
) -> bool {
    match t.kind() {
        SyntaxKind::T_COMMA if at_top_level => false,
        SyntaxKind::T_IMPLIES | SyntaxKind::T_DOT => false,
        kind => {
            buf.push_str(t.text());
            adjust_delimiter_depth(kind, depths);
            true
        }
    }
}

pub(crate) fn first_head_text(syntax: &rowan::SyntaxNode<DdlogLanguage>) -> Option<String> {
    use rowan::NodeOrToken;

    let mut buf = String::new();
    let mut depths = DelimiterDepths::default();

    for e in syntax.children_with_tokens() {
        let at_top_level = depths.at_top_level();
        match e {
            NodeOrToken::Token(t) => {
                if !process_token_for_head_text(&t, at_top_level, &mut buf, &mut depths) {
                    break;
                }
            }
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

/// Parsed representation of a single rule head.
#[derive(Debug, Clone, PartialEq)]
pub struct RuleHead {
    /// Atom expression for the head, including diff/delay wrappers.
    pub atom: Expr,
    /// Optional `@` location expression.
    pub location: Option<Expr>,
}

pub(crate) fn parse_rule_heads(
    rule_src: &str,
    base_offset: usize,
) -> Result<Vec<RuleHead>, Vec<Simple<SyntaxKind>>> {
    let head_end = find_rule_head_end(rule_src);
    let head_src = rule_src.get(..head_end).unwrap_or("");
    let head_tokens = tokenize_without_trivia(head_src);
    let head_spans = split_top_level_commas(&head_tokens);

    let mut heads = Vec::new();
    let mut errors = Vec::new();

    for span in head_spans {
        match parse_rule_head_span(head_src, &span, base_offset) {
            Ok(Some(head)) => heads.push(head),
            Ok(None) => {}
            Err(mut errs) => errors.append(&mut errs),
        }
    }

    if errors.is_empty() {
        Ok(heads)
    } else {
        Err(errors)
    }
}

fn parse_rule_head_span(
    head_src: &str,
    span: &Span,
    base_offset: usize,
) -> Result<Option<RuleHead>, Vec<Simple<SyntaxKind>>> {
    let raw = head_src.get(span.clone()).unwrap_or("");
    let (start, end) = trim_byte_range(raw);
    if start >= end {
        return Ok(None);
    }

    let trimmed = raw.get(start..end).unwrap_or("");
    let segment_offset = base_offset.saturating_add(span.start).saturating_add(start);

    let tokens = tokenize_without_trivia(trimmed);
    let at_idx = find_top_level_at(&tokens);

    if let Some(at_idx) = at_idx {
        let at_span = tokens
            .get(at_idx)
            .map_or_else(|| trimmed.len()..trimmed.len(), |(_, sp)| sp.clone());
        let delay = parse_delay_suffix(trimmed, &tokens, segment_offset)?;

        let location_start = at_span.end;
        let location_end = delay
            .as_ref()
            .map_or_else(|| trimmed.len(), |d| d.full.start);

        let atom_src = trimmed.get(..at_span.start).unwrap_or("");
        let location_src = trimmed.get(location_start..location_end).unwrap_or("");

        let mut atom = parse_expr_with_offset(atom_src, segment_offset)?;
        if let Some(delay) = delay {
            atom = Expr::AtomDelay {
                delay: delay.value,
                expr: Box::new(atom),
            };
        }
        atom = lower_by_ref_head(atom);

        let location = Some(parse_expr_with_offset(
            location_src,
            segment_offset.saturating_add(location_start),
        )?);

        return Ok(Some(RuleHead { atom, location }));
    }

    let atom = lower_by_ref_head(parse_expr_with_offset(trimmed, segment_offset)?);
    Ok(Some(RuleHead {
        atom,
        location: None,
    }))
}

fn parse_expr_with_offset(src: &str, base_offset: usize) -> Result<Expr, Vec<Simple<SyntaxKind>>> {
    let (start, end) = trim_byte_range(src);
    let trimmed = src.get(start..end).unwrap_or("");
    match parse_expression(trimmed) {
        Ok(expr) => Ok(expr),
        Err(errs) => Err(shift_errors(errs, base_offset.saturating_add(start))),
    }
}

fn lower_by_ref_head(expr: Expr) -> Expr {
    match expr {
        Expr::Unary {
            op: UnaryOp::Ref,
            expr,
        } => Expr::Call {
            callee: Box::new(Expr::Variable(REF_NEW_FN.to_string())),
            args: vec![*expr],
        },
        Expr::AtomDiff { expr } => Expr::AtomDiff {
            expr: Box::new(lower_by_ref_head(*expr)),
        },
        Expr::AtomDelay { delay, expr } => Expr::AtomDelay {
            delay,
            expr: Box::new(lower_by_ref_head(*expr)),
        },
        other => other,
    }
}

fn find_rule_head_end(rule_src: &str) -> usize {
    for (kind, span) in tokenize_with_trivia(rule_src) {
        if matches!(kind, SyntaxKind::T_IMPLIES | SyntaxKind::T_DOT) {
            return span.start;
        }
    }
    rule_src.len()
}

fn split_top_level_commas(tokens: &[(SyntaxKind, Span)]) -> Vec<Span> {
    let mut spans = Vec::new();
    if tokens.is_empty() {
        return spans;
    }

    let mut start_idx = 0usize;
    let mut depths = DelimiterDepths::default();

    for (idx, (kind, _)) in tokens.iter().enumerate() {
        adjust_delimiter_depth(*kind, &mut depths);

        let at_top_level = depths.at_top_level();
        if at_top_level && *kind == SyntaxKind::T_COMMA {
            spans.extend(span_for_token_range(tokens, start_idx, idx));
            start_idx = idx + 1;
        }
    }

    spans.extend(span_for_token_range(tokens, start_idx, tokens.len()));
    spans
}

fn span_for_token_range(tokens: &[(SyntaxKind, Span)], start: usize, end: usize) -> Option<Span> {
    if start >= end {
        return None;
    }
    let start_span = tokens.get(start).map(|(_, sp)| sp.clone())?;
    let end_span = tokens
        .get(end.saturating_sub(1))
        .map(|(_, sp)| sp.clone())?;
    Some(start_span.start..end_span.end)
}

fn find_top_level_at(tokens: &[(SyntaxKind, Span)]) -> Option<usize> {
    let mut depths = DelimiterDepths::default();
    let mut at_idx = None;

    for (idx, (kind, _)) in tokens.iter().enumerate() {
        adjust_delimiter_depth(*kind, &mut depths);

        let at_top_level = depths.at_top_level();
        if at_top_level && *kind == SyntaxKind::T_AT {
            at_idx = Some(idx);
            break;
        }
    }

    at_idx
}

#[derive(Debug, Clone)]
struct DelaySuffix {
    full: Span,
    value: u32,
}

fn parse_delay_suffix(
    src: &str,
    tokens: &[(SyntaxKind, Span)],
    base_offset: usize,
) -> Result<Option<DelaySuffix>, Vec<Simple<SyntaxKind>>> {
    if tokens.len() < 4 {
        return Ok(None);
    }

    let start = tokens.len() - 4;
    let Some((SyntaxKind::T_MINUS, minus_span)) = tokens.get(start) else {
        return Ok(None);
    };
    if tokens.get(start + 1).map(|(k, _)| *k) != Some(SyntaxKind::T_LT) {
        return Ok(None);
    }
    let Some((SyntaxKind::T_NUMBER, number_span)) = tokens.get(start + 2) else {
        return Ok(None);
    };
    if tokens.get(start + 3).map(|(k, _)| *k) != Some(SyntaxKind::T_GT) {
        return Ok(None);
    }

    let raw = src.get(number_span.clone()).unwrap_or("");
    let value = match parse_u32_decimal(raw) {
        Ok(value) => value,
        Err(msg) => {
            let shifted_span = base_offset.saturating_add(number_span.start)
                ..base_offset.saturating_add(number_span.end);
            return Err(vec![Simple::custom(shifted_span, msg)]);
        }
    };

    let full = minus_span.start..tokens.last().map_or(minus_span.end, |(_, sp)| sp.end);
    Ok(Some(DelaySuffix { full, value }))
}
