//! Top-level `for` desugaring into semantic rules.
//!
//! This module keeps CST construction lossless by deriving semantic rules from
//! source spans instead of injecting synthetic rule nodes into the syntax tree.

use chumsky::error::Simple;

use crate::parser::ast::{Expr, SemanticRule, SemanticRuleOrigin};
use crate::parser::expression::parse_expression;
use crate::parser::span_utils::{shift_errors, trim_byte_range};
use crate::{Span, SyntaxKind};

/// Diagnostic for top-level `for` statements whose bodies cannot lower.
pub(crate) const UNSUPPORTED_TOP_LEVEL_FOR_STATEMENT: &str = concat!(
    "top-level `for` body must end in an atom-like expression ",
    "(for example `Rel(args)`)"
);

const UNTERMINATED_TOP_LEVEL_FOR: &str =
    "unterminated top-level `for` statement; expected trailing `.`";

const FOR_KEYWORD_LEN: usize = "for".len();

type ParsedTopLevelFor = Option<(Span, Expr, usize)>;
type TopLevelForParseOutcome = (ParsedTopLevelFor, Vec<Simple<SyntaxKind>>);

#[derive(Debug)]
struct ExclusionCursor<'a> {
    exclusions: &'a [Span],
    index: usize,
}

impl<'a> ExclusionCursor<'a> {
    fn new(exclusions: &'a [Span]) -> Self {
        Self {
            exclusions,
            index: 0,
        }
    }

    fn overlap_with(&mut self, span: &Span) -> Option<Span> {
        while let Some(ex) = self.exclusions.get(self.index) {
            if ex.end > span.start {
                break;
            }
            self.index += 1;
        }

        self.exclusions.get(self.index).and_then(|ex| {
            if span.start < ex.end && ex.start < span.end {
                Some(ex.clone())
            } else {
                None
            }
        })
    }
}

#[derive(Debug, Default)]
struct DelimiterDepth {
    paren: usize,
    brace: usize,
    bracket: usize,
}

impl DelimiterDepth {
    fn update(&mut self, kind: SyntaxKind) {
        match kind {
            SyntaxKind::T_LPAREN => self.paren += 1,
            SyntaxKind::T_RPAREN => self.paren = self.paren.saturating_sub(1),
            SyntaxKind::T_LBRACE => self.brace += 1,
            SyntaxKind::T_RBRACE => self.brace = self.brace.saturating_sub(1),
            SyntaxKind::T_LBRACKET => self.bracket += 1,
            SyntaxKind::T_RBRACKET => self.bracket = self.bracket.saturating_sub(1),
            _ => {}
        }
    }

    fn is_top_level(&self) -> bool {
        self.paren == 0 && self.brace == 0 && self.bracket == 0
    }
}

/// Collect and desugar top-level `for` statements.
///
/// `exclusions` must be sorted and non-overlapping. Tokens whose starts fall
/// inside exclusions are skipped, so this scanner does not reprocess
/// declarations or explicit rule spans already handled elsewhere.
pub(crate) fn collect_desugared_top_level_for_rules(
    tokens: &[(SyntaxKind, Span)],
    src: &str,
    exclusions: &[Span],
) -> (Vec<SemanticRule>, Vec<Simple<SyntaxKind>>) {
    debug_assert!(
        exclusions
            .windows(2)
            .all(|w| matches!((w.first(), w.get(1)), (Some(a), Some(b)) if a.end <= b.start)),
        "exclusions must be sorted, non-overlapping, and merged"
    );

    let mut rules = Vec::new();
    let mut errors = Vec::new();
    let mut cursor = 0usize;
    let mut exclusion_cursor = ExclusionCursor::new(exclusions);

    while let Some((kind, span)) = tokens.get(cursor) {
        if let Some(exclusion) = exclusion_cursor.overlap_with(span) {
            cursor = skip_until(tokens, cursor, exclusion.end);
            continue;
        }

        if *kind == SyntaxKind::K_FOR && is_at_line_start(tokens, src, cursor, span.start) {
            let (parsed, parse_errors) = parse_top_level_for_statement(tokens, src, cursor);
            if let Some((statement_span, expression, next_cursor)) = parsed {
                if let Some(rule) =
                    lower_top_level_for(expression, statement_span.clone(), &mut errors)
                {
                    rules.push(rule);
                }
                cursor = next_cursor;
                continue;
            }

            errors.extend(parse_errors);
            let end = line_end(tokens, src, cursor);
            cursor = skip_until(tokens, cursor, end);
            continue;
        }

        cursor += 1;
    }

    (rules, errors)
}

fn parse_top_level_for_statement(
    tokens: &[(SyntaxKind, Span)],
    src: &str,
    start_idx: usize,
) -> TopLevelForParseOutcome {
    let Some((_, start_span)) = tokens.get(start_idx) else {
        return (None, Vec::new());
    };
    let start = start_span.start;

    let mut search_idx = start_idx;
    let mut parsed_statement = None;
    let mut last_errors = None;

    while let Some((dot_span, dot_idx)) = find_top_level_dot(tokens, src, search_idx) {
        search_idx = dot_idx + 1;

        let Some((trim_start, _trim_end, trimmed_statement)) =
            extract_trimmed_statement(src, start, dot_span.start)
        else {
            continue;
        };

        match try_parse_for_expression(trimmed_statement, start, trim_start) {
            Ok(Some(expr)) => {
                let statement_span = start..dot_span.end;
                let next_cursor = skip_until(tokens, start_idx, statement_span.end);
                parsed_statement = Some((statement_span, expr, next_cursor));
            }
            Ok(None) => {}
            Err(errs) => {
                last_errors = Some(errs);
            }
        }
    }

    if let Some(statement) = parsed_statement {
        return (Some(statement), Vec::new());
    }

    let fallback = vec![Simple::custom(
        start..start + FOR_KEYWORD_LEN,
        UNTERMINATED_TOP_LEVEL_FOR,
    )];
    (None, last_errors.unwrap_or(fallback))
}

fn find_top_level_dot(
    tokens: &[(SyntaxKind, Span)],
    src: &str,
    start_idx: usize,
) -> Option<(Span, usize)> {
    let mut depth = DelimiterDepth::default();
    for (idx, (kind, span)) in tokens.iter().enumerate().skip(start_idx) {
        depth.update(*kind);
        if *kind == SyntaxKind::T_DOT
            && depth.is_top_level()
            && is_top_level_for_statement_terminator_dot(tokens, src, span.start)
        {
            return Some((span.clone(), idx));
        }
    }
    None
}

fn extract_trimmed_statement(
    src: &str,
    start: usize,
    dot_start: usize,
) -> Option<(usize, usize, &str)> {
    let raw_statement = src.get(start..dot_start)?;
    let (trim_start, trim_end) = trim_byte_range(raw_statement);
    if trim_start == trim_end {
        return None;
    }
    let trimmed_statement = raw_statement.get(trim_start..trim_end)?;
    Some((trim_start, trim_end, trimmed_statement))
}

fn try_parse_for_expression(
    trimmed_statement: &str,
    start: usize,
    trim_start: usize,
) -> Result<Option<Expr>, Vec<Simple<SyntaxKind>>> {
    match parse_expression(trimmed_statement) {
        Ok(expr) if matches!(expr, Expr::ForLoop { .. }) => Ok(Some(expr)),
        Ok(_) => Ok(None),
        Err(errs) => Err(shift_errors(errs, start.saturating_add(trim_start))),
    }
}

fn lower_top_level_for(
    expression: Expr,
    statement_span: Span,
    errors: &mut Vec<Simple<SyntaxKind>>,
) -> Option<SemanticRule> {
    let mut body = Vec::new();
    let Some(head) = collect_lowered_terms(expression, &mut body) else {
        errors.push(Simple::custom(
            statement_span.clone(),
            UNSUPPORTED_TOP_LEVEL_FOR_STATEMENT,
        ));
        return None;
    };
    if !is_supported_head_expression(&head) {
        errors.push(Simple::custom(
            statement_span.clone(),
            UNSUPPORTED_TOP_LEVEL_FOR_STATEMENT,
        ));
        return None;
    }

    Some(SemanticRule::new(
        SemanticRuleOrigin::TopLevelFor,
        statement_span,
        head,
        body,
    ))
}

fn collect_lowered_terms(expression: Expr, body_terms: &mut Vec<Expr>) -> Option<Expr> {
    match expression {
        Expr::ForLoop {
            iterable,
            guard,
            body,
            ..
        } => {
            body_terms.push(*iterable);
            if let Some(guard_expr) = guard {
                body_terms.push(*guard_expr);
            }
            collect_lowered_terms(*body, body_terms)
        }
        Expr::IfElse { .. }
        | Expr::Match { .. }
        | Expr::Break
        | Expr::Continue
        | Expr::Return { .. }
        | Expr::Binary {
            op: crate::parser::ast::BinaryOp::Seq,
            ..
        } => None,
        other => Some(other),
    }
}

fn is_supported_head_expression(expr: &Expr) -> bool {
    match expr {
        Expr::Call { .. } | Expr::Apply { .. } => true,
        Expr::AtomDiff { expr } | Expr::AtomDelay { expr, .. } => {
            is_supported_head_expression(expr)
        }
        _ => false,
    }
}

fn is_at_line_start(
    tokens: &[(SyntaxKind, Span)],
    src: &str,
    mut cursor: usize,
    span_start: usize,
) -> bool {
    if cursor == 0 {
        return true;
    }

    while cursor > 0 {
        cursor -= 1;
        let Some((kind, prev_span)) = tokens.get(cursor) else {
            break;
        };
        if matches!(kind, SyntaxKind::T_WHITESPACE | SyntaxKind::T_COMMENT) {
            if range_contains_newline(src, prev_span.clone()) {
                return true;
            }
            continue;
        }

        if slice_contains_newline(src, prev_span.end, span_start) {
            return true;
        }
        return *kind == SyntaxKind::T_DOT;
    }

    true
}

fn range_contains_newline(src: &str, range: std::ops::Range<usize>) -> bool {
    src.get(range).is_some_and(|text| text.contains('\n'))
}

fn slice_contains_newline(src: &str, start: usize, end: usize) -> bool {
    range_contains_newline(src, start..end)
}

fn token_index_by_start(tokens: &[(SyntaxKind, Span)], start: usize) -> Option<usize> {
    tokens
        .binary_search_by_key(&start, |(_, span)| span.start)
        .ok()
}

fn next_significant_token(
    tokens: &[(SyntaxKind, Span)],
    start_idx: usize,
) -> Option<(SyntaxKind, Span)> {
    tokens
        .iter()
        .skip(start_idx)
        .find(|(kind, _)| !matches!(kind, SyntaxKind::T_WHITESPACE | SyntaxKind::T_COMMENT))
        .map(|(kind, span)| (*kind, span.clone()))
}

/// Return `true` when the `.` token at `dot_start` terminates a top-level
/// `for` statement.
///
/// Dots immediately followed by identifier/tuple-index tokens with no trivia
/// gap are treated as expression postfix access (for example `pair.0`), not as
/// statement terminators.
pub(crate) fn is_top_level_for_statement_terminator_dot(
    tokens: &[(SyntaxKind, Span)],
    src: &str,
    dot_start: usize,
) -> bool {
    let Some(dot_idx) = token_index_by_start(tokens, dot_start) else {
        return false;
    };
    let Some((kind, dot_span)) = tokens.get(dot_idx) else {
        return false;
    };
    if *kind != SyntaxKind::T_DOT {
        return false;
    }

    let Some((next_kind, next_span)) = next_significant_token(tokens, dot_idx + 1) else {
        return true;
    };

    if slice_contains_newline(src, dot_span.end, next_span.start) {
        return true;
    }

    let has_trivia_gap = next_span.start > dot_span.end;
    if !has_trivia_gap && matches!(next_kind, SyntaxKind::T_IDENT | SyntaxKind::T_NUMBER) {
        return false;
    }

    true
}

fn skip_until(tokens: &[(SyntaxKind, Span)], mut cursor: usize, end: usize) -> usize {
    while let Some((_, span)) = tokens.get(cursor) {
        if span.end <= end {
            cursor += 1;
        } else {
            break;
        }
    }
    cursor
}

fn line_end(tokens: &[(SyntaxKind, Span)], src: &str, start: usize) -> usize {
    let mut end = tokens.get(start).map_or(src.len(), |t| t.1.end);
    for (_, span) in tokens.iter().skip(start) {
        end = span.end;
        if range_contains_newline(src, span.clone()) {
            break;
        }
    }
    end
}
