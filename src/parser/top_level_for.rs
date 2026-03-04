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
    let mut exclude_idx = 0usize;

    while let Some((kind, span)) = tokens.get(cursor) {
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
            cursor = skip_until(tokens, cursor, ex.end);
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

    let mut paren_depth = 0usize;
    let mut brace_depth = 0usize;
    let mut bracket_depth = 0usize;
    let mut last_errors = None;

    for (idx, (kind, span)) in tokens.iter().enumerate().skip(start_idx) {
        adjust_depths(
            *kind,
            &mut paren_depth,
            &mut brace_depth,
            &mut bracket_depth,
        );

        if *kind != SyntaxKind::T_DOT {
            continue;
        }
        if paren_depth != 0 || brace_depth != 0 || bracket_depth != 0 {
            continue;
        }

        let Some(raw_statement) = src.get(start..span.start) else {
            continue;
        };
        let (trim_start, trim_end) = trim_byte_range(raw_statement);
        if trim_start == trim_end {
            continue;
        }
        let Some(trimmed_statement) = raw_statement.get(trim_start..trim_end) else {
            continue;
        };

        match parse_expression(trimmed_statement) {
            Ok(expr) if matches!(expr, Expr::ForLoop { .. }) => {
                let statement_span = start..span.end;
                let next_cursor = skip_until(tokens, start_idx, statement_span.end);
                return (Some((statement_span, expr, next_cursor)), Vec::new());
            }
            Ok(_) => {}
            Err(errs) => {
                last_errors = Some(shift_errors(errs, start.saturating_add(trim_start)));
            }
        }

        if idx + 1 >= tokens.len() {
            break;
        }
    }

    let fallback = vec![Simple::custom(
        start..start + FOR_KEYWORD_LEN,
        UNTERMINATED_TOP_LEVEL_FOR,
    )];
    (None, last_errors.unwrap_or(fallback))
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

fn adjust_depths(
    kind: SyntaxKind,
    paren_depth: &mut usize,
    brace_depth: &mut usize,
    bracket_depth: &mut usize,
) {
    match kind {
        SyntaxKind::T_LPAREN => *paren_depth += 1,
        SyntaxKind::T_RPAREN => *paren_depth = paren_depth.saturating_sub(1),
        SyntaxKind::T_LBRACE => *brace_depth += 1,
        SyntaxKind::T_RBRACE => *brace_depth = brace_depth.saturating_sub(1),
        SyntaxKind::T_LBRACKET => *bracket_depth += 1,
        SyntaxKind::T_RBRACKET => *bracket_depth = bracket_depth.saturating_sub(1),
        _ => {}
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

        if range_contains_newline(src, prev_span.end..span_start) {
            return true;
        }
        return *kind == SyntaxKind::T_DOT;
    }
    false
}

fn range_contains_newline(src: &str, range: std::ops::Range<usize>) -> bool {
    src.get(range).is_some_and(|text| text.contains('\n'))
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
        if src
            .get(span.clone())
            .is_some_and(|text| text.contains('\n'))
        {
            break;
        }
    }
    end
}
