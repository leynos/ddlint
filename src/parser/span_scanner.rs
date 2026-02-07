//! Orchestrates top-level span scanning across statement categories.
//!
//! This module delegates to specialised scanners for imports, typedefs,
//! relations, indexes, functions, transformers, and rules. It merges their
//! results into a single `ParsedSpans` structure used by later CST building
//! stages.

use crate::{Span, SyntaxKind};

use super::ParsedSpans;
use super::span_scanners::{
    collect_apply_spans, collect_function_spans, collect_import_spans, collect_index_spans,
    collect_relation_spans, collect_rule_spans, collect_transformer_spans, collect_typedef_spans,
};

/// Scan the token stream and collect spans for each statement category.
pub(super) fn parse_tokens(
    tokens: &[(SyntaxKind, Span)],
    src: &str,
) -> (ParsedSpans, Vec<chumsky::error::Simple<SyntaxKind>>) {
    let (import_spans, errors) = collect_import_spans(tokens, src);
    let (typedef_spans, typedef_errors) = collect_typedef_spans(tokens, src);
    let (relation_spans, relation_errors) = collect_relation_spans(tokens, src);
    let (index_spans, index_errors) = collect_index_spans(tokens, src);
    let (function_spans, function_errors) = collect_function_spans(tokens, src);
    let (transformer_spans, transformer_errors) = collect_transformer_spans(tokens, src);
    let (apply_spans, apply_errors) = collect_apply_spans(tokens, src);

    let non_rule_span_capacity = import_spans.len()
        + typedef_spans.len()
        + relation_spans.len()
        + index_spans.len()
        + function_spans.len()
        + transformer_spans.len()
        + apply_spans.len();

    let mut non_rule_spans = Vec::with_capacity(non_rule_span_capacity);
    non_rule_spans.extend(import_spans.iter().cloned());
    non_rule_spans.extend(typedef_spans.iter().cloned());
    non_rule_spans.extend(relation_spans.iter().cloned());
    non_rule_spans.extend(index_spans.iter().cloned());
    non_rule_spans.extend(function_spans.iter().cloned());
    non_rule_spans.extend(transformer_spans.iter().cloned());
    non_rule_spans.extend(apply_spans.iter().cloned());
    let non_rule_spans = merge_spans(non_rule_spans);

    let (rule_spans, expr_spans, rule_errors) = collect_rule_spans(tokens, src, &non_rule_spans);

    let mut all_errors = errors;
    all_errors.extend(typedef_errors);
    all_errors.extend(relation_errors);
    all_errors.extend(index_errors);
    all_errors.extend(function_errors);
    all_errors.extend(transformer_errors);
    all_errors.extend(apply_errors);
    all_errors.extend(rule_errors);

    let span_result = ParsedSpans::builder()
        .imports(import_spans)
        .typedefs(typedef_spans)
        .relations(relation_spans)
        .indexes(index_spans)
        .functions(function_spans)
        .transformers(transformer_spans)
        .applys(apply_spans)
        .rules(rule_spans)
        .expressions(expr_spans)
        .build();

    let spans = match span_result {
        Ok(spans) => spans,
        Err(err) => {
            all_errors.push(chumsky::error::Simple::custom(0..0, err.to_string()));
            ParsedSpans::default()
        }
    };

    (spans, all_errors)
}

/// Return a sorted, merged copy of the provided spans.
///
/// Adjacent or overlapping spans are coalesced to minimise skip checks during
/// rule scanning.
pub(super) fn merge_spans(mut spans: Vec<Span>) -> Vec<Span> {
    spans.sort_by_key(|sp| sp.start);
    let mut merged: Vec<Span> = Vec::with_capacity(spans.len());
    for span in spans {
        if let Some(last) = merged.last_mut()
            && span.start <= last.end
        {
            last.end = last.end.max(span.end);
            continue;
        }
        merged.push(span);
    }
    merged
}
