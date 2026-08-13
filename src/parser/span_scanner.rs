//! Orchestrates top-level span scanning across statement categories.
//!
//! This module delegates to specialised scanners for imports, typedefs,
//! relations, indexes, functions, transformers, and rules. It merges their
//! results into a single `ParsedSpans` structure used by later CST building
//! stages.

use crate::{Span, SyntaxKind};

use super::ParsedSpans;
use super::diagnostics::DiagnosticCategory;
use super::observability::{ParseObserver, complete_attempt};
use super::span_scanners::{
    collect_apply_spans, collect_attribute_spans, collect_function_spans, collect_import_spans,
    collect_index_spans, collect_relation_spans, collect_rule_spans, collect_transformer_spans,
    collect_typedef_spans,
};

/// Scan the token stream and collect spans for each statement category.
pub(super) fn parse_tokens(
    tokens: &[(SyntaxKind, Span)],
    src: &str,
    observer: &dyn ParseObserver,
) -> (ParsedSpans, Vec<chumsky::error::Simple<SyntaxKind>>) {
    let (attribute_spans, attribute_errors) =
        observe_span_scan(observer, DiagnosticCategory::Attribute, || {
            collect_attribute_spans(tokens, src)
        });
    let (import_spans, import_errors) =
        observe_span_scan(observer, DiagnosticCategory::Import, || {
            collect_import_spans(tokens, src)
        });
    let (typedef_spans, typedef_errors) =
        observe_span_scan(observer, DiagnosticCategory::Typedef, || {
            collect_typedef_spans(tokens, src)
        });
    let (relation_spans, relation_errors) =
        observe_span_scan(observer, DiagnosticCategory::Relation, || {
            collect_relation_spans(tokens, src)
        });
    let (index_spans, index_errors) =
        observe_span_scan(observer, DiagnosticCategory::Index, || {
            collect_index_spans(tokens, src)
        });
    let (function_spans, function_errors) =
        observe_span_scan(observer, DiagnosticCategory::Function, || {
            collect_function_spans(tokens, src)
        });
    let (transformer_spans, transformer_errors) =
        observe_span_scan(observer, DiagnosticCategory::Transformer, || {
            collect_transformer_spans(tokens, src)
        });
    let (apply_spans, apply_errors) =
        observe_span_scan(observer, DiagnosticCategory::Apply, || {
            collect_apply_spans(tokens, src)
        });

    let non_rule_span_capacity = attribute_spans.len()
        + import_spans.len()
        + typedef_spans.len()
        + relation_spans.len()
        + index_spans.len()
        + function_spans.len()
        + transformer_spans.len()
        + apply_spans.len();

    let mut non_rule_spans = Vec::with_capacity(non_rule_span_capacity);
    non_rule_spans.extend(attribute_spans.iter().cloned());
    non_rule_spans.extend(import_spans.iter().cloned());
    non_rule_spans.extend(typedef_spans.iter().cloned());
    non_rule_spans.extend(relation_spans.iter().cloned());
    non_rule_spans.extend(index_spans.iter().cloned());
    non_rule_spans.extend(function_spans.iter().cloned());
    non_rule_spans.extend(transformer_spans.iter().cloned());
    non_rule_spans.extend(apply_spans.iter().cloned());
    let non_rule_spans = merge_spans(non_rule_spans);

    let (rule_spans, expr_spans, rule_errors) = observe_rule_scan(observer, || {
        collect_rule_spans(tokens, src, &non_rule_spans)
    });

    let mut all_errors = attribute_errors;
    all_errors.extend(import_errors);
    all_errors.extend(typedef_errors);
    all_errors.extend(relation_errors);
    all_errors.extend(index_errors);
    all_errors.extend(function_errors);
    all_errors.extend(transformer_errors);
    all_errors.extend(apply_errors);
    all_errors.extend(rule_errors);
    let lexer_errors = observe_error_scan(observer, || lexer_errors(tokens));
    all_errors.extend(lexer_errors);

    observer.parse_attempt_started(DiagnosticCategory::SpanBuilder);
    let span_result = ParsedSpans::builder()
        .attributes(attribute_spans)
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
        Ok(spans) => {
            observer.parse_attempt_completed(super::observability::ParseAttemptContext::new(
                DiagnosticCategory::SpanBuilder,
                0,
            ));
            spans
        }
        Err(err) => {
            let span_errors = [chumsky::error::Simple::custom(0..0, err.to_string())];
            complete_attempt(observer, DiagnosticCategory::SpanBuilder, &span_errors);
            all_errors.extend(span_errors);
            ParsedSpans::default()
        }
    };

    (spans, all_errors)
}

type ScanErrors = Vec<chumsky::error::Simple<SyntaxKind>>;

fn observe_span_scan(
    observer: &dyn ParseObserver,
    category: DiagnosticCategory,
    scan: impl FnOnce() -> (Vec<Span>, ScanErrors),
) -> (Vec<Span>, ScanErrors) {
    observer.parse_attempt_started(category);
    let result = scan();
    complete_attempt(observer, category, &result.1);
    result
}

fn observe_rule_scan(
    observer: &dyn ParseObserver,
    scan: impl FnOnce() -> (Vec<Span>, Vec<Span>, ScanErrors),
) -> (Vec<Span>, Vec<Span>, ScanErrors) {
    observer.parse_attempt_started(DiagnosticCategory::Rule);
    let result = scan();
    complete_attempt(observer, DiagnosticCategory::Rule, &result.2);
    result
}

fn observe_error_scan(
    observer: &dyn ParseObserver,
    scan: impl FnOnce() -> ScanErrors,
) -> ScanErrors {
    observer.parse_attempt_started(DiagnosticCategory::Lexer);
    let errors = scan();
    complete_attempt(observer, DiagnosticCategory::Lexer, &errors);
    errors
}

fn lexer_errors(tokens: &[(SyntaxKind, Span)]) -> Vec<chumsky::error::Simple<SyntaxKind>> {
    tokens
        .iter()
        .filter(|(kind, _)| *kind == SyntaxKind::N_ERROR)
        .map(|(_, span)| chumsky::error::Simple::custom(span.clone(), "unrecognized token"))
        .collect()
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

#[cfg(test)]
mod tests {
    //! Tests for top-level span-scan orchestration.

    use super::parse_tokens;
    use crate::SyntaxKind;
    use crate::test_util::assert_parse_error;

    #[test]
    fn parse_tokens_reports_unrecognized_token_for_error_token() {
        // A synthetic `N_ERROR` token exercises the `lexer_errors` path, which
        // surfaces an "unrecognized token" diagnostic anchored at the token's
        // span.
        let src = "@";
        let tokens = vec![(SyntaxKind::N_ERROR, 0..src.len())];

        let (_spans, errors) = parse_tokens(
            &tokens,
            src,
            &super::super::observability::NoopParseObserver,
        );

        assert_parse_error(&errors, "unrecognized token", 0, src.len());
    }
}
