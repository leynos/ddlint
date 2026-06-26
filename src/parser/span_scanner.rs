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
use super::reserved_tokens::collect_reserved_token_errors;
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
    let scanned = scan_non_rule_spans(tokens, src, observer);
    let excluded_spans = scanned.merged_for_rule_scanning();
    let (rule_spans, expr_spans, rule_errors) = observe_rule_scan(observer, || {
        collect_rule_spans(tokens, src, &excluded_spans)
    });
    let mut errors = rule_errors;
    errors.extend(observe_error_scan(observer, || lexer_errors(tokens)));
    errors.extend(collect_reserved_token_errors(tokens, src, &expr_spans));

    build_parsed_spans(
        observer,
        SpanBuildInput {
            scanned,
            rule_spans,
            expr_spans,
            errors,
        },
    )
}

type ScanErrors = Vec<chumsky::error::Simple<SyntaxKind>>;

/// Results from non-rule scanners, consumed within one parser invocation.
///
/// This carrier keeps scan results and their errors together until rule
/// scanning and span construction finish; it must not be retained across calls.
struct NonRuleScanResults {
    attributes: Vec<Span>,
    imports: Vec<Span>,
    typedefs: Vec<Span>,
    relations: Vec<Span>,
    indexes: Vec<Span>,
    functions: Vec<Span>,
    transformers: Vec<Span>,
    applys: Vec<Span>,
    errors: ScanErrors,
}

/// Inputs consumed by one parsed-span construction attempt.
///
/// The value is a single-call carrier so spans and errors cannot leak between
/// parser invocations.
struct SpanBuildInput {
    scanned: NonRuleScanResults,
    rule_spans: Vec<Span>,
    expr_spans: Vec<Span>,
    errors: ScanErrors,
}

impl NonRuleScanResults {
    fn merged_for_rule_scanning(&self) -> Vec<Span> {
        let span_capacity = self.attributes.len()
            + self.imports.len()
            + self.typedefs.len()
            + self.relations.len()
            + self.indexes.len()
            + self.functions.len()
            + self.transformers.len()
            + self.applys.len();

        let mut spans = Vec::with_capacity(span_capacity);
        spans.extend(self.attributes.iter().cloned());
        spans.extend(self.imports.iter().cloned());
        spans.extend(self.typedefs.iter().cloned());
        spans.extend(self.relations.iter().cloned());
        spans.extend(self.indexes.iter().cloned());
        spans.extend(self.functions.iter().cloned());
        spans.extend(self.transformers.iter().cloned());
        spans.extend(self.applys.iter().cloned());
        merge_spans(spans)
    }
}

fn scan_non_rule_spans(
    tokens: &[(SyntaxKind, Span)],
    src: &str,
    observer: &dyn ParseObserver,
) -> NonRuleScanResults {
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

    let mut errors = attribute_errors;
    errors.extend(import_errors);
    errors.extend(typedef_errors);
    errors.extend(relation_errors);
    errors.extend(index_errors);
    errors.extend(function_errors);
    errors.extend(transformer_errors);
    errors.extend(apply_errors);

    NonRuleScanResults {
        attributes: attribute_spans,
        imports: import_spans,
        typedefs: typedef_spans,
        relations: relation_spans,
        indexes: index_spans,
        functions: function_spans,
        transformers: transformer_spans,
        applys: apply_spans,
        errors,
    }
}

fn build_parsed_spans(
    observer: &dyn ParseObserver,
    input: SpanBuildInput,
) -> (ParsedSpans, ScanErrors) {
    let mut all_errors = input.scanned.errors;
    all_errors.extend(input.errors);

    observer.parse_attempt_started(DiagnosticCategory::SpanBuilder);
    let span_result = ParsedSpans::builder()
        .attributes(input.scanned.attributes)
        .imports(input.scanned.imports)
        .typedefs(input.scanned.typedefs)
        .relations(input.scanned.relations)
        .indexes(input.scanned.indexes)
        .functions(input.scanned.functions)
        .transformers(input.scanned.transformers)
        .applys(input.scanned.applys)
        .rules(input.rule_spans)
        .expressions(input.expr_spans)
        .build();

    let spans = match span_result {
        Ok(spans) => {
            complete_attempt(observer, DiagnosticCategory::SpanBuilder, &[]);
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

    use std::cell::RefCell;

    use chumsky::error::SimpleReason;

    use super::{NonRuleScanResults, SpanBuildInput, build_parsed_spans, parse_tokens};
    use crate::parser::ParsedSpans;
    use crate::parser::diagnostics::DiagnosticCategory;
    use crate::parser::observability::{DiagnosticContext, ParseAttemptContext, ParseObserver};
    use crate::test_util::assert_parse_error;
    use crate::{Span, SyntaxKind};

    #[derive(Debug, PartialEq, Eq)]
    enum RecordedEvent {
        AttemptStarted(DiagnosticCategory),
        Diagnostic {
            category: DiagnosticCategory,
            span: Span,
            message: String,
        },
        AttemptCompleted {
            category: DiagnosticCategory,
            diagnostic_count: usize,
        },
    }

    #[derive(Default)]
    struct RecordingObserver(RefCell<Vec<RecordedEvent>>);

    impl ParseObserver for RecordingObserver {
        fn parse_attempt_started(&self, category: DiagnosticCategory) {
            self.0
                .borrow_mut()
                .push(RecordedEvent::AttemptStarted(category));
        }

        fn parse_attempt_completed(&self, context: ParseAttemptContext) {
            self.0.borrow_mut().push(RecordedEvent::AttemptCompleted {
                category: context.category(),
                diagnostic_count: context.diagnostic_count(),
            });
        }

        fn diagnostic_emitted(&self, context: &DiagnosticContext<'_>) {
            self.0.borrow_mut().push(RecordedEvent::Diagnostic {
                category: context.category(),
                span: context.span().clone(),
                message: context.message().to_owned(),
            });
        }
    }

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

    #[test]
    fn span_builder_failure_reports_diagnostic_after_existing_errors() {
        let observer = RecordingObserver::default();
        let input = SpanBuildInput {
            scanned: NonRuleScanResults {
                attributes: Vec::new(),
                imports: Vec::new(),
                typedefs: Vec::new(),
                relations: vec![2..4, 1..3],
                indexes: Vec::new(),
                functions: Vec::new(),
                transformers: Vec::new(),
                applys: Vec::new(),
                errors: vec![chumsky::error::Simple::custom(0..1, "scanner failure")],
            },
            rule_spans: Vec::new(),
            expr_spans: Vec::new(),
            errors: vec![chumsky::error::Simple::custom(4..5, "rule failure")],
        };

        let (spans, errors) = build_parsed_spans(&observer, input);

        assert_eq!(spans, ParsedSpans::default());
        let messages: Vec<_> = errors
            .iter()
            .filter_map(|error| match error.reason() {
                SimpleReason::Custom(message) => Some(message.as_str()),
                _ => None,
            })
            .collect();
        assert_eq!(messages.len(), 3);
        assert_eq!(messages.first(), Some(&"scanner failure"));
        assert_eq!(messages.get(1), Some(&"rule failure"));
        let builder_message = messages.get(2).copied().unwrap_or_default();
        assert!(builder_message.contains("relations not sorted"));

        let events = observer.0.borrow();
        assert_eq!(
            events.first(),
            Some(&RecordedEvent::AttemptStarted(
                DiagnosticCategory::SpanBuilder
            ))
        );
        assert!(matches!(
            events.get(1),
            Some(RecordedEvent::Diagnostic {
                category: DiagnosticCategory::SpanBuilder,
                span,
                message,
            }) if span == &(0..0) && message == builder_message
        ));
        assert_eq!(
            events.last(),
            Some(&RecordedEvent::AttemptCompleted {
                category: DiagnosticCategory::SpanBuilder,
                diagnostic_count: 1,
            })
        );
    }
}
