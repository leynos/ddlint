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
