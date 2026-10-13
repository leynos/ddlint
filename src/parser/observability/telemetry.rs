//! Optional `tracing` and `metrics` observer adapter.

use metrics::{counter, describe_counter};

use super::{DiagnosticContext, ParseAttemptContext, ParseObserver};
use crate::parser::diagnostics::DiagnosticCategory;

const ATTEMPT_METRIC: &str = "ddlint_parser_attempts_total";
const DIAGNOSTIC_METRIC: &str = "ddlint_parser_diagnostics_total";

/// Observer that forwards parser events to the `tracing` and `metrics` facades.
///
/// Constructing this adapter describes its counters but does not install a
/// global tracing subscriber, metrics recorder, or exporter.
#[derive(Debug, Clone, Copy)]
#[non_exhaustive]
pub struct TelemetryObserver;

impl TelemetryObserver {
    /// Construct the optional telemetry adapter.
    #[must_use]
    pub fn new() -> Self {
        describe_counter!(ATTEMPT_METRIC, "Parser and scanner attempts");
        describe_counter!(DIAGNOSTIC_METRIC, "Parser diagnostics emitted");
        Self
    }
}

impl Default for TelemetryObserver {
    fn default() -> Self {
        Self::new()
    }
}

impl ParseObserver for TelemetryObserver {
    fn parse_attempt_started(&self, category: DiagnosticCategory) {
        let category = category.as_str();
        counter!(ATTEMPT_METRIC, "category" => category).increment(1);
        tracing::debug!(
            target: "ddlint::parser",
            category,
            "parser attempt started"
        );
    }

    fn parse_attempt_completed(&self, context: ParseAttemptContext) {
        let category = context.category().as_str();
        let diagnostic_count = context.diagnostic_count();
        tracing::debug!(
            target: "ddlint::parser",
            category,
            diagnostic_count,
            "parser attempt completed"
        );
    }

    fn diagnostic_emitted(&self, context: &DiagnosticContext<'_>) {
        let code = context.code().map_or("uncoded", |code| code.as_str());
        let category = context.category().as_str();
        let severity = context.severity().as_str();
        let span_start = context.span().start;
        let span_end = context.span().end;
        let message = context.message();

        counter!(
            DIAGNOSTIC_METRIC,
            "code" => code,
            "category" => category,
            "severity" => severity
        )
        .increment(1);
        tracing::warn!(
            target: "ddlint::parser",
            code,
            category,
            severity,
            span_start,
            span_end,
            message,
            "parser diagnostic"
        );
    }
}

#[cfg(test)]
mod tests {
    //! Tests for the optional metrics and tracing adapter.

    use std::collections::BTreeMap;

    use metrics_util::debugging::{DebugValue, DebuggingRecorder};

    use super::{ATTEMPT_METRIC, DIAGNOSTIC_METRIC, TelemetryObserver};
    use crate::parser::{
        diagnostics::{DiagnosticCategory, DiagnosticCode},
        observability::{
            DiagnosticContext, DiagnosticSeverity, ParseAttemptContext, ParseObserver,
        },
    };

    #[test]
    fn emits_attempt_and_diagnostic_counters_with_bounded_labels() {
        let recorder = DebuggingRecorder::new();
        let snapshotter = recorder.snapshotter();

        metrics::with_local_recorder(&recorder, || {
            let observer = TelemetryObserver::new();
            observer.parse_attempt_started(DiagnosticCategory::Relation);
            observer
                .parse_attempt_completed(ParseAttemptContext::new(DiagnosticCategory::Relation, 1));
            observer.diagnostic_emitted(&DiagnosticContext {
                code: Some(DiagnosticCode::RelationDuplicateRole),
                category: DiagnosticCategory::Relation,
                span: 6..12,
                severity: DiagnosticSeverity::Error,
                message: "D-REL-002: duplicate role",
            });
        });

        let snapshot = snapshotter.snapshot().into_vec();
        assert_eq!(snapshot.len(), 2);

        let attempt = snapshot
            .iter()
            .find(|(key, _, _, _)| key.key().name() == ATTEMPT_METRIC);
        let Some((attempt_key, _, _, attempt_value)) = attempt else {
            panic!("expected attempt counter");
        };
        assert_eq!(*attempt_value, DebugValue::Counter(1));
        assert_eq!(
            labels(attempt_key),
            BTreeMap::from([("category", "relation")])
        );

        let diagnostic = snapshot
            .iter()
            .find(|(key, _, _, _)| key.key().name() == DIAGNOSTIC_METRIC);
        let Some((diagnostic_key, _, _, diagnostic_value)) = diagnostic else {
            panic!("expected diagnostic counter");
        };
        assert_eq!(*diagnostic_value, DebugValue::Counter(1));
        assert_eq!(
            labels(diagnostic_key),
            BTreeMap::from([
                ("category", "relation"),
                ("code", "D-REL-002"),
                ("severity", "error"),
            ])
        );
    }

    fn labels(key: &metrics_util::CompositeKey) -> BTreeMap<&str, &str> {
        key.key()
            .labels()
            .map(|label| (label.key(), label.value()))
            .collect()
    }
}
