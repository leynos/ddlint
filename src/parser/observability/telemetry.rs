//! Optional `tracing` and `metrics` observer adapter.

use metrics::{counter, describe_counter};

use super::{DiagnosticContext, ParseAttemptContext, ParseObserver};
use crate::parser::diagnostics::DiagnosticCategory;

const ATTEMPT_METRIC: &str = "ddlint_parser_attempts_total";
const DIAGNOSTIC_METRIC: &str = "ddlint_parser_diagnostics_total";

/// Observer that forwards parser events to the `tracing` and `metrics` facades.
///
/// Constructing this adapter describes its counters but does not install a
/// global tracing subscriber, metrics recorder, or exporter. Construct it once
/// after the host configures its metrics recorder, then reuse it for parses.
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
    use std::fmt;
    use std::sync::{Arc, Mutex};

    use metrics_util::debugging::{DebugValue, DebuggingRecorder};
    use tracing::{
        Event, Metadata, Subscriber,
        field::{Field, Visit},
        span::{Attributes, Id, Record},
    };

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
        let (attempt_key, _, _, attempt_value) = attempt.expect("expected attempt counter");
        assert_eq!(*attempt_value, DebugValue::Counter(1));
        assert_eq!(
            labels(attempt_key),
            BTreeMap::from([("category", "relation")])
        );

        let diagnostic = snapshot
            .iter()
            .find(|(key, _, _, _)| key.key().name() == DIAGNOSTIC_METRIC);
        let (diagnostic_key, _, _, diagnostic_value) =
            diagnostic.expect("expected diagnostic counter");
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

    #[test]
    fn labels_diagnostics_without_a_code_as_uncoded() {
        let recorder = DebuggingRecorder::new();
        let snapshotter = recorder.snapshotter();

        metrics::with_local_recorder(&recorder, || {
            let observer = TelemetryObserver::new();
            observer.diagnostic_emitted(&DiagnosticContext {
                code: None,
                category: DiagnosticCategory::Rule,
                span: 4..9,
                severity: DiagnosticSeverity::Error,
                message: "rule diagnostic without a stable code",
            });
        });

        let snapshot = snapshotter.snapshot().into_vec();
        let [(key, _, _, value)] = snapshot.as_slice() else {
            panic!("expected one uncoded diagnostic counter, got {snapshot:?}");
        };

        assert_eq!(key.key().name(), DIAGNOSTIC_METRIC);
        assert_eq!(*value, DebugValue::Counter(1));
        assert_eq!(
            labels(key),
            BTreeMap::from([
                ("category", "rule"),
                ("code", "uncoded"),
                ("severity", "error"),
            ])
        );
    }

    #[test]
    fn emits_structured_attempt_tracing_fields() {
        let observer = TelemetryObserver::new();
        let subscriber = RecordingSubscriber::default();
        let events = Arc::clone(&subscriber.events);
        let dispatch = tracing::Dispatch::new(subscriber);

        tracing::dispatcher::with_default(&dispatch, || {
            observer.parse_attempt_started(DiagnosticCategory::Relation);
            observer
                .parse_attempt_completed(ParseAttemptContext::new(DiagnosticCategory::Relation, 2));
        });

        let events = events
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        assert_eq!(events.len(), 2);
        assert_eq!(
            events
                .iter()
                .map(|event| event.fields.get("message").map(String::as_str))
                .collect::<Vec<_>>(),
            [
                Some("parser attempt started"),
                Some("parser attempt completed")
            ]
        );

        let [started, completed] = events.as_slice() else {
            panic!("expected start and completion tracing events, got {events:?}");
        };
        for event in [started, completed] {
            assert_eq!(event.target, "ddlint::parser");
            assert_eq!(event.level, "DEBUG");
            assert_eq!(
                event.fields.get("category").map(String::as_str),
                Some("relation")
            );
        }
        assert!(!started.fields.contains_key("diagnostic_count"));
        assert_eq!(
            completed.fields.get("diagnostic_count").map(String::as_str),
            Some("2")
        );
    }

    #[test]
    fn emits_structured_diagnostic_tracing_fields() {
        let observer = TelemetryObserver::new();
        let subscriber = RecordingSubscriber::default();
        let events = Arc::clone(&subscriber.events);
        let dispatch = tracing::Dispatch::new(subscriber);

        tracing::dispatcher::with_default(&dispatch, || {
            observer.diagnostic_emitted(&DiagnosticContext {
                code: Some(DiagnosticCode::RelationDuplicateRole),
                category: DiagnosticCategory::Relation,
                span: 6..12,
                severity: DiagnosticSeverity::Error,
                message: "D-REL-002: duplicate role",
            });
        });

        let events = events
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        assert_eq!(events.len(), 1);

        for event in events.iter() {
            assert_eq!(event.target, "ddlint::parser");
            assert_eq!(event.level, "WARN");
            assert_eq!(
                event.fields.get("code").map(String::as_str),
                Some("D-REL-002")
            );
            assert_eq!(
                event.fields.get("category").map(String::as_str),
                Some("relation")
            );
            assert_eq!(
                event.fields.get("severity").map(String::as_str),
                Some("error")
            );
            assert_eq!(
                event.fields.get("span_start").map(String::as_str),
                Some("6")
            );
            assert_eq!(event.fields.get("span_end").map(String::as_str), Some("12"));
            assert_eq!(
                event.fields.get("message").map(String::as_str),
                Some("D-REL-002: duplicate role")
            );
        }
    }

    #[derive(Clone, Default)]
    struct RecordingSubscriber {
        events: Arc<Mutex<Vec<RecordedEvent>>>,
    }

    impl Subscriber for RecordingSubscriber {
        fn enabled(&self, _: &Metadata<'_>) -> bool {
            true
        }

        fn new_span(&self, _: &Attributes<'_>) -> Id {
            Id::from_u64(1)
        }

        fn record(&self, _: &Id, _: &Record<'_>) {}

        fn record_follows_from(&self, _: &Id, _: &Id) {}

        fn event(&self, event: &Event<'_>) {
            let mut fields = BTreeMap::new();
            event.record(&mut RecordingVisitor(&mut fields));

            self.events
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner)
                .push(RecordedEvent {
                    target: event.metadata().target().to_owned(),
                    level: event.metadata().level().to_string(),
                    fields,
                });
        }

        fn enter(&self, _: &Id) {}

        fn exit(&self, _: &Id) {}
    }

    #[derive(Debug, PartialEq, Eq)]
    struct RecordedEvent {
        target: String,
        level: String,
        fields: BTreeMap<String, String>,
    }

    struct RecordingVisitor<'a>(&'a mut BTreeMap<String, String>);

    impl Visit for RecordingVisitor<'_> {
        fn record_str(&mut self, field: &Field, value: &str) {
            self.0.insert(field.name().to_owned(), value.to_owned());
        }

        fn record_debug(&mut self, field: &Field, value: &dyn fmt::Debug) {
            self.0.insert(field.name().to_owned(), format!("{value:?}"));
        }
    }

    fn labels(key: &metrics_util::CompositeKey) -> BTreeMap<&str, &str> {
        key.key()
            .labels()
            .map(|label| (label.key(), label.value()))
            .collect()
    }
}
