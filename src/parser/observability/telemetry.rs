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
        tracing::error!(
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
