//! Recording parser observer for integration tests.

use std::cell::RefCell;

use ddlint::{
    DiagnosticCategory, DiagnosticCode, DiagnosticContext, DiagnosticSeverity, ParseAttemptContext,
    ParseObserver, Span,
};

/// Owned representation of an observer callback.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RecordedParseEvent {
    /// A parser category started an attempt.
    AttemptStarted(DiagnosticCategory),
    /// A parser category completed an attempt.
    AttemptCompleted {
        /// Category that completed.
        category: DiagnosticCategory,
        /// Diagnostics emitted during the attempt.
        diagnostic_count: usize,
    },
    /// The parser emitted a diagnostic.
    Diagnostic {
        /// Stable diagnostic code, when assigned.
        code: Option<DiagnosticCode>,
        /// Parser category that emitted the diagnostic.
        category: DiagnosticCategory,
        /// Diagnostic source span.
        span: Span,
        /// Diagnostic severity.
        severity: DiagnosticSeverity,
        /// Human-facing diagnostic message.
        message: String,
    },
}

/// Observer that records callbacks in emission order.
#[derive(Debug, Default)]
pub struct RecordingParseObserver {
    events: RefCell<Vec<RecordedParseEvent>>,
}

impl RecordingParseObserver {
    /// Return an owned snapshot of all recorded events.
    pub fn events(&self) -> Vec<RecordedParseEvent> {
        self.events.borrow().clone()
    }
}

impl ParseObserver for RecordingParseObserver {
    fn parse_attempt_started(&self, category: DiagnosticCategory) {
        self.events
            .borrow_mut()
            .push(RecordedParseEvent::AttemptStarted(category));
    }

    fn parse_attempt_completed(&self, context: ParseAttemptContext) {
        self.events
            .borrow_mut()
            .push(RecordedParseEvent::AttemptCompleted {
                category: context.category(),
                diagnostic_count: context.diagnostic_count(),
            });
    }

    fn diagnostic_emitted(&self, context: &DiagnosticContext<'_>) {
        self.events
            .borrow_mut()
            .push(RecordedParseEvent::Diagnostic {
                code: context.code(),
                category: context.category(),
                span: context.span().clone(),
                severity: context.severity(),
                message: context.message().to_owned(),
            });
    }
}
