//! Backend-neutral parser observability.
//!
//! The observer contract contains no logging or metrics runtime types.
//! Applications opt in by passing an observer to
//! [`parse_with_observer`](crate::parser::parse_with_observer). Observer return
//! values cannot alter parser control flow.

use std::fmt;

use chumsky::error::{Simple, SimpleReason};

use crate::{Span, SyntaxKind};

use super::diagnostics::{DiagnosticCategory, DiagnosticCode};

#[cfg(feature = "observability")]
mod telemetry;
#[cfg(feature = "observability")]
pub use telemetry::TelemetryObserver;

/// Severity of a parser diagnostic.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum DiagnosticSeverity {
    /// The parser could not accept or validate part of the source.
    Error,
}

impl DiagnosticSeverity {
    /// Return the stable, low-cardinality severity label.
    #[must_use]
    pub const fn as_str(self) -> &'static str {
        match self {
            Self::Error => "error",
        }
    }
}

impl fmt::Display for DiagnosticSeverity {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter.write_str(self.as_str())
    }
}

/// Structured context for one parser diagnostic.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DiagnosticContext<'a> {
    code: Option<DiagnosticCode>,
    category: DiagnosticCategory,
    span: Span,
    severity: DiagnosticSeverity,
    message: &'a str,
}

impl<'a> DiagnosticContext<'a> {
    /// Construct diagnostic context for an observer callback.
    #[must_use]
    pub const fn new(
        code: Option<DiagnosticCode>,
        category: DiagnosticCategory,
        span: Span,
        severity: DiagnosticSeverity,
        message: &'a str,
    ) -> Self {
        Self {
            code,
            category,
            span,
            severity,
            message,
        }
    }

    /// Return the stable diagnostic code, when one is assigned.
    #[must_use]
    pub const fn code(&self) -> Option<DiagnosticCode> {
        self.code
    }

    /// Return the diagnostic's parser category.
    #[must_use]
    pub const fn category(&self) -> DiagnosticCategory {
        self.category
    }

    /// Return the source span associated with the diagnostic.
    #[must_use]
    pub fn span(&self) -> &Span {
        &self.span
    }

    /// Return the diagnostic severity.
    #[must_use]
    pub const fn severity(&self) -> DiagnosticSeverity {
        self.severity
    }

    /// Return the human-facing diagnostic message.
    ///
    /// Message text is structured logging context, not a metrics label.
    #[must_use]
    pub const fn message(&self) -> &'a str {
        self.message
    }
}

/// Completion context for one parser or scanner attempt.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ParseAttemptContext {
    category: DiagnosticCategory,
    diagnostic_count: usize,
}

impl ParseAttemptContext {
    /// Construct completion context for a parser attempt.
    #[must_use]
    pub const fn new(category: DiagnosticCategory, diagnostic_count: usize) -> Self {
        Self {
            category,
            diagnostic_count,
        }
    }

    /// Return the attempted parser category.
    #[must_use]
    pub const fn category(self) -> DiagnosticCategory {
        self.category
    }

    /// Return the number of diagnostics emitted by the attempt.
    #[must_use]
    pub const fn diagnostic_count(self) -> usize {
        self.diagnostic_count
    }
}

/// Observer for deterministic parser attempts and diagnostics.
///
/// Implementations may forward callbacks to any logging or metrics backend.
/// Callback results are deliberately absent, so an observer cannot direct
/// parser recovery or change the returned syntax tree.
pub trait ParseObserver {
    /// Observe the start of a parser or scanner attempt.
    fn parse_attempt_started(&self, _category: DiagnosticCategory) {}

    /// Observe completion of a parser or scanner attempt.
    fn parse_attempt_completed(&self, _context: ParseAttemptContext) {}

    /// Observe one parser diagnostic.
    fn diagnostic_emitted(&self, _context: &DiagnosticContext<'_>) {}
}

/// Observer that discards every callback.
#[derive(Debug, Default, Clone, Copy)]
pub struct NoopParseObserver;

impl ParseObserver for NoopParseObserver {}

pub(crate) fn report_diagnostics(
    observer: &dyn ParseObserver,
    category: DiagnosticCategory,
    errors: &[Simple<SyntaxKind>],
) {
    for error in errors {
        let message = diagnostic_message(error);
        let context = DiagnosticContext::new(
            DiagnosticCode::from_message(message),
            category,
            error.span(),
            DiagnosticSeverity::Error,
            message,
        );
        observer.diagnostic_emitted(&context);
    }
}

pub(crate) fn complete_attempt(
    observer: &dyn ParseObserver,
    category: DiagnosticCategory,
    errors: &[Simple<SyntaxKind>],
) {
    report_diagnostics(observer, category, errors);
    observer.parse_attempt_completed(ParseAttemptContext::new(category, errors.len()));
}

fn diagnostic_message(error: &Simple<SyntaxKind>) -> &str {
    match error.reason() {
        SimpleReason::Custom(message) => message,
        SimpleReason::Unexpected => "unexpected input",
        SimpleReason::Unclosed { .. } => "unclosed delimiter",
    }
}
