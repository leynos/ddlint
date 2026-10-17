//! Backend-neutral parser observability.
//!
//! The observer contract contains no logging or metrics runtime types.
//! Applications opt in by passing an observer to
//! [`parse_with_observer`](crate::parser::parse_with_observer). Observer return
//! values cannot alter parser control flow.

use std::fmt;

use chumsky::error::{Simple, SimpleReason};

use crate::{Span, SyntaxKind, parser::lexer_helpers::token_display};

use super::diagnostics::{DiagnosticCategory, DiagnosticCode};

#[cfg(feature = "observability")]
mod telemetry;
#[cfg(feature = "observability")]
pub use telemetry::TelemetryObserver;

/// Severity of a parser diagnostic.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[non_exhaustive]
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

    /// Return the number of diagnostics associated with the completed attempt.
    ///
    /// For the top-level `Parser` category this is an aggregate count from all
    /// parser phases. Consumers should not add it to the per-phase counts.
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
    /// Return whether diagnostic callbacks are needed.
    ///
    /// The default reports every diagnostic. An observer that does not need
    /// diagnostic context may return `false` to skip message formatting; parse
    /// attempt callbacks remain enabled.
    fn should_report_diagnostics(&self) -> bool {
        true
    }

    /// Observe the start of a parser or scanner attempt.
    fn parse_attempt_started(&self, _category: DiagnosticCategory) {}

    /// Observe completion of a parser or scanner attempt.
    ///
    /// The top-level `Parser` completion summarizes all final diagnostics,
    /// including reserved-token completion. Child diagnostics are not emitted
    /// again; newly completed diagnostics are reported before completion.
    fn parse_attempt_completed(&self, _context: ParseAttemptContext) {}

    /// Observe one parser diagnostic.
    fn diagnostic_emitted(&self, _context: &DiagnosticContext<'_>) {}
}

/// Observer that discards every callback.
#[derive(Debug, Default, Clone, Copy)]
pub struct NoopParseObserver;

impl ParseObserver for NoopParseObserver {
    fn should_report_diagnostics(&self) -> bool {
        false
    }
}

pub(crate) fn report_diagnostics(
    observer: &dyn ParseObserver,
    category: DiagnosticCategory,
    errors: &[Simple<SyntaxKind>],
) {
    if !observer.should_report_diagnostics() {
        return;
    }

    for error in errors {
        let message = diagnostic_message(error);
        let context = DiagnosticContext {
            code: DiagnosticCode::from_message(&message),
            category,
            span: error.span(),
            severity: DiagnosticSeverity::Error,
            message: &message,
        };
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

fn diagnostic_message(error: &Simple<SyntaxKind>) -> String {
    match error.reason() {
        SimpleReason::Custom(message) => message.clone(),
        SimpleReason::Unexpected => {
            let found = error
                .found()
                .map_or_else(|| "end of input".to_owned(), |kind| display_kind(*kind));
            let mut expected: Vec<String> = error
                .expected()
                .map(|kind| {
                    kind.as_ref()
                        .map_or_else(|| "end of input".to_owned(), |kind| display_kind(*kind))
                })
                .collect();
            expected.sort();

            if expected.is_empty() {
                format!("unexpected {found}")
            } else {
                format!("expected {} but found {found}", expected.join(" or "))
            }
        }
        SimpleReason::Unclosed { delimiter, .. } => {
            format!("unclosed {}", display_kind(*delimiter))
        }
    }
}

fn display_kind(kind: SyntaxKind) -> String {
    match token_display(kind) {
        "" => format!("{kind:?}"),
        symbol => format!("`{symbol}`"),
    }
}

#[cfg(test)]
mod tests {
    //! Tests for detailed, deterministic observer messages.

    use chumsky::error::Error;

    use super::diagnostic_message;
    use crate::SyntaxKind;

    #[test]
    fn unexpected_message_includes_sorted_expected_and_found_tokens() {
        let error = chumsky::error::Simple::<SyntaxKind>::expected_input_found(
            4..5,
            [Some(SyntaxKind::T_IDENT), Some(SyntaxKind::T_LPAREN)],
            Some(SyntaxKind::T_RPAREN),
        );

        assert_eq!(
            diagnostic_message(&error),
            "expected T_IDENT or `(` but found `)`"
        );
    }

    #[test]
    fn unclosed_message_includes_the_delimiter_kind() {
        let error = chumsky::error::Simple::<SyntaxKind>::unclosed_delimiter(
            0..1,
            SyntaxKind::T_LBRACKET,
            4..5,
            SyntaxKind::T_RBRACKET,
            Some(SyntaxKind::T_IDENT),
        );

        assert_eq!(diagnostic_message(&error), "unclosed `[`");
    }
}
