//! Support helper for the `unused-variable` integration tests.

#[path = "lint.rs"]
mod lint;

use ddlint::linter::rules::correctness::UnusedVariableRule;

/// Run the `unused-variable` lint rule on the given source code.
///
/// # Panics
///
/// Panics if the source code fails to parse cleanly.
#[must_use]
pub fn run_unused_variable_rule(source: &str) -> Vec<ddlint::linter::LintDiagnostic> {
    lint::run_rule(UnusedVariableRule, source)
}
