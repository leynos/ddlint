//! Support helper for the `unused-relation` integration tests.

#[path = "lint.rs"]
mod lint;

use ddlint::linter::rules::correctness::UnusedRelationRule;

/// Run the `unused-relation` lint rule on the given source code.
///
/// # Panics
///
/// Panics if the source code fails to parse cleanly.
#[must_use]
pub fn run_unused_relation_rule(source: &str) -> Vec<ddlint::linter::LintDiagnostic> {
    lint::run_rule(UnusedRelationRule, source)
}
