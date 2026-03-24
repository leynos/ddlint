//! Shared test utilities for behavioral tests.

use ddlint::linter::rules::correctness::UnusedRelationRule;
use ddlint::linter::{CstRuleStore, RuleConfig, Runner};
use ddlint::parse;

/// Run the `unused-relation` lint rule on the given source code.
///
/// # Panics
///
/// Panics if the source code fails to parse cleanly.
#[must_use]
pub fn run_unused_relation_rule(source: &str) -> Vec<ddlint::linter::LintDiagnostic> {
    let parsed = parse(source);
    assert!(
        parsed.errors().is_empty(),
        "unused-relation test source should parse cleanly: {:?}",
        parsed.errors()
    );

    let mut store = CstRuleStore::new();
    store.register(Box::new(UnusedRelationRule));
    Runner::new(&store, source, &parsed, RuleConfig::new()).run()
}
