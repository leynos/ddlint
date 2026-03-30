//! Shared runner setup for `unused-variable` behavioural tests.

use ddlint::linter::rules::correctness::UnusedVariableRule;
use ddlint::linter::{CstRuleStore, RuleConfig, Runner};
use ddlint::parse;

/// Run the `unused-variable` lint rule on the given source code.
///
/// # Panics
///
/// Panics if the source code fails to parse cleanly.
#[must_use]
pub fn run_unused_variable_rule(source: &str) -> Vec<ddlint::linter::LintDiagnostic> {
    let parsed = parse(source);
    assert!(
        parsed.errors().is_empty(),
        "unused-variable test source should parse cleanly: {:?}",
        parsed.errors()
    );

    let mut store = CstRuleStore::new();
    store.register(Box::new(UnusedVariableRule));
    Runner::new(&store, source, &parsed, RuleConfig::new()).run()
}
