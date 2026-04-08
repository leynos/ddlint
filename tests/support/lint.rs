//! Shared integration-test helpers for running a single CST lint rule.

use ddlint::linter::{CstRule, CstRuleStore, LintDiagnostic, RuleConfig, Runner};

/// Run a single CST rule against the provided source text.
///
/// # Panics
///
/// Panics if the source code fails to parse cleanly.
#[must_use]
pub fn run_rule<R>(rule: R, source: &str) -> Vec<LintDiagnostic>
where
    R: CstRule + 'static,
{
    let parsed = ddlint::parse(source);
    assert!(
        parsed.errors().is_empty(),
        "test source should parse cleanly: {:?}",
        parsed.errors()
    );

    let mut store = CstRuleStore::new();
    store.register(Box::new(rule));
    Runner::new(&store, source, &parsed, RuleConfig::new()).run()
}
