//! CST rule registry for syntax-kind-based dispatch.
//!
//! [`CstRuleStore`] maps [`SyntaxKind`] values to the [`CstRule`]
//! implementations that have registered interest in them.  The rule runner
//! queries the store during CST traversal to identify which rules apply to
//! each node or token.

use std::collections::HashMap;
use std::sync::Arc;

use crate::SyntaxKind;
use crate::linter::CstRule;

/// Central registry mapping syntax kinds to their interested lint rules.
///
/// Rules are registered via [`Self::register`], which reads each rule's
/// [`CstRule::target_kinds`] to build an internal dispatch index.  During
/// traversal the runner calls [`Self::rules_for_kind`] to obtain the
/// applicable rules for a given [`SyntaxKind`] in O(1) time.
///
/// # Thread safety
///
/// `CstRuleStore` is `Send + Sync`, so it can be shared across threads
/// (for example, via a shared reference in a `rayon` parallel iterator).
///
/// # Examples
///
/// ```rust,no_run
/// use ddlint::linter::{CstRuleStore, CstRule, Rule, LintDiagnostic, RuleCtx};
/// use ddlint::SyntaxKind;
///
/// struct MyRule;
///
/// impl Rule for MyRule {
///     fn name(&self) -> &'static str { "my-rule" }
///     fn group(&self) -> &'static str { "correctness" }
///     fn docs(&self) -> &'static str { "Example rule." }
/// }
///
/// impl CstRule for MyRule {
///     fn target_kinds(&self) -> &'static [SyntaxKind] {
///         &[SyntaxKind::N_RULE]
///     }
/// }
///
/// let mut store = CstRuleStore::new();
/// store.register(Box::new(MyRule));
///
/// assert_eq!(store.rules_for_kind(SyntaxKind::N_RULE).len(), 1);
/// assert!(store.rules_for_kind(SyntaxKind::T_COMMA).is_empty());
/// ```
pub struct CstRuleStore {
    /// All registered rules in insertion order.
    rules: Vec<Arc<dyn CstRule>>,
    /// Dispatch index from syntax kind to interested rules.
    by_kind: HashMap<SyntaxKind, Vec<Arc<dyn CstRule>>>,
}

impl CstRuleStore {
    /// Create an empty rule store.
    #[must_use]
    pub fn new() -> Self {
        Self {
            rules: Vec::new(),
            by_kind: HashMap::new(),
        }
    }

    /// Register a rule, indexing it under each of its target syntax kinds.
    ///
    /// The rule is wrapped in [`Arc`] internally so it can be shared across
    /// multiple kind entries without duplication.  Returns `&mut Self` for
    /// chaining.
    pub fn register(&mut self, rule: Box<dyn CstRule>) -> &mut Self {
        let rule: Arc<dyn CstRule> = Arc::from(rule);
        for &kind in rule.target_kinds() {
            self.by_kind
                .entry(kind)
                .or_default()
                .push(Arc::clone(&rule));
        }
        self.rules.push(rule);
        self
    }

    /// Return all rules registered for the given syntax kind.
    ///
    /// Returns an empty slice when no rules target the given kind.
    #[must_use]
    pub fn rules_for_kind(&self, kind: SyntaxKind) -> &[Arc<dyn CstRule>] {
        self.by_kind.get(&kind).map_or(&[], Vec::as_slice)
    }

    /// Return all registered rules in insertion order.
    #[must_use]
    pub fn all_rules(&self) -> &[Arc<dyn CstRule>] {
        &self.rules
    }

    /// Return the total number of registered rules.
    #[must_use]
    pub fn len(&self) -> usize {
        self.rules.len()
    }

    /// Return `true` when no rules have been registered.
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.rules.is_empty()
    }
}

impl Default for CstRuleStore {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::linter::Rule;

    // -- stub rules -----------------------------------------------------------

    struct StubRule {
        name: &'static str,
        kinds: &'static [SyntaxKind],
    }

    impl Rule for StubRule {
        fn name(&self) -> &'static str {
            self.name
        }

        fn group(&self) -> &'static str {
            "test"
        }

        fn docs(&self) -> &'static str {
            "Stub rule for testing."
        }
    }

    impl CstRule for StubRule {
        fn target_kinds(&self) -> &'static [SyntaxKind] {
            self.kinds
        }
    }

    // -- helpers --------------------------------------------------------------

    fn assert_send_sync<T: Send + Sync>() {}

    fn stub(name: &'static str, kinds: &'static [SyntaxKind]) -> Box<dyn CstRule> {
        Box::new(StubRule { name, kinds })
    }

    // -- tests ----------------------------------------------------------------

    #[test]
    fn new_store_is_empty() {
        let store = CstRuleStore::new();

        assert_eq!(store.len(), 0);
        assert!(store.is_empty());
        assert!(store.all_rules().is_empty());
    }

    #[test]
    fn register_single_rule() {
        let mut store = CstRuleStore::new();
        store.register(stub("alpha", &[SyntaxKind::N_RULE]));

        assert_eq!(store.len(), 1);
        assert!(!store.is_empty());
        assert_eq!(store.all_rules().len(), 1);
        assert_eq!(store.rules_for_kind(SyntaxKind::N_RULE).len(), 1);
    }

    #[test]
    fn register_rule_with_multiple_targets() {
        let mut store = CstRuleStore::new();
        store.register(stub(
            "multi",
            &[SyntaxKind::N_RELATION_DECL, SyntaxKind::K_RELATION],
        ));

        // Appears under both kinds.
        assert_eq!(store.rules_for_kind(SyntaxKind::N_RELATION_DECL).len(), 1);
        assert_eq!(store.rules_for_kind(SyntaxKind::K_RELATION).len(), 1);

        // Only one canonical entry.
        assert_eq!(store.all_rules().len(), 1);
    }

    #[test]
    fn multiple_rules_same_kind() {
        let mut store = CstRuleStore::new();
        store.register(stub("alpha", &[SyntaxKind::N_RULE]));
        store.register(stub("beta", &[SyntaxKind::N_RULE]));

        assert_eq!(store.rules_for_kind(SyntaxKind::N_RULE).len(), 2);
        assert_eq!(store.len(), 2);
    }

    #[test]
    fn rules_for_unregistered_kind_returns_empty() {
        let mut store = CstRuleStore::new();
        store.register(stub("alpha", &[SyntaxKind::N_RULE]));

        assert!(store.rules_for_kind(SyntaxKind::T_COMMA).is_empty());
    }

    #[test]
    fn rule_with_empty_target_kinds() {
        let mut store = CstRuleStore::new();
        store.register(stub("empty", &[]));

        // Present in all_rules but not in any kind entry.
        assert_eq!(store.all_rules().len(), 1);
        assert!(store.rules_for_kind(SyntaxKind::N_RULE).is_empty());
        assert!(store.rules_for_kind(SyntaxKind::T_COMMA).is_empty());
    }

    #[test]
    fn register_chaining_works() {
        let mut store = CstRuleStore::new();
        store
            .register(stub("alpha", &[SyntaxKind::N_RULE]))
            .register(stub("beta", &[SyntaxKind::K_RELATION]));

        assert_eq!(store.len(), 2);
    }

    #[test]
    fn store_is_send_and_sync() {
        assert_send_sync::<CstRuleStore>();
    }

    #[test]
    fn rules_for_kind_preserves_insertion_order() {
        let mut store = CstRuleStore::new();
        store.register(stub("alpha", &[SyntaxKind::N_RULE]));
        store.register(stub("beta", &[SyntaxKind::N_RULE]));
        store.register(stub("gamma", &[SyntaxKind::N_RULE]));

        let names: Vec<&str> = store
            .rules_for_kind(SyntaxKind::N_RULE)
            .iter()
            .map(|r| r.name())
            .collect();

        assert_eq!(names, vec!["alpha", "beta", "gamma"]);
    }

    #[test]
    fn rule_metadata_accessible_through_store() {
        let mut store = CstRuleStore::new();
        store.register(stub("alpha", &[SyntaxKind::N_RULE]));

        let names: Vec<&str> = store
            .rules_for_kind(SyntaxKind::N_RULE)
            .iter()
            .map(|r| r.name())
            .collect();
        let groups: Vec<&str> = store
            .rules_for_kind(SyntaxKind::N_RULE)
            .iter()
            .map(|r| r.group())
            .collect();
        let docs: Vec<&str> = store
            .rules_for_kind(SyntaxKind::N_RULE)
            .iter()
            .map(|r| r.docs())
            .collect();

        assert_eq!(names, vec!["alpha"]);
        assert_eq!(groups, vec!["test"]);
        assert_eq!(docs, vec!["Stub rule for testing."]);
    }

    #[test]
    fn default_creates_empty_store() {
        let store = CstRuleStore::default();

        assert!(store.is_empty());
        assert_eq!(store.len(), 0);
    }
}
