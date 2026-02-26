//! Visitor-based parallel rule runner.
//!
//! The [`Runner`] walks a concrete syntax tree (CST) and invokes applicable
//! lint rules from a [`CstRuleStore`], dispatching rule evaluations across a
//! `rayon` thread pool for concurrent execution.
//!
//! Each rule evaluation constructs a thread-local red tree from the shared
//! [`rowan::GreenNode`], so `rowan`'s `!Send` constraint on `SyntaxNode` is
//! respected while still enabling genuine parallelism.

use std::sync::Arc;

use rayon::prelude::*;
use rowan::{GreenNode, NodeOrToken};

use crate::Parsed;
use crate::linter::rule::{CstRule, LintDiagnostic, RuleConfig, RuleCtx};
use crate::linter::store::CstRuleStore;
use crate::parser::ast::Root;

/// Visitor-based parallel rule runner.
///
/// Walks the concrete syntax tree (CST) and invokes applicable lint rules
/// from a [`CstRuleStore`], dispatching rule evaluations across a `rayon`
/// thread pool for concurrent execution.
///
/// # Thread safety
///
/// `Runner` is `Send + Sync`.  Each rule evaluation constructs a
/// thread-local red tree from the shared [`GreenNode`], so `rowan`'s
/// `!Send` constraint on `SyntaxNode` is respected.
///
/// # Deterministic output
///
/// Diagnostics are sorted by `(span.start, span.end, rule_name)` before
/// being returned, ensuring deterministic output regardless of thread
/// scheduling.
///
/// # Examples
///
/// ```rust,no_run
/// use ddlint::linter::{CstRuleStore, RuleConfig, Runner};
/// use ddlint::parse;
///
/// let source = "input relation R(x: u32);";
/// let parsed = parse(source);
/// let store = CstRuleStore::new();
/// let runner = Runner::new(&store, source, &parsed, RuleConfig::new());
/// let diagnostics = runner.run();
/// assert!(diagnostics.is_empty()); // No rules registered.
/// ```
pub struct Runner<'a> {
    store: &'a CstRuleStore,
    green: GreenNode,
    source_text: Arc<str>,
    config: RuleConfig,
}

impl<'a> Runner<'a> {
    /// Create a runner from a rule store, source text, parsed result, and
    /// per-rule configuration.
    ///
    /// The runner clones the [`GreenNode`] from `parsed` internally, so the
    /// caller may continue to use the parse result after construction.
    #[must_use]
    pub fn new(
        store: &'a CstRuleStore,
        source_text: impl Into<Arc<str>>,
        parsed: &Parsed,
        config: RuleConfig,
    ) -> Self {
        Self {
            store,
            green: parsed.green().clone(),
            source_text: source_text.into(),
            config,
        }
    }

    /// Execute all registered rules against the CST in parallel.
    ///
    /// Returns diagnostics sorted by span start, then span end, then rule
    /// name, ensuring deterministic output regardless of thread scheduling.
    #[must_use]
    pub fn run(&self) -> Vec<LintDiagnostic> {
        if self.store.is_empty() {
            return Vec::new();
        }

        let mut diagnostics: Vec<LintDiagnostic> = self
            .store
            .all_rules()
            .par_iter()
            .flat_map_iter(|rule| {
                run_single_rule(rule.as_ref(), &self.green, &self.source_text, &self.config)
            })
            .collect();

        sort_diagnostics(&mut diagnostics);
        diagnostics
    }
}

/// Execute a single rule against the CST rooted at `green`.
///
/// Each invocation constructs a thread-local red tree from the shared green
/// node, enabling safe use from rayon worker threads.
fn run_single_rule(
    rule: &dyn CstRule,
    green: &GreenNode,
    source_text: &Arc<str>,
    config: &RuleConfig,
) -> Vec<LintDiagnostic> {
    let root = Root::from_green(green.clone());
    let ctx = RuleCtx::new(Arc::clone(source_text), root.clone(), config.clone());
    let target_kinds = rule.target_kinds();
    let mut diagnostics = Vec::new();

    for element in root.syntax().descendants_with_tokens() {
        if target_kinds.contains(&element.kind()) {
            match &element {
                NodeOrToken::Node(node) => {
                    rule.check_node(node, &ctx, &mut diagnostics);
                }
                NodeOrToken::Token(token) => {
                    rule.check_token(token, &ctx, &mut diagnostics);
                }
            }
        }
    }

    diagnostics
}

/// Sort diagnostics deterministically by span position then rule name.
fn sort_diagnostics(diagnostics: &mut [LintDiagnostic]) {
    diagnostics.sort_by(|a, b| {
        a.span()
            .start()
            .cmp(&b.span().start())
            .then_with(|| a.span().end().cmp(&b.span().end()))
            .then_with(|| a.rule_name().cmp(b.rule_name()))
    });
}

#[cfg(test)]
mod tests {
    use rowan::TextRange;
    use rstest::{fixture, rstest};

    use super::*;
    use crate::linter::Rule;
    use crate::{SyntaxKind, parse};

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

        fn check_node(
            &self,
            node: &rowan::SyntaxNode<crate::DdlogLanguage>,
            _ctx: &RuleCtx,
            diagnostics: &mut Vec<LintDiagnostic>,
        ) {
            diagnostics.push(LintDiagnostic::new(
                self.name(),
                "node hit",
                node.text_range(),
            ));
        }

        fn check_token(
            &self,
            token: &rowan::SyntaxToken<crate::DdlogLanguage>,
            _ctx: &RuleCtx,
            diagnostics: &mut Vec<LintDiagnostic>,
        ) {
            diagnostics.push(LintDiagnostic::new(
                self.name(),
                "token hit",
                token.text_range(),
            ));
        }
    }

    fn stub(name: &'static str, kinds: &'static [SyntaxKind]) -> Box<dyn CstRule> {
        Box::new(StubRule { name, kinds })
    }

    // -- fixtures -------------------------------------------------------------

    #[fixture]
    fn store() -> CstRuleStore {
        CstRuleStore::new()
    }

    // -- tests ----------------------------------------------------------------

    fn assert_send_sync<T: Send + Sync>() {}

    #[test]
    fn runner_is_send_and_sync() {
        assert_send_sync::<Runner<'_>>();
    }

    #[rstest]
    fn empty_store_produces_no_diagnostics(store: CstRuleStore) {
        let source = "input relation R(x: u32);";
        let parsed = parse(source);
        let runner = Runner::new(&store, source, &parsed, RuleConfig::new());
        let diagnostics = runner.run();
        assert!(diagnostics.is_empty());
    }

    #[rstest]
    fn single_rule_produces_diagnostics(mut store: CstRuleStore) {
        store.register(stub("alpha", &[SyntaxKind::N_RELATION_DECL]));

        let source = "input relation R(x: u32);";
        let parsed = parse(source);
        let runner = Runner::new(&store, source, &parsed, RuleConfig::new());
        let diagnostics = runner.run();

        assert!(
            !diagnostics.is_empty(),
            "a rule targeting N_RELATION_DECL should produce diagnostics",
        );
        assert!(
            diagnostics.iter().all(|d| d.rule_name() == "alpha"),
            "all diagnostics should be attributed to 'alpha'",
        );
    }

    #[rstest]
    fn diagnostics_are_sorted_by_span(mut store: CstRuleStore) {
        // Two rules targeting the same kind produce diagnostics at the same
        // span; the sort tiebreaker is rule name.
        store.register(stub("beta", &[SyntaxKind::N_RELATION_DECL]));
        store.register(stub("alpha", &[SyntaxKind::N_RELATION_DECL]));

        let source = "input relation R(x: u32);";
        let parsed = parse(source);
        let runner = Runner::new(&store, source, &parsed, RuleConfig::new());
        let diagnostics = runner.run();

        let names: Vec<&str> = diagnostics.iter().map(LintDiagnostic::rule_name).collect();
        // Both target N_RELATION_DECL; "alpha" sorts before "beta".
        assert_eq!(names, vec!["alpha", "beta"]);
    }

    #[rstest]
    fn multiple_rules_produce_merged_diagnostics(mut store: CstRuleStore) {
        store.register(stub("node-rule", &[SyntaxKind::N_RELATION_DECL]));
        store.register(stub("token-rule", &[SyntaxKind::K_RELATION]));

        let source = "input relation R(x: u32);";
        let parsed = parse(source);
        let runner = Runner::new(&store, source, &parsed, RuleConfig::new());
        let diagnostics = runner.run();

        let node_hits = diagnostics
            .iter()
            .filter(|d| d.rule_name() == "node-rule")
            .count();
        let token_hits = diagnostics
            .iter()
            .filter(|d| d.rule_name() == "token-rule")
            .count();

        assert!(node_hits > 0, "node-rule should produce diagnostics");
        assert!(token_hits > 0, "token-rule should produce diagnostics");
        assert_eq!(diagnostics.len(), node_hits + token_hits);
    }

    #[test]
    fn sort_diagnostics_orders_by_start_then_end_then_name() {
        let span_a = TextRange::new(0.into(), 5.into());
        let span_b = TextRange::new(0.into(), 10.into());
        let span_c = TextRange::new(5.into(), 10.into());

        let mut diagnostics = vec![
            LintDiagnostic::new("z-rule", "msg", span_c),
            LintDiagnostic::new("b-rule", "msg", span_a),
            LintDiagnostic::new("a-rule", "msg", span_a),
            LintDiagnostic::new("a-rule", "msg", span_b),
        ];

        sort_diagnostics(&mut diagnostics);

        let result: Vec<(&str, u32, u32)> = diagnostics
            .iter()
            .map(|d| {
                (
                    d.rule_name(),
                    u32::from(d.span().start()),
                    u32::from(d.span().end()),
                )
            })
            .collect();

        assert_eq!(
            result,
            vec![
                ("a-rule", 0, 5),
                ("b-rule", 0, 5),
                ("a-rule", 0, 10),
                ("z-rule", 5, 10),
            ]
        );
    }
}
