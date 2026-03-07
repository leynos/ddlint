//! Behavioural tests for `declare_lint!` rules running through the linter core.

use rowan::NodeOrToken;
use rstest::{fixture, rstest};

use ddlint::linter::{
    CstRule, CstRuleStore, LintDiagnostic, Rule, RuleConfig, RuleCtx, RuleLevel, Runner,
};
use ddlint::{SyntaxKind, parse};

ddlint::declare_lint! {
    /// Emits diagnostics for every relation declaration node.
    pub MacroNodeRule {
        name: "macro-node-rule",
        group: "correctness",
        level: warn,
        target_kinds: [SyntaxKind::N_RELATION_DECL],
        fn check_node(&self, node, _ctx, diagnostics) {
            diagnostics.push(LintDiagnostic::new(
                self.name(),
                "macro node hit",
                node.text_range(),
            ));
        }
    }
}

ddlint::declare_lint! {
    /// Emits diagnostics for every identifier token.
    pub MacroTokenRule {
        name: "macro-token-rule",
        group: "style",
        level: hint,
        target_kinds: [SyntaxKind::T_IDENT],
        fn check_token(&self, token, _ctx, diagnostics) {
            diagnostics.push(LintDiagnostic::new(
                self.name(),
                "macro token hit",
                token.text_range(),
            ));
        }
    }
}

ddlint::declare_lint! {
    /// Emits diagnostics for both nodes and tokens.
    pub MacroDualRule {
        name: "macro-dual-rule",
        group: "test",
        level: error,
        target_kinds: [SyntaxKind::N_RELATION_DECL, SyntaxKind::K_RELATION],
        fn check_node(&self, node, _ctx, diagnostics) {
            diagnostics.push(LintDiagnostic::new(
                self.name(),
                "macro node hit",
                node.text_range(),
            ));
        },
        fn check_token(&self, token, _ctx, diagnostics) {
            diagnostics.push(LintDiagnostic::new(
                self.name(),
                "macro token hit",
                token.text_range(),
            ));
        }
    }
}

fn sequential_dispatch(source: &str, rule: &dyn CstRule) -> Vec<LintDiagnostic> {
    let parsed = parse(source);
    assert!(parsed.errors().is_empty());
    let ctx = RuleCtx::from_parsed(source, &parsed, RuleConfig::new());
    let mut diagnostics = Vec::new();

    for element in parsed.root().syntax().descendants_with_tokens() {
        if rule.target_kinds().contains(&element.kind()) {
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

    diagnostics.sort_by(|a, b| {
        a.span()
            .start()
            .cmp(&b.span().start())
            .then_with(|| a.span().end().cmp(&b.span().end()))
            .then_with(|| a.rule_name().cmp(b.rule_name()))
    });
    diagnostics
}

#[fixture]
fn source() -> &'static str {
    include_str!("../examples/hello_join.dl")
}

#[rstest]
fn macro_generated_node_rule_registers_and_runs(source: &str) {
    let mut store = CstRuleStore::new();
    store.register(Box::new(MacroNodeRule));

    let parsed = parse(source);
    assert!(parsed.errors().is_empty());

    let runner = Runner::new(&store, source, &parsed, RuleConfig::new());
    let diagnostics = runner.run();

    assert!(!diagnostics.is_empty());
    assert!(
        diagnostics
            .iter()
            .all(|d| d.rule_name() == "macro-node-rule")
    );
}

#[rstest]
fn macro_generated_token_rule_runs_end_to_end(source: &str) {
    let mut store = CstRuleStore::new();
    store.register(Box::new(MacroTokenRule));

    let parsed = parse(source);
    assert!(parsed.errors().is_empty());

    let runner = Runner::new(&store, source, &parsed, RuleConfig::new());
    let diagnostics = runner.run();

    assert!(!diagnostics.is_empty());
    assert!(
        diagnostics
            .iter()
            .all(|d| d.rule_name() == "macro-token-rule")
    );
}

#[rstest]
fn macro_generated_rule_matches_sequential_dispatch(source: &str) {
    let mut store = CstRuleStore::new();
    store.register(Box::new(MacroDualRule));

    let parsed = parse(source);
    assert!(parsed.errors().is_empty());

    let expected = sequential_dispatch(source, &MacroDualRule);
    let runner = Runner::new(&store, source, &parsed, RuleConfig::new());
    let actual = runner.run();

    assert_eq!(actual, expected);
}

#[rstest]
fn macro_generated_runner_output_is_deterministic(source: &str) {
    let parsed = parse(source);
    assert!(parsed.errors().is_empty());

    let mut store = CstRuleStore::new();
    store.register(Box::new(MacroDualRule));

    let first = Runner::new(&store, source, &parsed, RuleConfig::new()).run();
    let second = Runner::new(&store, source, &parsed, RuleConfig::new()).run();
    let third = Runner::new(&store, source, &parsed, RuleConfig::new()).run();

    assert_eq!(first, second);
    assert_eq!(second, third);
}

#[rstest]
fn macro_generated_metadata_is_visible_through_trait_object() {
    let rule: &dyn Rule = &MacroDualRule;
    assert_eq!(rule.name(), "macro-dual-rule");
    assert_eq!(rule.group(), "test");
    assert_eq!(rule.default_level(), RuleLevel::Error);
    assert!(
        rule.docs()
            .contains("Emits diagnostics for both nodes and tokens.")
    );
}
