//! Behavioural tests for `CstRuleStore` dispatch over parsed CST input.

use rowan::NodeOrToken;
use rstest::{fixture, rstest};

use ddlint::linter::{
    CstRule, CstRuleStore, LintDiagnostic, Rule, RuleConfig, RuleConfigValue, RuleCtx,
};
use ddlint::{Parsed, SyntaxKind, parse};

// -- test rules ---------------------------------------------------------------

const NODE_HIT: &str = "node hit";
const TOKEN_HIT: &str = "token hit";

struct CountingRule;

impl CountingRule {
    fn push_diagnostic(
        &self,
        message: &'static str,
        range: rowan::TextRange,
        diagnostics: &mut Vec<LintDiagnostic>,
    ) {
        diagnostics.push(LintDiagnostic::new(self.name(), message, range));
    }
}

impl Rule for CountingRule {
    fn name(&self) -> &'static str {
        "counting-rule"
    }

    fn group(&self) -> &'static str {
        "correctness"
    }

    fn docs(&self) -> &'static str {
        "Counts matching node and token visits."
    }
}

impl CstRule for CountingRule {
    fn target_kinds(&self) -> &'static [SyntaxKind] {
        &[SyntaxKind::N_RELATION_DECL, SyntaxKind::K_RELATION]
    }

    fn check_node(
        &self,
        node: &rowan::SyntaxNode<ddlint::DdlogLanguage>,
        _ctx: &RuleCtx,
        diagnostics: &mut Vec<LintDiagnostic>,
    ) {
        self.push_diagnostic(NODE_HIT, node.text_range(), diagnostics);
    }

    fn check_token(
        &self,
        token: &rowan::SyntaxToken<ddlint::DdlogLanguage>,
        _ctx: &RuleCtx,
        diagnostics: &mut Vec<LintDiagnostic>,
    ) {
        self.push_diagnostic(TOKEN_HIT, token.text_range(), diagnostics);
    }
}

/// A rule with no target kinds; must never be dispatched.
struct EmptyTargetsRule;

impl Rule for EmptyTargetsRule {
    fn name(&self) -> &'static str {
        "empty-targets-rule"
    }

    fn group(&self) -> &'static str {
        "test"
    }

    fn docs(&self) -> &'static str {
        "Rule with empty target_kinds; must never be dispatched."
    }
}

impl CstRule for EmptyTargetsRule {
    fn target_kinds(&self) -> &'static [SyntaxKind] {
        &[]
    }

    fn check_node(
        &self,
        _node: &rowan::SyntaxNode<ddlint::DdlogLanguage>,
        _ctx: &RuleCtx,
        _diagnostics: &mut Vec<LintDiagnostic>,
    ) {
        panic!("EmptyTargetsRule::check_node must never be called");
    }

    fn check_token(
        &self,
        _token: &rowan::SyntaxToken<ddlint::DdlogLanguage>,
        _ctx: &RuleCtx,
        _diagnostics: &mut Vec<LintDiagnostic>,
    ) {
        panic!("EmptyTargetsRule::check_token must never be called");
    }
}

/// A rule that reads config and only emits diagnostics when enabled.
struct ConfigAwareRule;

impl Rule for ConfigAwareRule {
    fn name(&self) -> &'static str {
        "config-aware-rule"
    }

    fn group(&self) -> &'static str {
        "test"
    }

    fn docs(&self) -> &'static str {
        "Only emits diagnostics when config key 'enabled' is true."
    }
}

impl CstRule for ConfigAwareRule {
    fn target_kinds(&self) -> &'static [SyntaxKind] {
        &[SyntaxKind::N_RELATION_DECL]
    }

    fn check_node(
        &self,
        node: &rowan::SyntaxNode<ddlint::DdlogLanguage>,
        ctx: &RuleCtx,
        diagnostics: &mut Vec<LintDiagnostic>,
    ) {
        if ctx.config_bool("enabled") == Some(true) {
            diagnostics.push(LintDiagnostic::new(
                self.name(),
                "config-enabled hit",
                node.text_range(),
            ));
        }
    }
}

struct IdentTokenRule;

impl Rule for IdentTokenRule {
    fn name(&self) -> &'static str {
        "ident-token-rule"
    }

    fn group(&self) -> &'static str {
        "style"
    }

    fn docs(&self) -> &'static str {
        "Flags every identifier token."
    }
}

impl CstRule for IdentTokenRule {
    fn target_kinds(&self) -> &'static [SyntaxKind] {
        &[SyntaxKind::T_IDENT]
    }

    fn check_token(
        &self,
        token: &rowan::SyntaxToken<ddlint::DdlogLanguage>,
        _ctx: &RuleCtx,
        diagnostics: &mut Vec<LintDiagnostic>,
    ) {
        diagnostics.push(LintDiagnostic::new(
            self.name(),
            TOKEN_HIT,
            token.text_range(),
        ));
    }
}

// -- helpers ------------------------------------------------------------------

/// Run every element in the CST through the store's dispatch index.
fn run_store_over_cst(parsed: &Parsed, ctx: &RuleCtx, store: &CstRuleStore) -> Vec<LintDiagnostic> {
    let mut diagnostics = Vec::new();
    for element in parsed.root().syntax().descendants_with_tokens() {
        let kind = element.kind();
        for rule in store.rules_for_kind(kind) {
            match &element {
                NodeOrToken::Node(node) => {
                    rule.check_node(node, ctx, &mut diagnostics);
                }
                NodeOrToken::Token(token) => {
                    rule.check_token(token, ctx, &mut diagnostics);
                }
            }
        }
    }
    diagnostics
}

/// Run a single rule using the manual `target_kinds().contains()` pattern
/// from `tests/linter_rule_traits.rs`.
fn run_rule_manually(parsed: &Parsed, ctx: &RuleCtx, rule: &dyn CstRule) -> Vec<LintDiagnostic> {
    let mut diagnostics = Vec::new();
    let target_kinds = rule.target_kinds();

    for element in parsed.root().syntax().descendants_with_tokens() {
        match element {
            NodeOrToken::Node(node) if target_kinds.contains(&node.kind()) => {
                rule.check_node(&node, ctx, &mut diagnostics);
            }
            NodeOrToken::Token(token) if target_kinds.contains(&token.kind()) => {
                rule.check_token(&token, ctx, &mut diagnostics);
            }
            _ => {}
        }
    }

    diagnostics
}

// -- fixtures -----------------------------------------------------------------

#[fixture]
fn parsed_fixture(
    #[default(include_str!("../examples/hello_join.dl"))] source: &str,
    #[default("hello_join")] fixture_name: &str,
) -> Parsed {
    let parsed = parse(source);
    assert!(
        parsed.errors().is_empty(),
        "fixture source `{fixture_name}` should parse cleanly"
    );
    parsed
}

// -- tests --------------------------------------------------------------------

#[rstest]
#[case("hello_join", include_str!("../examples/hello_join.dl"))]
#[case("reachability", include_str!("../examples/reachability.dl"))]
fn store_dispatch_matches_manual_traversal(
    #[case] fixture_name: &str,
    #[case] source: &str,
    #[with(source, fixture_name)] parsed_fixture: Parsed,
) {
    let ctx = RuleCtx::from_parsed(source, &parsed_fixture, RuleConfig::new());

    // Manual dispatch (baseline).
    let manual = run_rule_manually(&parsed_fixture, &ctx, &CountingRule);

    // Store-based dispatch.
    let mut store = CstRuleStore::new();
    store.register(Box::new(CountingRule));
    let via_store = run_store_over_cst(&parsed_fixture, &ctx, &store);

    assert_eq!(
        manual.len(),
        via_store.len(),
        "store dispatch should produce the same number of diagnostics as manual \
         dispatch for `{fixture_name}`",
    );
    assert_eq!(
        manual, via_store,
        "store dispatch diagnostics should match manual dispatch exactly for \
         `{fixture_name}`",
    );
}

#[rstest]
fn store_dispatch_with_multiple_rules(parsed_fixture: Parsed) {
    let source: String = parsed_fixture.root().text();
    let ctx = RuleCtx::from_parsed(source.as_str(), &parsed_fixture, RuleConfig::new());

    let mut store = CstRuleStore::new();
    store
        .register(Box::new(CountingRule))
        .register(Box::new(IdentTokenRule));

    let diagnostics = run_store_over_cst(&parsed_fixture, &ctx, &store);

    let counting_hits: Vec<_> = diagnostics
        .iter()
        .filter(|d| d.rule_name() == "counting-rule")
        .collect();
    let ident_hits: Vec<_> = diagnostics
        .iter()
        .filter(|d| d.rule_name() == "ident-token-rule")
        .collect();

    // CountingRule targets N_RELATION_DECL and K_RELATION — both present in
    // the hello_join fixture.
    assert!(
        !counting_hits.is_empty(),
        "counting-rule should produce diagnostics"
    );

    // IdentTokenRule targets T_IDENT — identifiers are present in every
    // DDlog source file.
    assert!(
        !ident_hits.is_empty(),
        "ident-token-rule should produce diagnostics"
    );

    // Total is the sum from both rules.
    assert_eq!(diagnostics.len(), counting_hits.len() + ident_hits.len());
}

#[rstest]
fn empty_store_produces_no_matches(parsed_fixture: Parsed) {
    let source: String = parsed_fixture.root().text();
    let ctx = RuleCtx::from_parsed(source.as_str(), &parsed_fixture, RuleConfig::new());

    let store = CstRuleStore::new();
    let diagnostics = run_store_over_cst(&parsed_fixture, &ctx, &store);

    assert!(
        diagnostics.is_empty(),
        "an empty store should produce no diagnostics"
    );
}

#[rstest]
fn store_dispatch_with_context_config(parsed_fixture: Parsed) {
    let source: String = parsed_fixture.root().text();

    // With "enabled" = true the config-aware rule emits diagnostics.
    let enabled_config = RuleConfig::from([("enabled".to_owned(), RuleConfigValue::Bool(true))]);
    let enabled_ctx = RuleCtx::from_parsed(source.as_str(), &parsed_fixture, enabled_config);

    let mut store = CstRuleStore::new();
    store.register(Box::new(ConfigAwareRule));

    let enabled_diagnostics = run_store_over_cst(&parsed_fixture, &enabled_ctx, &store);

    assert!(
        !enabled_diagnostics.is_empty(),
        "config-aware rule should emit diagnostics when enabled"
    );
    assert!(
        enabled_diagnostics
            .iter()
            .all(|d| d.rule_name() == "config-aware-rule"),
        "all diagnostics should be from config-aware-rule"
    );

    // With "enabled" = false the same rule emits nothing.
    let disabled_config = RuleConfig::from([("enabled".to_owned(), RuleConfigValue::Bool(false))]);
    let disabled_ctx = RuleCtx::from_parsed(source.as_str(), &parsed_fixture, disabled_config);

    let disabled_diagnostics = run_store_over_cst(&parsed_fixture, &disabled_ctx, &store);

    assert!(
        disabled_diagnostics.is_empty(),
        "config-aware rule should emit nothing when disabled"
    );
}

#[rstest]
fn rule_with_empty_target_kinds_is_never_dispatched(parsed_fixture: Parsed) {
    let source: String = parsed_fixture.root().text();
    let ctx = RuleCtx::from_parsed(source.as_str(), &parsed_fixture, RuleConfig::new());

    let mut store = CstRuleStore::new();
    store
        .register(Box::new(EmptyTargetsRule))
        .register(Box::new(CountingRule));

    // If EmptyTargetsRule were dispatched its check_* methods would panic.
    let diagnostics = run_store_over_cst(&parsed_fixture, &ctx, &store);

    // Only CountingRule diagnostics should appear.
    assert!(
        !diagnostics.is_empty(),
        "counting-rule should still produce diagnostics"
    );
    assert!(
        diagnostics.iter().all(|d| d.rule_name() == "counting-rule"),
        "no diagnostics should come from empty-targets-rule"
    );
}
