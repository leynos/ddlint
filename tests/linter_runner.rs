//! Behavioural tests for the visitor-based parallel rule runner.

use rowan::NodeOrToken;
use rstest::{fixture, rstest};

use ddlint::linter::{
    CstRule, CstRuleStore, LintDiagnostic, Rule, RuleConfig, RuleConfigValue, RuleCtx, Runner,
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
        "Only emits diagnostics when 'enabled' is true."
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

/// Run every element in the CST through the store's dispatch index
/// (sequential baseline for comparison).
fn sequential_dispatch(
    parsed: &Parsed,
    ctx: &RuleCtx,
    store: &CstRuleStore,
) -> Vec<LintDiagnostic> {
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
    diagnostics.sort_by(|a, b| {
        a.span()
            .start()
            .cmp(&b.span().start())
            .then_with(|| a.span().end().cmp(&b.span().end()))
            .then_with(|| a.rule_name().cmp(b.rule_name()))
    });
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
fn runner_matches_sequential_dispatch(
    #[case] fixture_name: &str,
    #[case] source: &str,
    #[with(source, fixture_name)] parsed_fixture: Parsed,
) {
    let ctx = RuleCtx::from_parsed(source, &parsed_fixture, RuleConfig::new());

    let mut store = CstRuleStore::new();
    store.register(Box::new(CountingRule));

    // Sequential baseline.
    let expected = sequential_dispatch(&parsed_fixture, &ctx, &store);

    // Parallel runner.
    let runner = Runner::new(&store, source, &parsed_fixture, RuleConfig::new());
    let actual = runner.run();

    assert_eq!(
        expected.len(),
        actual.len(),
        "runner should produce the same number of diagnostics as sequential \
         dispatch for `{fixture_name}`",
    );
    assert_eq!(
        expected, actual,
        "runner diagnostics should match sequential dispatch for `{fixture_name}`",
    );
}

#[rstest]
fn runner_with_empty_store_produces_no_diagnostics(parsed_fixture: Parsed) {
    let source: String = parsed_fixture.root().text();
    let store = CstRuleStore::new();
    let runner = Runner::new(&store, source.as_str(), &parsed_fixture, RuleConfig::new());
    let diagnostics = runner.run();

    assert!(
        diagnostics.is_empty(),
        "an empty store should produce no diagnostics"
    );
}

#[rstest]
fn runner_with_multiple_rules(parsed_fixture: Parsed) {
    let source: String = parsed_fixture.root().text();

    let mut store = CstRuleStore::new();
    store
        .register(Box::new(CountingRule))
        .register(Box::new(IdentTokenRule));

    let runner = Runner::new(&store, source.as_str(), &parsed_fixture, RuleConfig::new());
    let diagnostics = runner.run();

    let counting_hits: Vec<_> = diagnostics
        .iter()
        .filter(|d| d.rule_name() == "counting-rule")
        .collect();
    let ident_hits: Vec<_> = diagnostics
        .iter()
        .filter(|d| d.rule_name() == "ident-token-rule")
        .collect();

    assert!(
        !counting_hits.is_empty(),
        "counting-rule should produce diagnostics"
    );
    assert!(
        !ident_hits.is_empty(),
        "ident-token-rule should produce diagnostics"
    );
    assert_eq!(diagnostics.len(), counting_hits.len() + ident_hits.len());
}

#[rstest]
fn runner_handles_empty_target_kinds(parsed_fixture: Parsed) {
    let source: String = parsed_fixture.root().text();

    let mut store = CstRuleStore::new();
    store
        .register(Box::new(EmptyTargetsRule))
        .register(Box::new(CountingRule));

    // If EmptyTargetsRule were dispatched its check_* methods would panic.
    let runner = Runner::new(&store, source.as_str(), &parsed_fixture, RuleConfig::new());
    let diagnostics = runner.run();

    assert!(
        !diagnostics.is_empty(),
        "counting-rule should still produce diagnostics"
    );
    assert!(
        diagnostics.iter().all(|d| d.rule_name() == "counting-rule"),
        "no diagnostics should come from empty-targets-rule"
    );
}

#[rstest]
fn runner_respects_config(parsed_fixture: Parsed) {
    let source: String = parsed_fixture.root().text();

    let mut store = CstRuleStore::new();
    store.register(Box::new(ConfigAwareRule));

    // With "enabled" = true the config-aware rule emits diagnostics.
    let enabled_config = RuleConfig::from([("enabled".to_owned(), RuleConfigValue::Bool(true))]);
    let runner = Runner::new(&store, source.as_str(), &parsed_fixture, enabled_config);
    let enabled_diagnostics = runner.run();

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
    let runner = Runner::new(&store, source.as_str(), &parsed_fixture, disabled_config);
    let disabled_diagnostics = runner.run();

    assert!(
        disabled_diagnostics.is_empty(),
        "config-aware rule should emit nothing when disabled"
    );
}

#[rstest]
fn runner_produces_deterministic_ordering(parsed_fixture: Parsed) {
    let source: String = parsed_fixture.root().text();

    let mut store = CstRuleStore::new();
    store
        .register(Box::new(CountingRule))
        .register(Box::new(IdentTokenRule));

    let config = RuleConfig::new();
    let first = Runner::new(&store, source.as_str(), &parsed_fixture, config.clone()).run();
    let second = Runner::new(&store, source.as_str(), &parsed_fixture, config.clone()).run();
    let third = Runner::new(&store, source.as_str(), &parsed_fixture, config).run();

    assert_eq!(first, second, "first and second runs should be identical");
    assert_eq!(second, third, "second and third runs should be identical");
}

#[rstest]
fn runner_diagnostics_sorted_by_span_then_rule_name(parsed_fixture: Parsed) {
    let source: String = parsed_fixture.root().text();

    let mut store = CstRuleStore::new();
    // Two rules targeting the same kind — sort tiebreaker is rule name.
    store
        .register(Box::new(CountingRule))
        .register(Box::new(IdentTokenRule));

    let runner = Runner::new(&store, source.as_str(), &parsed_fixture, RuleConfig::new());
    let diagnostics = runner.run();

    // Verify the sort invariant holds for all adjacent pairs.
    for pair in diagnostics.windows(2) {
        let (Some(a), Some(b)) = (pair.first(), pair.get(1)) else {
            continue;
        };
        let order = a
            .span()
            .start()
            .cmp(&b.span().start())
            .then_with(|| a.span().end().cmp(&b.span().end()))
            .then_with(|| a.rule_name().cmp(b.rule_name()));
        assert!(
            order.is_le(),
            "diagnostics should be sorted: ({:?}, {:?}, {:?}) should come \
             before ({:?}, {:?}, {:?})",
            a.span().start(),
            a.span().end(),
            a.rule_name(),
            b.span().start(),
            b.span().end(),
            b.rule_name(),
        );
    }
}
