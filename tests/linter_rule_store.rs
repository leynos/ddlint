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
    let config = RuleConfig::from([("enabled".to_owned(), RuleConfigValue::Bool(true))]);
    let ctx = RuleCtx::from_parsed(source.as_str(), &parsed_fixture, config);

    let mut store = CstRuleStore::new();
    store.register(Box::new(CountingRule));

    let diagnostics = run_store_over_cst(&parsed_fixture, &ctx, &store);

    // Verify context is available through the store dispatch path.
    assert!(
        !diagnostics.is_empty(),
        "store-dispatched rules should receive a valid RuleCtx"
    );
    assert!(
        diagnostics.iter().all(|d| d.rule_name() == "counting-rule"),
        "all diagnostics should be attributed to counting-rule"
    );
}
