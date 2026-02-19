//! Behavioural tests for linter rule traits and hook dispatch.

use rowan::NodeOrToken;
use rstest::rstest;

use ddlint::linter::{CstRule, LintDiagnostic, Rule, RuleCtx};
use ddlint::{Parsed, SyntaxKind, parse};

const NODE_HIT_MESSAGE: &str = "node hit";
const TOKEN_HIT_MESSAGE: &str = "token hit";

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
        self.push_diagnostic(NODE_HIT_MESSAGE, node.text_range(), diagnostics);
    }

    fn check_token(
        &self,
        token: &rowan::SyntaxToken<ddlint::DdlogLanguage>,
        _ctx: &RuleCtx,
        diagnostics: &mut Vec<LintDiagnostic>,
    ) {
        self.push_diagnostic(TOKEN_HIT_MESSAGE, token.text_range(), diagnostics);
    }
}

struct MetadataOnlyRule;

impl Rule for MetadataOnlyRule {
    fn name(&self) -> &'static str {
        "metadata-only-rule"
    }

    fn group(&self) -> &'static str {
        "correctness"
    }

    fn docs(&self) -> &'static str {
        "Uses default no-op hook methods."
    }
}

impl CstRule for MetadataOnlyRule {
    fn target_kinds(&self) -> &'static [SyntaxKind] {
        &[SyntaxKind::N_RELATION_DECL, SyntaxKind::K_RELATION]
    }
}

fn run_rule_over_cst(parsed: &Parsed, rule: &dyn CstRule) -> Vec<LintDiagnostic> {
    let ctx = RuleCtx::default();
    let mut diagnostics = Vec::new();
    let target_kinds = rule.target_kinds();

    for element in parsed.root().syntax().descendants_with_tokens() {
        match element {
            NodeOrToken::Node(node) if target_kinds.contains(&node.kind()) => {
                rule.check_node(&node, &ctx, &mut diagnostics);
            }
            NodeOrToken::Token(token) if target_kinds.contains(&token.kind()) => {
                rule.check_token(&token, &ctx, &mut diagnostics);
            }
            _ => {}
        }
    }

    diagnostics
}

fn count_kind_hits(parsed: &Parsed, kind: SyntaxKind) -> usize {
    parsed
        .root()
        .syntax()
        .descendants_with_tokens()
        .filter(|element| element.kind() == kind)
        .count()
}

#[rstest]
#[case("hello_join", include_str!("../examples/hello_join.dl"))]
#[case("reachability", include_str!("../examples/reachability.dl"))]
fn dispatch_invokes_node_and_token_hooks(#[case] fixture_name: &str, #[case] source: &str) {
    let parsed = parse(source);
    assert!(
        parsed.errors().is_empty(),
        "fixture source `{fixture_name}` should parse cleanly"
    );

    let expected_node_hits = count_kind_hits(&parsed, SyntaxKind::N_RELATION_DECL);
    let expected_token_hits = count_kind_hits(&parsed, SyntaxKind::K_RELATION);
    let diagnostics = run_rule_over_cst(&parsed, &CountingRule);

    let node_hits = diagnostics
        .iter()
        .filter(|diagnostic| diagnostic.message() == NODE_HIT_MESSAGE)
        .count();
    let token_hits = diagnostics
        .iter()
        .filter(|diagnostic| diagnostic.message() == TOKEN_HIT_MESSAGE)
        .count();

    assert_eq!(node_hits, expected_node_hits);
    assert_eq!(token_hits, expected_token_hits);
}

#[test]
fn default_hook_implementations_emit_no_diagnostics() {
    let parsed = parse(include_str!("../examples/hello_join.dl"));
    assert!(
        parsed.errors().is_empty(),
        "fixture source should parse cleanly"
    );

    let diagnostics = run_rule_over_cst(&parsed, &MetadataOnlyRule);
    assert!(diagnostics.is_empty());
}
