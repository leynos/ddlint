//! Behavioural tests for linter rule traits and hook dispatch.

use rowan::NodeOrToken;
use rstest::{fixture, rstest};

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

fn collect_node_ranges(parsed: &Parsed) -> Vec<rowan::TextRange> {
    parsed
        .root()
        .syntax()
        .descendants()
        .filter(|node| node.kind() == SyntaxKind::N_RELATION_DECL)
        .map(|node| node.text_range())
        .collect()
}

fn collect_token_ranges(parsed: &Parsed) -> Vec<rowan::TextRange> {
    parsed
        .root()
        .syntax()
        .descendants_with_tokens()
        .filter_map(rowan::NodeOrToken::into_token)
        .filter(|token| token.kind() == SyntaxKind::K_RELATION)
        .map(|token| token.text_range())
        .collect()
}

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

#[rstest]
#[case("hello_join", include_str!("../examples/hello_join.dl"))]
#[case("reachability", include_str!("../examples/reachability.dl"))]
fn dispatch_invokes_node_and_token_hooks(
    #[case] fixture_name: &str,
    #[case] source: &str,
    #[with(source, fixture_name)] parsed_fixture: Parsed,
) {
    let expected_node_hits = count_kind_hits(&parsed_fixture, SyntaxKind::N_RELATION_DECL);
    let expected_token_hits = count_kind_hits(&parsed_fixture, SyntaxKind::K_RELATION);
    let diagnostics = run_rule_over_cst(&parsed_fixture, &CountingRule);
    let Ok(source_len_u32) = u32::try_from(source.len()) else {
        panic!("fixture `{fixture_name}` has an invalid source length for range checks");
    };
    let source_end = rowan::TextSize::from(source_len_u32);
    let node_ranges = collect_node_ranges(&parsed_fixture);
    let token_ranges = collect_token_ranges(&parsed_fixture);

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

    assert_eq!(
        diagnostics.len(),
        expected_node_hits + expected_token_hits,
        "expected one diagnostic per hook invocation in `{fixture_name}`",
    );
    assert!(
        diagnostics
            .iter()
            .all(|diagnostic| diagnostic.rule_name() == "counting-rule"),
        "all diagnostics should be attributed to `counting-rule`",
    );
    assert!(
        diagnostics
            .iter()
            .all(|diagnostic| !diagnostic.span().is_empty()),
        "all diagnostics should have non-empty spans",
    );
    assert!(
        diagnostics
            .iter()
            .all(|diagnostic| diagnostic.span().end() <= source_end),
        "all diagnostics should stay within source bounds",
    );

    let matched_node_diagnostics = diagnostics
        .iter()
        .filter(|diagnostic| {
            diagnostic.message() == NODE_HIT_MESSAGE && node_ranges.contains(&diagnostic.span())
        })
        .count();
    let matched_token_diagnostics = diagnostics
        .iter()
        .filter(|diagnostic| {
            diagnostic.message() == TOKEN_HIT_MESSAGE && token_ranges.contains(&diagnostic.span())
        })
        .count();

    assert_eq!(
        matched_node_diagnostics, expected_node_hits,
        "node diagnostics should map to node ranges in `{fixture_name}`",
    );
    assert_eq!(
        matched_token_diagnostics, expected_token_hits,
        "token diagnostics should map to token ranges in `{fixture_name}`",
    );
}

#[rstest]
fn default_hook_implementations_emit_no_diagnostics(parsed_fixture: Parsed) {
    let diagnostics = run_rule_over_cst(&parsed_fixture, &MetadataOnlyRule);
    assert!(diagnostics.is_empty());
}
