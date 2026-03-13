//! Behavioural tests for semantic symbol tables and scope resolution.

use ddlint::linter::{CstRule, CstRuleStore, LintDiagnostic, Rule, RuleConfig, Runner};
use ddlint::sema::{self, DeclarationKind, Resolution, UseKind};
use ddlint::{SyntaxKind, parse};

fn parse_ok(source: &str) -> ddlint::Parsed {
    let parsed = parse(source);
    assert!(
        parsed.errors().is_empty(),
        "semantic behaviour source should parse cleanly: {:?}",
        parsed.errors()
    );
    parsed
}

fn uses_named<'a>(
    model: &'a ddlint::sema::SemanticModel,
    name: &str,
    kind: UseKind,
) -> Vec<&'a ddlint::sema::UseSite> {
    model
        .uses()
        .iter()
        .filter(|use_site| use_site.name() == name && use_site.kind() == kind)
        .collect()
}

#[test]
fn semantic_model_records_declarations_and_resolved_uses_end_to_end() {
    let parsed = parse_ok("input relation Source(x: u32)\nOutput(x) :- Source(x).");
    let model = sema::build(&parsed);

    let source_declarations: Vec<_> = model
        .symbols()
        .iter()
        .filter(|symbol| symbol.kind() == DeclarationKind::Relation && symbol.name() == "Source")
        .collect();
    let source_uses = uses_named(&model, "Source", UseKind::Relation);
    let x_uses = uses_named(&model, "x", UseKind::Variable);

    assert_eq!(source_declarations.len(), 1);
    assert_eq!(source_uses.len(), 1);
    assert!(matches!(
        source_uses.first().map(|use_site| use_site.resolution()),
        Some(Resolution::Resolved(_))
    ));
    assert!(
        x_uses
            .iter()
            .any(|use_site| matches!(use_site.resolution(), Resolution::Resolved(_))),
        "body variable use should resolve through the semantic model",
    );
}

#[test]
fn semantic_model_keeps_unresolved_names_without_crashing() {
    let parsed = parse_ok("Output(x) :- Source(x), Missing(y).");
    let model = sema::build(&parsed);
    let missing_y = uses_named(&model, "y", UseKind::Variable);

    assert_eq!(missing_y.len(), 1);
    assert_eq!(
        missing_y.first().map(|use_site| use_site.resolution()),
        Some(Resolution::Unresolved)
    );
}

struct SemanticAwareRule;

impl Rule for SemanticAwareRule {
    fn name(&self) -> &'static str {
        "semantic-aware-rule"
    }

    fn group(&self) -> &'static str {
        "correctness"
    }

    fn docs(&self) -> &'static str {
        "Proves that semantic context is available through RuleCtx."
    }
}

impl CstRule for SemanticAwareRule {
    fn target_kinds(&self) -> &'static [SyntaxKind] {
        &[SyntaxKind::N_RULE]
    }

    fn check_node(
        &self,
        node: &rowan::SyntaxNode<ddlint::DdlogLanguage>,
        ctx: &ddlint::linter::RuleCtx,
        diagnostics: &mut Vec<LintDiagnostic>,
    ) {
        let unresolved_variable_uses = ctx
            .semantic_model()
            .uses()
            .iter()
            .filter(|use_site| {
                use_site.kind() == UseKind::Variable
                    && use_site.resolution() == Resolution::Unresolved
            })
            .count();

        if unresolved_variable_uses > 0 {
            diagnostics.push(LintDiagnostic::new(
                self.name(),
                format!("semantic model saw {unresolved_variable_uses} unresolved variable use(s)"),
                node.text_range(),
            ));
        }
    }
}

#[test]
fn runner_exposes_semantic_model_to_lint_rules() {
    let source = "Output(x) :- Source(x), Missing(y).";
    let parsed = parse_ok(source);
    let mut store = CstRuleStore::new();
    store.register(Box::new(SemanticAwareRule));

    let diagnostics = Runner::new(&store, source, &parsed, RuleConfig::new()).run();

    assert_eq!(diagnostics.len(), 1);
    assert_eq!(
        diagnostics
            .first()
            .map(ddlint::linter::LintDiagnostic::rule_name),
        Some("semantic-aware-rule")
    );
    assert_eq!(
        diagnostics
            .first()
            .map(ddlint::linter::LintDiagnostic::message),
        Some("semantic model saw 1 unresolved variable use(s)")
    );
}
