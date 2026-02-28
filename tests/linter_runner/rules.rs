//! Shared test rule stubs for linter runner behavioural tests.

use ddlint::SyntaxKind;
use ddlint::linter::{CstRule, LintDiagnostic, Rule, RuleCtx};

pub const NODE_HIT: &str = "node hit";
pub const TOKEN_HIT: &str = "token hit";

pub struct CountingRule;

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
pub struct EmptyTargetsRule;

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
pub struct ConfigAwareRule;

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

pub struct IdentTokenRule;

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
