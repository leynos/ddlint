//! Core lint rule traits.
//!
//! The contracts in this module intentionally stay small for the initial
//! linter-engine milestone. Rule execution, storage, and richer contextual
//! data are introduced by follow-up roadmap items.

use rowan::TextRange;

use crate::{DdlogLanguage, SyntaxKind};

/// A lightweight lint diagnostic emitted by a rule.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LintDiagnostic {
    rule_name: &'static str,
    message: String,
    span: TextRange,
}

impl LintDiagnostic {
    /// Create a new lint diagnostic.
    #[must_use]
    pub fn new(rule_name: &'static str, message: impl Into<String>, span: TextRange) -> Self {
        Self {
            rule_name,
            message: message.into(),
            span,
        }
    }

    /// Return the emitting rule's canonical name.
    #[must_use]
    pub fn rule_name(&self) -> &'static str {
        self.rule_name
    }

    /// Return the user-facing diagnostic message.
    #[must_use]
    pub fn message(&self) -> &str {
        &self.message
    }

    /// Return the source span associated with this diagnostic.
    #[must_use]
    pub fn span(&self) -> TextRange {
        self.span
    }
}

/// Transient rule execution context.
///
/// This placeholder is intentionally empty in milestone `3.1.1`. Subsequent
/// milestones extend it with source text, configuration, and semantic context.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct RuleCtx {
    _reserved: (),
}

/// Metadata shared by every lint rule.
pub trait Rule {
    /// Return the rule's unique kebab-case identifier.
    fn name(&self) -> &'static str;

    /// Return the rule group identifier (for example, `correctness`).
    fn group(&self) -> &'static str;

    /// Return human-readable rule documentation.
    fn docs(&self) -> &'static str;
}

/// CST-driven lint behaviour contract.
///
/// Rules advertise which syntax kinds they want via [`Self::target_kinds`].
/// The rule runner then invokes [`Self::check_node`] and
/// [`Self::check_token`] for matching elements.
pub trait CstRule: Rule + Send + Sync {
    /// Return syntax kinds that should trigger this rule.
    fn target_kinds(&self) -> &'static [SyntaxKind];

    /// Analyse a matching syntax node.
    ///
    /// The default implementation is a no-op.
    fn check_node(
        &self,
        _node: &rowan::SyntaxNode<DdlogLanguage>,
        _ctx: &RuleCtx,
        _diagnostics: &mut Vec<LintDiagnostic>,
    ) {
    }

    /// Analyse a matching syntax token.
    ///
    /// The default implementation is a no-op.
    fn check_token(
        &self,
        _token: &rowan::SyntaxToken<DdlogLanguage>,
        _ctx: &RuleCtx,
        _diagnostics: &mut Vec<LintDiagnostic>,
    ) {
    }
}

#[cfg(test)]
mod tests {
    use rowan::TextSize;

    use super::{CstRule, LintDiagnostic, Rule, RuleCtx};
    use crate::SyntaxKind;

    struct ExampleRule;

    impl Rule for ExampleRule {
        fn name(&self) -> &'static str {
            "example-rule"
        }

        fn group(&self) -> &'static str {
            "correctness"
        }

        fn docs(&self) -> &'static str {
            "Example rule documentation."
        }
    }

    impl CstRule for ExampleRule {
        fn target_kinds(&self) -> &'static [SyntaxKind] {
            &[SyntaxKind::N_RULE]
        }
    }

    fn assert_send_sync<T: Send + Sync>() {}

    #[test]
    fn metadata_is_available_through_trait_object() {
        let rule: &dyn CstRule = &ExampleRule;
        assert_eq!(rule.name(), "example-rule");
        assert_eq!(rule.group(), "correctness");
        assert_eq!(rule.docs(), "Example rule documentation.");
        assert_eq!(rule.target_kinds(), &[SyntaxKind::N_RULE]);
    }

    #[test]
    fn cst_rule_is_send_and_sync() {
        assert_send_sync::<ExampleRule>();
    }

    #[test]
    fn lint_diagnostic_accessors_round_trip() {
        let span = rowan::TextRange::new(TextSize::from(1), TextSize::from(3));
        let diagnostic = LintDiagnostic::new("example-rule", "message", span);

        assert_eq!(diagnostic.rule_name(), "example-rule");
        assert_eq!(diagnostic.message(), "message");
        assert_eq!(diagnostic.span(), span);
    }

    #[test]
    fn rule_ctx_defaults() {
        let ctx = RuleCtx::default();
        assert_eq!(ctx, RuleCtx::default());
    }
}
