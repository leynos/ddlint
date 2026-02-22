//! Core lint rule traits.
//!
//! The contracts in this module intentionally stay small for the initial
//! linter-engine milestone. Rule execution, storage, and richer contextual
//! data are introduced by follow-up roadmap items.

use std::collections::BTreeMap;
use std::sync::Arc;

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

/// A typed configuration value passed into a lint rule.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RuleConfigValue {
    /// A boolean setting.
    Bool(bool),
    /// A signed integer setting.
    Integer(i64),
    /// A string setting.
    String(String),
}

impl RuleConfigValue {
    /// Return the boolean value when this variant is [`Self::Bool`].
    #[must_use]
    pub fn as_bool(&self) -> Option<bool> {
        match self {
            Self::Bool(value) => Some(*value),
            Self::Integer(_) | Self::String(_) => None,
        }
    }

    /// Return the integer value when this variant is [`Self::Integer`].
    #[must_use]
    pub fn as_integer(&self) -> Option<i64> {
        match self {
            Self::Integer(value) => Some(*value),
            Self::Bool(_) | Self::String(_) => None,
        }
    }

    /// Return the string value when this variant is [`Self::String`].
    #[must_use]
    pub fn as_string(&self) -> Option<&str> {
        match self {
            Self::String(value) => Some(value),
            Self::Bool(_) | Self::Integer(_) => None,
        }
    }
}

/// Rule-specific configuration mapping.
pub type RuleConfig = BTreeMap<String, RuleConfigValue>;

/// Transient rule execution context.
///
/// The context is created per lint run and passed to every rule invocation.
/// It contains the full source text, the typed AST root, and resolved
/// configuration values for the active rule.
#[derive(Debug, Clone)]
pub struct RuleCtx {
    source_text: Arc<str>,
    ast_root: crate::parser::ast::Root,
    config: RuleConfig,
}

impl RuleCtx {
    /// Create a new rule context from source text, AST root, and configuration.
    #[must_use]
    pub fn new(
        source_text: impl Into<Arc<str>>,
        ast_root: crate::parser::ast::Root,
        config: RuleConfig,
    ) -> Self {
        Self {
            source_text: source_text.into(),
            ast_root,
            config,
        }
    }

    /// Create a context from an already parsed program.
    ///
    /// This helper clones the typed AST root from `parsed`, so callers can
    /// keep using the parse result after constructing the context.
    #[must_use]
    pub fn from_parsed(
        source_text: impl Into<Arc<str>>,
        parsed: &crate::Parsed,
        config: RuleConfig,
    ) -> Self {
        Self::new(source_text, parsed.root().clone(), config)
    }

    /// Return the full source text associated with the lint run.
    #[must_use]
    pub fn source_text(&self) -> &str {
        &self.source_text
    }

    /// Return the typed AST root for semantic navigation.
    #[must_use]
    pub fn ast_root(&self) -> &crate::parser::ast::Root {
        &self.ast_root
    }

    /// Return the CST root syntax node.
    #[must_use]
    pub fn cst_root(&self) -> &rowan::SyntaxNode<DdlogLanguage> {
        self.ast_root.syntax()
    }

    /// Return the full per-rule configuration map.
    #[must_use]
    pub fn config(&self) -> &RuleConfig {
        &self.config
    }

    /// Return a single configuration entry by key.
    #[must_use]
    pub fn config_value(&self, key: &str) -> Option<&RuleConfigValue> {
        self.config.get(key)
    }

    /// Return a boolean configuration value by key when present and typed.
    #[must_use]
    pub fn config_bool(&self, key: &str) -> Option<bool> {
        self.config_value(key).and_then(RuleConfigValue::as_bool)
    }

    /// Return an integer configuration value by key when present and typed.
    #[must_use]
    pub fn config_int(&self, key: &str) -> Option<i64> {
        self.config_value(key).and_then(RuleConfigValue::as_integer)
    }

    /// Return a string configuration value by key when present and typed.
    #[must_use]
    pub fn config_string(&self, key: &str) -> Option<&str> {
        self.config_value(key).and_then(RuleConfigValue::as_string)
    }
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

    use super::{CstRule, LintDiagnostic, Rule, RuleConfig, RuleConfigValue, RuleCtx};
    use crate::{SyntaxKind, parse};

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
    fn rule_config_value_accessors_are_typed() {
        assert_eq!(RuleConfigValue::Bool(true).as_bool(), Some(true));
        assert_eq!(RuleConfigValue::Integer(3).as_integer(), Some(3));
        assert_eq!(
            RuleConfigValue::String("x".to_owned()).as_string(),
            Some("x")
        );
        assert_eq!(RuleConfigValue::Bool(true).as_integer(), None);
        assert_eq!(RuleConfigValue::Integer(3).as_string(), None);
    }

    #[test]
    fn rule_ctx_exposes_source_ast_and_config() {
        let source = "input relation R(x: u32);";
        let parsed = parse(source);
        assert!(parsed.errors().is_empty());

        let config = RuleConfig::from([
            ("enabled".to_owned(), RuleConfigValue::Bool(true)),
            ("max_depth".to_owned(), RuleConfigValue::Integer(2)),
            (
                "style".to_owned(),
                RuleConfigValue::String("strict".to_owned()),
            ),
        ]);

        let ctx = RuleCtx::from_parsed(source, &parsed, config.clone());

        assert_eq!(ctx.source_text(), source);
        assert_eq!(ctx.ast_root().relations().len(), 1);
        assert_eq!(ctx.cst_root().kind(), SyntaxKind::N_DATALOG_PROGRAM);
        assert_eq!(ctx.config(), &config);
        assert_eq!(
            ctx.config_value("enabled"),
            Some(&RuleConfigValue::Bool(true))
        );
        assert_eq!(ctx.config_bool("enabled"), Some(true));
        assert_eq!(ctx.config_int("max_depth"), Some(2));
        assert_eq!(ctx.config_string("style"), Some("strict"));
        assert_eq!(ctx.config_bool("style"), None);
        assert_eq!(ctx.config_value("missing"), None);
    }
}
