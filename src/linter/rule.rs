//! Core lint rule traits.
//!
//! The contracts in this module intentionally stay small for the initial
//! linter-engine milestone. Rule execution, storage, and richer contextual
//! data are introduced by follow-up roadmap items.

use std::collections::BTreeMap;
use std::fmt;
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

/// Built-in severity level for a lint rule before configuration overrides.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RuleLevel {
    /// The rule is disabled by default.
    Allow,
    /// The rule emits a low-priority suggestion.
    Hint,
    /// The rule emits a warning.
    Warn,
    /// The rule emits an error.
    Error,
}

impl RuleLevel {
    /// Return the canonical lower-case spelling used in docs and config.
    #[must_use]
    pub const fn as_str(self) -> &'static str {
        match self {
            Self::Allow => "allow",
            Self::Hint => "hint",
            Self::Warn => "warn",
            Self::Error => "error",
        }
    }
}

impl fmt::Display for RuleLevel {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.as_str())
    }
}

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

    /// Return the rule's built-in severity level.
    fn default_level(&self) -> RuleLevel {
        RuleLevel::Warn
    }
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
mod tests;
