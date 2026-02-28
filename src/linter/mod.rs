//! Linter engine: traits, diagnostics, rule store, and parallel runner.
//!
//! This module defines the foundational traits, lightweight data structures,
//! rule-dispatch registry, and visitor-based parallel rule runner used by
//! CST-driven lint rules.

mod rule;
mod runner;
mod store;

pub use rule::{CstRule, LintDiagnostic, Rule, RuleConfig, RuleConfigValue, RuleCtx};
pub use runner::Runner;
pub use store::CstRuleStore;
