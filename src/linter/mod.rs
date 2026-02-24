//! Linter engine contracts.
//!
//! This module defines the foundational traits, lightweight data structures,
//! and rule-dispatch registry used by CST-driven lint rules.  Execution
//! orchestration is added in later roadmap milestones.

mod rule;
mod store;

pub use rule::{CstRule, LintDiagnostic, Rule, RuleConfig, RuleConfigValue, RuleCtx};
pub use store::CstRuleStore;
