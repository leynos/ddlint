//! Linter engine contracts.
//!
//! This module defines the foundational traits and lightweight data structures
//! used by CST-driven lint rules. Execution orchestration and rich rule
//! context are added in later roadmap milestones.

mod rule;

pub use rule::{CstRule, LintDiagnostic, Rule, RuleCtx};
