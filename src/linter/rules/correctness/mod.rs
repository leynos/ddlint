//! Correctness lint rules that flag likely semantic mistakes.

mod unused_relation;
mod unused_variable;

pub use unused_relation::UnusedRelationRule;
pub use unused_variable::UnusedVariableRule;
