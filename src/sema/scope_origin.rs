//! Scope categories and provenance recorded by semantic analysis.
//!
//! [`ScopeKind`] says what sort of scope the semantic model recorded (the root
//! program, one rule, a nested `for`-loop body, or a closure body), and
//! [`ScopeOrigin`] says where the scope came from: the program itself, an
//! AST-backed rule, a parse-time semantic rule, a `for` loop inside a rule
//! body, or a closure inside a rule body. The builder in
//! `sema::builder` and the traversal in `sema::traverse` create them, and
//! `sema::model` re-exports both so existing imports keep working. They live
//! here, apart from the model's symbol tables, to keep `sema::model` under
//! Whitaker's module-length limit.

use crate::parser::ast::SemanticRuleOrigin;

/// Scope category recorded by semantic analysis.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ScopeKind {
    /// Root scope containing top-level declarations.
    Program,
    /// One syntactic rule or one desugared semantic rule.
    Rule,
    /// Nested scope created for a `for`-loop body.
    ForLoop,
    /// Nested scope created for a closure body.
    Closure,
}

/// Provenance for one recorded scope.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ScopeOrigin {
    /// The root program scope.
    Program,
    /// Scope for an AST-backed rule.
    AstRule {
        /// Index of the rule in the program's AST rule list.
        index: usize,
    },
    /// Scope for a parse-time semantic rule.
    SemanticRule {
        /// Index of the rule in the parser's semantic rule list.
        index: usize,
        /// Origin category of the semantic rule.
        origin: SemanticRuleOrigin,
    },
    /// Scope for a nested `for`-loop body.
    ForLoop {
        /// Index of the enclosing rule's body literal that holds the loop.
        literal_index: usize,
    },
    /// Scope for a closure body.
    Closure {
        /// Index of the enclosing rule body literal that holds the closure.
        literal_index: usize,
    },
}
