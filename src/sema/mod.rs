//! Semantic-analysis entrypoints and owned semantic model types.
//!
//! This module builds an immutable symbol and scope model from parsed `DDlog`
//! input so lint rules can query declarations, bindings, and resolved name
//! uses without re-walking the CST manually.

mod build;
mod builder;
mod model;
mod resolve;
mod traverse;
mod variables;

pub use build::{build, build_from_parts, build_from_root};
pub use model::{
    DeclarationKind, Resolution, Scope, ScopeId, ScopeKind, ScopeOrigin, SemanticModel, Symbol,
    SymbolId, SymbolOrigin, UseKind, UseSite,
};

#[cfg(test)]
mod tests;
