//! Public semantic-model construction entrypoints.
//!
//! The heavy lifting lives in `builder.rs`; this module keeps the outward
//! build API small and easy to navigate.

use crate::Parsed;
use crate::parser::ast;
use crate::parser::ast::SemanticRule;

use super::builder::SemanticModelBuilder;
use super::model::SemanticModel;

/// Build a semantic model from a parsed `DDlog` program.
///
/// # Examples
///
/// ```rust
/// use ddlint::parse;
/// use ddlint::sema;
///
/// let parsed = parse("input relation R(x: u32)\nOutput(x) :- R(x).");
/// let model = sema::build(&parsed);
/// assert!(!model.symbols().is_empty());
/// assert!(!model.uses().is_empty());
/// ```
#[must_use]
pub fn build(parsed: &Parsed) -> SemanticModel {
    build_from_parts(parsed.root(), parsed.semantic_rules())
}

/// Build a semantic model from an AST root only.
///
/// This variant omits parse-time semantic rules such as top-level `for`
/// desugaring. It is suitable for tests and call sites that have only a root.
///
/// # Examples
///
/// ```rust
/// use ddlint::parse;
/// use ddlint::sema;
///
/// let parsed = parse("input relation R(x: u32)");
/// let model = sema::build_from_root(parsed.root());
/// assert_eq!(model.scopes().len(), 1);
/// ```
#[must_use]
pub fn build_from_root(root: &ast::Root) -> SemanticModel {
    build_from_parts(root, &[])
}

/// Build a semantic model from an AST root and parse-time semantic rules.
#[must_use]
pub fn build_from_parts(root: &ast::Root, semantic_rules: &[SemanticRule]) -> SemanticModel {
    let mut builder = SemanticModelBuilder::new(root.syntax());
    builder.collect_program_declarations(root);
    builder.collect_ast_rules(root);
    builder.collect_semantic_rules(semantic_rules);
    builder.finish()
}
