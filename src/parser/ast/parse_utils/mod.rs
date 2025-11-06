//! Parsing helpers shared across AST modules.
//!
//! This module provides small utilities for collecting parameter names and
//! types from the CST, recursively parsing type expressions, and parsing
//! transformer output identifiers. `Function`, `Relation`, and `Transformer`
//! nodes import these helpers, allowing them to share the same logic when
//! interpreting their declarations. See
//! `docs/function-parsing-design.md` for an overview.

mod delimiter;
pub(crate) mod errors;
mod outputs;
mod params;
mod relation;
mod token_utils;
mod type_expr;

/// Error returned when a delimited block is not properly closed.
pub use delimiter::UnclosedDelimiterError;
/// Extract text balanced between the specified opening and closing delimiters.
/// Prefer this over legacy names.
pub use delimiter::extract_delimited;
#[deprecated(since = "0.1.0", note = "Use extract_delimited instead")]
/// Legacy alias retained for backwards compatibility; schedule removal in the
/// next minor release.
pub use delimiter::extract_delimited as extract_parenthesized;

pub(crate) use outputs::parse_output_list;
pub(crate) use params::parse_name_type_pairs;
pub(crate) use relation::{primary_key_clause, relation_columns};
pub(crate) use token_utils::is_trivia;
pub(crate) use type_expr::parse_type_after_colon;
