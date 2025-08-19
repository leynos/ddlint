//! Parsing helpers shared across AST modules.
//!
//! This module provides small utilities for collecting parameter names and
//! types from the CST, recursively parsing type expressions, and parsing
//! transformer output identifiers. `Function`, `Relation`, and `Transformer`
//! nodes import these helpers so they can share the same logic when
//! interpreting their declarations. See
//! `docs/function-parsing-design.md` for an overview.

mod delimiter;
pub(crate) mod errors;
mod outputs;
mod params;
mod token_utils;
mod type_expr;

pub use delimiter::extract_parenthesized;

pub(crate) use outputs::parse_output_list;
pub(crate) use params::parse_name_type_pairs;
pub(crate) use token_utils::is_trivia;
pub(crate) use type_expr::parse_type_after_colon;
