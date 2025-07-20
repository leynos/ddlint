//! Parsing helpers shared across AST modules.
//!
//! This module provides small utilities for collecting parameter names and
//! types from the CST and for recursively parsing type expressions. Both the
//! `Function` and `Relation` nodes import these helpers so they can share the
//! same logic when interpreting their declarations. See
//! `docs/function-parsing-design.md` for an overview.

mod delimiter;
pub(crate) mod errors;
mod token_utils;
mod type_parsing;

pub use delimiter::extract_parenthesized;
pub(crate) use delimiter::paren_block_span;

pub(crate) use type_parsing::{parse_name_type_pairs, parse_output_list, parse_type_after_colon};
