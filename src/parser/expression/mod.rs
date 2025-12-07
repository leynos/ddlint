//! Pratt parser for `DDlog` expressions.
//!
//! Provides [`parse_expression`], a hand-rolled Pratt parser for arithmetic and
//! logical expressions used across the project. The implementation is split
//! across focused submodules for clarity: [`pratt`] hosts the core parser,
//! [`prefix`] dispatches prefix forms, [`literals`] handles literal tokens,
//! [`data_structures`] covers identifiers, structs, closures, and groupings,
//! [`control_flow`] parses `if`/`match`/`for`, and [`infix`] folds infix
//! operators. The public API is re-exported here.

mod control_flow;
mod data_structures;
mod delimiter_handling;
mod delimiter_utils;
mod infix;
mod literals;
pub(crate) mod numeric;
mod pattern_collection;
mod pratt;
mod prefix;
mod token_stream;

pub use numeric::parse_numeric_literal;
pub use pratt::parse_expression;
