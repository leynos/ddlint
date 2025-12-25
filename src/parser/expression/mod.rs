//! Pratt parser for `DDlog` expressions.
//!
//! Provides [`parse_expression`], a hand-rolled Pratt parser for arithmetic and
//! logical expressions used across the project. The implementation is split
//! across focused submodules for clarity: [`pratt`] hosts the core parser,
//! [`prefix`] dispatches prefix forms, [`literals`] handles literal tokens,
//! [`data_structures`] covers identifiers, structs, closures, and groupings,
//! [`collections`] parses vector and map literals, [`control_flow`] parses
//! `if`/`match`/`for`, [`infix`] folds infix operators, and [`numeric`] parses
//! width-qualified numeric literals (e.g., `8'hFF`, `16'sd-1`, `3.14'f32`). The
//! public API is re-exported here, including [`parse_numeric_literal`] for
//! standalone numeric literal parsing.

mod collections;
mod control_flow;
mod data_structures;
mod delimiter_handling;
mod delimiter_utils;
mod infix;
mod literals;
pub(crate) mod numeric;
mod numeric_newtypes;
mod pattern_collection;
mod pratt;
mod prefix;
mod token_stream;

pub use numeric::parse_numeric_literal;
pub use pratt::parse_expression;
