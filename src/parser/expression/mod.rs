//! Pratt parser for `DDlog` expressions.
//!
//! Provides [`parse_expression`], a hand-rolled Pratt parser for arithmetic and
//! logical expressions used across the project. The implementation is split
//! across submodules for clarity: [`pratt`] hosts the core parser, [`prefix`]
//! handles literals and prefix forms, and [`infix`] folds infix operators. The
//! public API is re-exported here.

mod pratt;
mod prefix;
mod infix;

pub use pratt::parse_expression;

use pratt::Pratt;
