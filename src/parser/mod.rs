//! Chumsky-based parser producing a rowan CST.
//!
//! This module contains the entry point for parsing `DDlog` source code.
//! The parser tokenizes the input and wraps tokens into a `rowan::GreenNode`,
//! with support for parsing imports, typedefs, relations, indexes, functions,
//! and rules. It lays down the framework for integrating `chumsky` combinators
//! and error recovery in later stages.

use crate::tokenize_with_trivia;

#[macro_use]
mod lexer_helpers;

mod token_stream;

mod span_collector;

mod span_scanners;

mod span_scanner;
use span_scanner::parse_tokens;
mod cst_builder;
use cst_builder::build_green_tree;
mod delimiter;
pub mod expression;
mod expression_span;
pub use cst_builder::{Parsed, ParsedSpans};

/// Parse the provided source string.
///
/// The function tokenizes the source using [`tokenize_with_trivia`], then uses a minimal
/// `chumsky` parser to wrap those tokens into a CST. Syntactic error recovery
/// will insert `N_ERROR` nodes when grammar rules fail once they exist.
///
/// # Examples
///
/// ```rust,no_run
/// # use ddlint::parse;
/// let parsed = parse("input relation R(x: u32);");
/// assert!(parsed.errors().is_empty());
/// assert_eq!(parsed.root().relations().len(), 1);
/// ```
#[must_use]
pub fn parse(src: &str) -> Parsed {
    let tokens = tokenize_with_trivia(src);
    let (spans, errors) = parse_tokens(&tokens, src);

    let green = build_green_tree(&tokens, src, &spans);
    let root = ast::Root::from_green(green.clone());

    Parsed::new(green, root, errors)
}

pub mod ast;

#[cfg(test)]
mod tests;
