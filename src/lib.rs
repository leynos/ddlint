//! Library crate for ddlint.
//!
//! Exposes parser language definitions and lexical analysis functionality.

#![forbid(unsafe_code)]

pub mod language;
pub mod parser;
pub mod syntax_utils;
pub mod tokenizer;

#[cfg(any(test, feature = "test-support"))]
#[doc(hidden)]
pub mod test_util;

pub use language::{DdlogLanguage, SyntaxKind};
pub use parser::{Parsed, ast, parse};
pub use syntax_utils::parse_parenthesized_list;
pub use tokenizer::{Span, tokenize};
