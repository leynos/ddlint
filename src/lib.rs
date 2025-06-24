//! Library crate for ddlint.
//!
//! Currently exposes only the parser language definitions.

#![forbid(unsafe_code)]

pub mod language;
pub mod tokenizer;

pub use language::{DdlogLanguage, SyntaxKind};
pub use tokenizer::{Span, tokenize};
