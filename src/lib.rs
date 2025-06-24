//! Library crate for ddlint.
//!
//! Exposes parser language definitions and lexical analysis functionality.

#![forbid(unsafe_code)]

pub mod language;
pub mod tokenizer;

pub use language::{DdlogLanguage, SyntaxKind};
pub use tokenizer::{Span, tokenize};
