//! Library crate for ddlint.
//!
//! Exposes parser language definitions and lexical analysis functionality.

#![forbid(unsafe_code)]

pub mod language;
pub mod linter;
pub mod parser;
pub mod sema;
pub mod syntax_utils;
pub mod tokenizer;

// Only expose test utilities to tests and opt-in consumers.
#[cfg(any(test, feature = "test-support"))]
#[doc(hidden)]
pub mod test_util;

pub use language::{DdlogLanguage, SyntaxKind};
pub use parser::{Parsed, ast, parse};
/// Re-exported for macro-generated rule handlers and downstream CST consumers
/// so callers can use `ddlint`'s public syntax types without a direct `rowan`
/// dependency.
pub use rowan::{SyntaxNode, SyntaxToken};
pub use syntax_utils::parse_parenthesized_list;
pub use tokenizer::{Span, tokenize_with_trivia, tokenize_without_trivia};
