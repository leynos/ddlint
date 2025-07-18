//! CST construction utilities.
//!
//! Provides [`Parsed`], [`ParsedSpans`] and [`build_green_tree`].

use chumsky::error::Simple;
use rowan::GreenNode;

use crate::SyntaxKind;

mod spans;
mod tree;

pub use self::spans::ParsedSpans;
pub(crate) use self::tree::build_green_tree;

/// Result of a parse operation.
#[derive(Debug)]
pub struct Parsed {
    green: GreenNode,
    root: super::ast::Root,
    errors: Vec<Simple<SyntaxKind>>,
}

impl Parsed {
    pub(super) fn new(
        green: GreenNode,
        root: super::ast::Root,
        errors: Vec<Simple<SyntaxKind>>,
    ) -> Self {
        Self {
            green,
            root,
            errors,
        }
    }

    /// Access the `rowan` green tree.
    #[must_use]
    pub fn green(&self) -> &GreenNode {
        &self.green
    }

    /// Access the typed AST root.
    #[must_use]
    pub fn root(&self) -> &super::ast::Root {
        &self.root
    }

    /// Access parser errors collected during recovery.
    #[must_use]
    pub fn errors(&self) -> &[Simple<SyntaxKind>] {
        &self.errors
    }
}
