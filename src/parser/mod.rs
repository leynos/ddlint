//! Chumsky-based parser producing a rowan CST.
//!
//! This module contains the entry point for parsing `DDlog` source code.
//! The grammar rules are not implemented yet. The parser currently tokenises
//! the input and wraps the tokens into a `rowan::GreenNode`. It lays down the
//! framework for integrating `chumsky` combinators and error recovery in later
//! stages.

use chumsky::Stream;
use chumsky::prelude::*;
use log::warn;
use rowan::{GreenNode, GreenNodeBuilder, Language};

use crate::{DdlogLanguage, Span, SyntaxKind, tokenize};

/// Result of a parse operation.
#[derive(Debug)]
pub struct Parsed {
    green: GreenNode,
    root: ast::Root,
}

impl Parsed {
    /// Access the `rowan` green tree.
    #[must_use]
    pub fn green(&self) -> &GreenNode {
        &self.green
    }

    /// Access the typed AST root.
    #[must_use]
    pub fn root(&self) -> &ast::Root {
        &self.root
    }
}

/// Parse the provided source string.
///
/// The function tokenises the source using [`tokenize`], then uses a minimal
/// `chumsky` parser to wrap those tokens into a CST. Syntactic error recovery
/// will insert `N_ERROR` nodes when grammar rules fail once they exist.
#[must_use]
pub fn parse(src: &str) -> Parsed {
    let tokens = tokenize(src);
    let parsed_kinds = parse_tokens(&tokens, src.len());
    debug_assert_eq!(
        parsed_kinds.len(),
        tokens.len(),
        "parser output token count differs from lexer",
    );

    let green = build_green_tree(tokens, src);
    let root = ast::Root::from_green(green.clone());

    Parsed { green, root }
}

fn parse_tokens(tokens: &[(SyntaxKind, Span)], len: usize) -> Vec<SyntaxKind> {
    let stream = Stream::from_iter(0..len, tokens.iter().cloned());

    let parser = any::<SyntaxKind, Simple<SyntaxKind>>()
        .repeated()
        .then_ignore(end());
    let (parsed_kinds, _errors) = parser.parse_recovery(stream);

    parsed_kinds.unwrap_or_default()
}

fn build_green_tree(tokens: Vec<(SyntaxKind, Span)>, src: &str) -> GreenNode {
    let mut builder = GreenNodeBuilder::new();
    builder.start_node(DdlogLanguage::kind_to_raw(SyntaxKind::N_DATALOG_PROGRAM));
    for (kind, span) in tokens {
        let log_span = span.clone();
        let text = src.get(span).map_or_else(
            || {
                warn!(
                    "token span {:?} out of bounds for source of length {}",
                    log_span,
                    src.len()
                );
                ""
            },
            |t| t,
        );
        if kind == SyntaxKind::N_ERROR {
            builder.start_node(DdlogLanguage::kind_to_raw(SyntaxKind::N_ERROR));
            builder.token(DdlogLanguage::kind_to_raw(kind), text);
            builder.finish_node();
        } else {
            builder.token(DdlogLanguage::kind_to_raw(kind), text);
        }
    }
    builder.finish_node();
    builder.finish()
}

pub mod ast {
    //! Minimal typed AST wrappers used by the parser.
    //!
    //! This layer will expand as grammar rules are implemented. For now it
    //! exposes only the root node so tests and higher layers can navigate the
    //! parsed CST.

    use rowan::{GreenNode, SyntaxNode};

    use crate::{DdlogLanguage, SyntaxKind};

    /// The root of a parsed `DDlog` file.
    ///
    /// Provides typed access to the syntax tree root node with methods
    /// for navigation and introspection.
    #[derive(Debug, Clone)]
    pub struct Root {
        pub(crate) syntax: SyntaxNode<DdlogLanguage>,
    }

    impl Root {
        /// Obtain the underlying syntax node.
        #[must_use]
        pub fn syntax(&self) -> &SyntaxNode<DdlogLanguage> {
            &self.syntax
        }

        /// Create a new `Root` from a green node.
        #[must_use]
        pub fn from_green(green: GreenNode) -> Self {
            Self {
                syntax: SyntaxNode::<DdlogLanguage>::new_root(green),
            }
        }

        /// The kind of this root node. Provided for completeness.
        #[must_use]
        pub fn kind(&self) -> SyntaxKind {
            self.syntax.kind()
        }
    }
}
