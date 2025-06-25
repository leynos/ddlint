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
    errors: Vec<Simple<SyntaxKind>>,
    items: Vec<ast::Item>,
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

    /// Access parser errors collected during recovery.
    #[must_use]
    pub fn errors(&self) -> &[Simple<SyntaxKind>] {
        &self.errors
    }

    /// Access parsed items such as imports.
    #[must_use]
    pub fn items(&self) -> &[ast::Item] {
        &self.items
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
    let (items, errors) = parse_tokens(&tokens, src.len(), src);

    let green = build_green_tree(tokens, src);
    let root = ast::Root::from_green(green.clone());

    Parsed {
        green,
        root,
        errors,
        items,
    }
}

fn parse_tokens(
    tokens: &[(SyntaxKind, Span)],
    len: usize,
    src: &str,
) -> (Vec<ast::Item>, Vec<Simple<SyntaxKind>>) {
    let stream = Stream::from_iter(0..len, tokens.iter().cloned());
    let parser = decl(src)
        .repeated()
        .then_ignore(end())
        .map(|items| items.into_iter().flatten().collect());
    let (items, errors) = parser.parse_recovery(stream);

    (items.unwrap_or_default(), errors)
}

fn build_green_tree(tokens: Vec<(SyntaxKind, Span)>, src: &str) -> GreenNode {
    let mut builder = GreenNodeBuilder::new();
    builder.start_node(DdlogLanguage::kind_to_raw(SyntaxKind::N_DATALOG_PROGRAM));
    for (kind, span) in tokens {
        let text = src.get(span.clone()).map_or_else(
            || {
                warn!(
                    "token span {:?} out of bounds for source of length {}",
                    span,
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

fn parse_import(
    src: &str,
) -> impl Parser<SyntaxKind, ast::Import, Error = Simple<SyntaxKind>> + Clone + '_ {
    just(SyntaxKind::K_IMPORT)
        .then_ignore(select! { SyntaxKind::T_WHITESPACE => () }.repeated())
        .ignore_then(select!(|span| SyntaxKind::T_IDENT => span))
        .then_ignore(just(SyntaxKind::T_SEMI))
        .map(move |span: Span| {
            let text = src.get(span.clone()).unwrap_or("");
            ast::Import {
                module: text.to_string(),
            }
        })
        .boxed()
}

fn decl(
    src: &str,
) -> impl Parser<SyntaxKind, Option<ast::Item>, Error = Simple<SyntaxKind>> + Clone + '_ {
    parse_import(src)
        .map(ast::Item::Import)
        .map(Some)
        .or(any::<SyntaxKind, Simple<SyntaxKind>>().map(|_| None))
        .boxed()
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

        /// Get the text range covered by this root node.
        #[must_use]
        pub fn text_range(&self) -> rowan::TextRange {
            self.syntax.text_range()
        }

        /// Get the text content of this root node.
        #[must_use]
        pub fn text(&self) -> String {
            self.syntax.text().to_string()
        }
    }

    /// An import declaration.
    #[derive(Debug, Clone, PartialEq, Eq)]
    pub struct Import {
        /// The imported module path as text.
        pub module: String,
    }

    /// Top-level items recognised by the parser.
    #[derive(Debug, Clone, PartialEq, Eq)]
    pub enum Item {
        /// An import statement.
        Import(Import),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use chumsky::Parser;

    #[test]
    fn import_parses() {
        let src = "import foo;";
        let tokens = crate::tokenize(src);
        let stream = Stream::from_iter(0..src.len(), tokens.clone().into_iter());
        let (out, _errs) = parse_import(src).parse_recovery(stream);
        assert_eq!(
            out,
            Some(ast::Import {
                module: "foo".to_string(),
            })
        );
    }

    #[test]
    fn import_missing_semicolon_errors() {
        let src = "import foo";
        let tokens = crate::tokenize(src);
        let stream = Stream::from_iter(0..src.len(), tokens.clone().into_iter());
        let (out, errs) = parse_import(src).parse_recovery(stream);
        assert!(out.is_none());
        assert!(!errs.is_empty());
    }
}
