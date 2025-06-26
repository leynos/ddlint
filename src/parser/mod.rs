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
}

/// Parse the provided source string.
///
/// The function tokenises the source using [`tokenize`], then uses a minimal
/// `chumsky` parser to wrap those tokens into a CST. Syntactic error recovery
/// will insert `N_ERROR` nodes when grammar rules fail once they exist.
#[must_use]
pub fn parse(src: &str) -> Parsed {
    let tokens = tokenize(src);
    let (import_spans, errors) = parse_tokens(&tokens, src.len());

    let green = build_green_tree(tokens, src, &import_spans);
    let root = ast::Root::from_green(green.clone());

    Parsed {
        green,
        root,
        errors,
    }
}

fn parse_tokens(tokens: &[(SyntaxKind, Span)], len: usize) -> (Vec<Span>, Vec<Simple<SyntaxKind>>) {
    let stream = Stream::from_iter(0..len, tokens.iter().cloned());

    let ws = filter(|kind: &SyntaxKind| {
        matches!(kind, SyntaxKind::T_WHITESPACE | SyntaxKind::T_COMMENT)
    })
    .ignored();

    let ident = just(SyntaxKind::T_IDENT).ignored().padded_by(ws.repeated());

    let module_path = ident
        .then(
            just(SyntaxKind::T_COLON_COLON)
                .padded_by(ws.repeated())
                .ignore_then(ident)
                .repeated(),
        )
        .ignored();

    let alias = just(SyntaxKind::K_AS)
        .padded_by(ws.repeated())
        .ignore_then(ident);

    let imprt = just(SyntaxKind::K_IMPORT)
        .padded_by(ws.repeated())
        .ignore_then(module_path)
        .then(alias.or_not())
        .padded_by(ws.repeated())
        .map_with_span(|_, span| span);

    let parser = imprt.repeated().then_ignore(end());
    let (res, errors) = parser.parse_recovery(stream);
    (res.unwrap_or_default(), errors)
}

fn build_green_tree(tokens: Vec<(SyntaxKind, Span)>, src: &str, imports: &[Span]) -> GreenNode {
    let mut builder = GreenNodeBuilder::new();
    builder.start_node(DdlogLanguage::kind_to_raw(SyntaxKind::N_DATALOG_PROGRAM));
    // Iterator over the spans recorded for each `import` statement.  Each span
    // covers the entire statement so we can nest tokens inside an
    // `N_IMPORT_STMT` node while building the CST.
    let mut import_iter = imports.iter().peekable();
    for (kind, span) in tokens {
        // Advance to the next import span if this token lies after the end of
        // the current one.  Multiple tokens can share the same span, so we need
        // to skip spans that have already been closed.
        while let Some(next) = import_iter.peek() {
            if span.start >= next.end {
                import_iter.next();
            } else {
                break;
            }
        }
        // Begin an `N_IMPORT_STMT` node when this token marks the start of an
        // import span. Tokens emitted by the lexer appear in order, so equality
        // is sufficient here.
        if import_iter
            .peek()
            .is_some_and(|current| span.start == current.start)
        {
            builder.start_node(DdlogLanguage::kind_to_raw(SyntaxKind::N_IMPORT_STMT));
        }
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
        // Close an `N_IMPORT_STMT` when this token reaches or passes the end of
        // the active import span. Tokens can span multiple characters and may
        // end exactly on the boundary, so we use `>=` rather than equality.
        if import_iter
            .peek()
            .is_some_and(|current| span.end >= current.end)
        {
            builder.finish_node();
            import_iter.next();
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

        /// Collect all `import` statements under this root.
        #[must_use]
        pub fn imports(&self) -> Vec<Import> {
            self.syntax
                .children()
                .filter(|n| n.kind() == SyntaxKind::N_IMPORT_STMT)
                .map(|syntax| Import { syntax })
                .collect()
        }
    }

    /// Typed wrapper for an `import` statement.
    #[derive(Debug, Clone)]
    pub struct Import {
        pub(crate) syntax: SyntaxNode<DdlogLanguage>,
    }

    impl Import {
        /// Access the underlying syntax node.
        #[must_use]
        pub fn syntax(&self) -> &SyntaxNode<DdlogLanguage> {
            &self.syntax
        }

        /// The module path text as written in the source.
        #[must_use]
        pub fn path(&self) -> String {
            self.syntax
                .children_with_tokens()
                .skip_while(|e| !matches!(e.kind(), SyntaxKind::K_IMPORT))
                .skip(1)
                .take_while(|e| !matches!(e.kind(), SyntaxKind::K_AS))
                .filter_map(|e| match e {
                    rowan::NodeOrToken::Token(t) if t.kind() == SyntaxKind::T_IDENT => {
                        Some(t.text().to_string())
                    }
                    rowan::NodeOrToken::Token(t) if t.kind() == SyntaxKind::T_COLON_COLON => {
                        Some("::".to_string())
                    }
                    _ => None,
                })
                .collect::<String>()
        }

        /// The alias assigned with `as`, if any.
        #[must_use]
        pub fn alias(&self) -> Option<String> {
            self.syntax
                .children_with_tokens()
                .skip_while(|e| !matches!(e.kind(), SyntaxKind::K_AS))
                .skip(1)
                .find_map(|e| match e {
                    rowan::NodeOrToken::Token(t) if t.kind() == SyntaxKind::T_IDENT => {
                        Some(t.text().to_string())
                    }
                    _ => None,
                })
        }
    }
}
