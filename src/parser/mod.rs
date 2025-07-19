//! Chumsky-based parser producing a rowan CST.
//!
//! This module contains the entry point for parsing `DDlog` source code.
//! The parser tokenises the input and wraps tokens into a `rowan::GreenNode`,
//! with support for parsing imports, typedefs, relations, indexes, functions,
//! and rules. It lays down the framework for integrating `chumsky` combinators
//! and error recovery in later stages.

use crate::tokenize;

#[macro_use]
mod lexer_helpers;

mod token_stream;

mod span_collector;

mod span_scanner;
use span_scanner::parse_tokens;
mod cst_builder;
use cst_builder::build_green_tree;
pub use cst_builder::{Parsed, ParsedSpans};

/// Parse the provided source string.
///
/// The function tokenises the source using [`tokenize`], then uses a minimal
/// `chumsky` parser to wrap those tokens into a CST. Syntactic error recovery
/// will insert `N_ERROR` nodes when grammar rules fail once they exist.
///
/// # Examples
///
/// ```rust,no_run
/// use ddlint::parse;
///
/// let parsed = parse("input relation R(x: u32);");
/// assert!(parsed.errors().is_empty());
/// assert_eq!(parsed.root().relations().len(), 1);
/// ```
#[must_use]
pub fn parse(src: &str) -> Parsed {
    let tokens = tokenize(src);
    let (spans, errors) = parse_tokens(&tokens, src);

    let green = build_green_tree(&tokens, src, &spans);
    let root = ast::Root::from_green(green.clone());

    Parsed::new(green, root, errors)
}

pub mod ast {
    //! Minimal typed AST wrappers used by the parser.
    //!
    //! This layer will expand as grammar rules are implemented. For now it
    //! exposes only the root node so tests and higher layers can navigate the
    //! parsed CST.

    use rowan::{GreenNode, SyntaxElement, SyntaxNode};

    use crate::{DdlogLanguage, SyntaxKind};

    /// Common interface for AST wrappers.
    ///
    /// The trait exposes the underlying [`rowan::SyntaxNode`] so callers can
    /// navigate the CST without losing type information.
    ///
    /// # Examples
    ///
    /// ```no_run
    /// use ddlint::{ast::{AstNode, Import}, parse, SyntaxKind};
    /// let parsed = parse("import foo::bar;");
    /// let import = parsed.root().imports().first().unwrap();
    /// assert_eq!(import.syntax().kind(), SyntaxKind::N_IMPORT_STMT);
    /// ```
    #[cfg_attr(
        not(test),
        expect(dead_code, reason = "primarily exercised through test modules")
    )]
    // This trait enables inspection of AST nodes. It is mainly used in tests,
    // but remains part of the crate interface for future tooling.
    pub(crate) trait AstNode {
        /// Access the underlying syntax node.
        fn syntax(&self) -> &SyntaxNode<DdlogLanguage>;
    }

    macro_rules! impl_ast_node {
        ($ty:ty) => {
            impl AstNode for $ty {
                fn syntax(&self) -> &SyntaxNode<DdlogLanguage> {
                    &self.syntax
                }
            }
        };
    }

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
        ///
        /// # Examples
        ///
        /// ```no_run
        /// # use crate::parser;
        /// let parsed = parser::parse("import foo::bar as baz;");
        /// let imports = parsed.root().imports();
        /// assert_eq!(imports.len(), 1);
        /// ```
        #[must_use]
        pub fn imports(&self) -> Vec<Import> {
            self.syntax
                .children()
                .filter(|n| n.kind() == SyntaxKind::N_IMPORT_STMT)
                .map(|syntax| Import { syntax })
                .collect()
        }

        /// Collect all `typedef` declarations under this root.
        ///
        /// # Examples
        ///
        /// ```no_run
        /// # use crate::parser;
        /// let parsed = parser::parse("typedef Foo = Bar;");
        /// let defs = parsed.root().type_defs();
        /// assert_eq!(defs.len(), 1);
        /// ```
        #[must_use]
        pub fn type_defs(&self) -> Vec<TypeDef> {
            self.syntax
                .children()
                .filter(|n| n.kind() == SyntaxKind::N_TYPE_DEF)
                .map(|syntax| TypeDef { syntax })
                .collect()
        }

        /// Collect all relation declarations under this root.
        ///
        /// # Examples
        ///
        /// ```no_run
        /// # use crate::parser;
        /// let parsed = parser::parse("relation User(id: u32)");
        /// let relations = parsed.root().relations();
        /// assert_eq!(relations.len(), 1);
        /// ```
        #[must_use]
        pub fn relations(&self) -> Vec<Relation> {
            self.syntax
                .children()
                .filter(|n| n.kind() == SyntaxKind::N_RELATION_DECL)
                .map(|syntax| Relation { syntax })
                .collect()
        }

        /// Collect all index declarations under this root.
        ///
        /// # Examples
        ///
        /// ```no_run
        /// # use crate::parser;
        /// let parsed = parser::parse("index Idx on User(id)");
        /// let indexes = parsed.root().indexes();
        /// assert_eq!(indexes.len(), 1);
        /// ```
        #[must_use]
        pub fn indexes(&self) -> Vec<Index> {
            self.syntax
                .children()
                .filter(|n| n.kind() == SyntaxKind::N_INDEX)
                .map(|syntax| Index { syntax })
                .collect()
        }

        /// Collect all function declarations under this root.
        ///
        /// # Examples
        ///
        /// ```no_run
        /// # use crate::parser;
        /// let parsed = parser::parse("function f() {}");
        /// let funcs = parsed.root().functions();
        /// assert_eq!(funcs.len(), 1);
        /// ```
        #[must_use]
        pub fn functions(&self) -> Vec<Function> {
            self.syntax
                .children()
                .filter(|n| n.kind() == SyntaxKind::N_FUNCTION)
                .map(|syntax| Function { syntax })
                .collect()
        }

        /// Collect all transformer declarations under this root.
        ///
        /// # Examples
        ///
        /// ```no_run
        /// # use crate::parser;
        /// let parsed = parser::parse("extern transformer t(i: In): Out");
        /// let ts = parsed.root().transformers();
        /// assert_eq!(ts.len(), 1);
        /// ```
        #[must_use]
        pub fn transformers(&self) -> Vec<Transformer> {
            self.syntax
                .children()
                .filter(|n| n.kind() == SyntaxKind::N_TRANSFORMER)
                .map(|syntax| Transformer { syntax })
                .collect()
        }

        /// Collect all rule declarations under this root.
        ///
        /// # Examples
        ///
        /// ```no_run
        /// # use crate::parser;
        /// let parsed = parser::parse("A(x) :- B(x).");
        /// let rules = parsed.root().rules();
        /// assert_eq!(rules.len(), 1);
        /// ```
        #[must_use]
        pub fn rules(&self) -> Vec<Rule> {
            self.syntax
                .children()
                .filter(|n| n.kind() == SyntaxKind::N_RULE)
                .map(|syntax| Rule { syntax })
                .collect()
        }
    }

    /// Typed wrapper for an `import` statement.
    #[derive(Debug, Clone)]
    pub struct Import {
        pub(crate) syntax: SyntaxNode<DdlogLanguage>,
    }

    impl Import {
        /// The module path text as written in the source.
        ///
        /// # Examples
        ///
        /// ```no_run
        /// # use crate::parser;
        /// let parsed = parser::parse("import foo::bar as baz;");
        /// let import = parsed.root().imports().first().unwrap();
        /// assert_eq!(import.path(), "foo::bar");
        /// ```
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
        ///
        /// # Examples
        ///
        /// ```no_run
        /// # use crate::parser;
        /// let parsed = parser::parse("import foo as bar;");
        /// let import = parsed.root().imports().first().unwrap();
        /// assert_eq!(import.alias(), Some("bar".to_string()));
        /// ```
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

    impl_ast_node!(Import);

    /// Typed wrapper for a `typedef` or `extern type` declaration.
    #[derive(Debug, Clone)]
    pub struct TypeDef {
        pub(crate) syntax: SyntaxNode<DdlogLanguage>,
    }

    impl TypeDef {
        /// Name of the defined type.
        ///
        /// # Examples
        ///
        /// ```no_run
        /// # use crate::parser;
        /// let parsed = parser::parse("typedef Foo = Bar;");
        /// let td = parsed.root().type_defs().first().unwrap();
        /// assert_eq!(td.name(), Some("Foo".to_string()));
        /// ```
        #[must_use]
        pub fn name(&self) -> Option<String> {
            let mut iter = self.syntax.children_with_tokens().peekable();
            if !skip_to_typedef_keyword(&mut iter) {
                return None;
            }
            take_first_ident(iter)
        }

        /// Whether this declaration is `extern`.
        ///
        /// # Examples
        ///
        /// ```no_run
        /// # use crate::parser;
        /// let parsed = parser::parse("extern type Baz;");
        /// let td = parsed.root().type_defs().first().unwrap();
        /// assert!(td.is_extern());
        /// ```
        #[must_use]
        pub fn is_extern(&self) -> bool {
            self.syntax
                .children_with_tokens()
                .any(|e| e.kind() == SyntaxKind::K_EXTERN)
        }
    }

    impl_ast_node!(TypeDef);

    /// Advance the iterator until `predicate` returns `true` for a token kind.
    fn skip_to_match(
        iter: &mut impl Iterator<Item = rowan::SyntaxElement<DdlogLanguage>>,
        predicate: impl Fn(SyntaxKind) -> bool,
    ) -> bool {
        for e in iter.by_ref() {
            let kind = e.kind();
            if kind == SyntaxKind::K_EXTERN {
                continue;
            }
            if predicate(kind) {
                return true;
            }
        }
        false
    }

    /// Advance the iterator until `typedef` or `type` is encountered.
    fn skip_to_typedef_keyword(
        iter: &mut impl Iterator<Item = rowan::SyntaxElement<DdlogLanguage>>,
    ) -> bool {
        skip_to_match(iter, |k| {
            matches!(k, SyntaxKind::K_TYPEDEF | SyntaxKind::K_TYPE)
        })
    }

    /// Advance the iterator until `transformer` is encountered.
    fn skip_to_transformer_keyword(
        iter: &mut impl Iterator<Item = rowan::SyntaxElement<DdlogLanguage>>,
    ) -> bool {
        skip_to_match(iter, |k| k == SyntaxKind::K_TRANSFORMER)
    }

    fn take_first_ident(
        iter: impl Iterator<Item = rowan::SyntaxElement<DdlogLanguage>>,
    ) -> Option<String> {
        use rowan::NodeOrToken;
        for e in iter {
            match e {
                NodeOrToken::Token(t) if t.kind() == SyntaxKind::T_IDENT => {
                    return Some(t.text().to_string());
                }
                NodeOrToken::Token(t)
                    if matches!(t.kind(), SyntaxKind::T_WHITESPACE | SyntaxKind::T_COMMENT) => {}
                _ => return None,
            }
        }
        None
    }

    pub(super) fn skip_whitespace_and_comments<I>(iter: &mut std::iter::Peekable<I>)
    where
        I: Iterator<Item = SyntaxElement<DdlogLanguage>>,
    {
        while matches!(
            iter.peek().map(SyntaxElement::kind),
            Some(SyntaxKind::T_WHITESPACE | SyntaxKind::T_COMMENT)
        ) {
            iter.next();
        }
    }

    pub(super) mod parse_utils;
    use parse_utils::{parse_name_type_pairs, parse_output_list, parse_type_after_colon};

    /// Typed wrapper for a relation declaration.
    #[derive(Debug, Clone)]
    pub struct Relation {
        pub(crate) syntax: SyntaxNode<DdlogLanguage>,
    }

    impl Relation {
        /// Name of the relation if present.
        ///
        /// # Examples
        ///
        /// ```no_run
        /// # use crate::parser;
        /// let parsed = parser::parse("relation Person(name: string)");
        /// let rel = parsed.root().relations().first().unwrap();
        /// assert_eq!(rel.name(), Some("Person".to_string()));
        /// ```
        #[must_use]
        pub fn name(&self) -> Option<String> {
            self.syntax
                .children_with_tokens()
                .skip_while(|e| !matches!(e.kind(), SyntaxKind::K_RELATION))
                .skip(1)
                .find_map(|e| match e {
                    rowan::NodeOrToken::Token(t) if t.kind() == SyntaxKind::T_IDENT => {
                        Some(t.text().to_string())
                    }
                    _ => None,
                })
        }

        /// Whether the relation is declared as `input`.
        ///
        /// # Examples
        ///
        /// ```no_run
        /// # use crate::parser;
        /// let parsed = parser::parse("input relation Data(x: i32)");
        /// let rel = parsed.root().relations().first().unwrap();
        /// assert!(rel.is_input());
        /// ```
        #[must_use]
        pub fn is_input(&self) -> bool {
            self.syntax
                .children_with_tokens()
                .any(|e| e.kind() == SyntaxKind::K_INPUT)
        }

        /// Returns `true` if the relation is declared with the `output` keyword.
        ///
        /// # Examples
        ///
        /// ```no_run
        /// # use crate::parser::ast::{Relation, Root};
        /// # let src = "output MyRel(x: u32)";
        /// # let parsed = crate::parser::parse(src);
        /// # let rel = parsed.root.relations().next().unwrap();
        /// assert!(rel.is_output());
        /// ```
        #[must_use]
        pub fn is_output(&self) -> bool {
            self.syntax
                .children_with_tokens()
                .any(|e| e.kind() == SyntaxKind::K_OUTPUT)
        }

        /// Returns the columns declared for the relation as name/type pairs.
        ///
        /// Delimiter errors encountered during parsing are currently ignored,
        /// but this behaviour may change in future versions to surface such diagnostics.
        ///
        /// # Examples
        ///
        /// ```no_run
        /// # use your_crate::parser::ast::{Relation, Root};
        /// # let src = "relation Person(name: string, age: integer)";
        /// # let parsed = your_crate::parser::parse(src);
        /// # let root = parsed.root();
        /// # let relation = root.relations().next().unwrap();
        /// let columns = relation.columns();
        /// assert_eq!(columns, vec![("name".to_string(), "string".to_string()), ("age".to_string(), "integer".to_string())]);
        /// ```
        #[must_use]
        pub fn columns(&self) -> Vec<(String, String)> {
            let (pairs, errors) = parse_name_type_pairs(self.syntax.children_with_tokens());
            // Delimiter errors are ignored for now. Future versions may surface them.
            let _ = errors;
            pairs
        }

        /// Primary key column names if specified.
        ///
        /// # Examples
        ///
        /// ```no_run
        /// # use crate::parser;
        /// let src = "relation Person(name: string, age: u32) primary key(name)";
        /// let rel = parser::parse(src).root().relations().first().unwrap();
        /// assert_eq!(rel.primary_key(), Some(vec!["name".to_string()]));
        /// ```
        #[must_use]
        pub fn primary_key(&self) -> Option<Vec<String>> {
            use rowan::NodeOrToken;

            let mut iter = self.syntax.children_with_tokens().peekable();
            // Skip to the first '(' and consume column list
            for e in &mut iter {
                if e.kind() == SyntaxKind::T_LPAREN {
                    break;
                }
            }
            let mut depth = 1usize;
            for e in iter.by_ref() {
                match e.kind() {
                    SyntaxKind::T_LPAREN => depth += 1,
                    SyntaxKind::T_RPAREN => {
                        depth -= 1;
                        if depth == 0 {
                            break;
                        }
                    }
                    _ => {}
                }
            }

            // Skip whitespace/comments
            skip_whitespace_and_comments(&mut iter);

            match iter.next() {
                Some(NodeOrToken::Token(t))
                    if t.kind() == SyntaxKind::T_IDENT && t.text() == "primary" => {}
                _ => return None,
            }

            skip_whitespace_and_comments(&mut iter);

            match iter.next() {
                Some(NodeOrToken::Token(t))
                    if t.kind() == SyntaxKind::T_IDENT && t.text() == "key" => {}
                _ => return None,
            }

            skip_whitespace_and_comments(&mut iter);

            if !matches!(
                iter.peek().map(rowan::SyntaxElement::kind),
                Some(SyntaxKind::T_LPAREN)
            ) {
                return None;
            }
            iter.next(); // consume '('
            depth = 1;
            let mut buf = String::new();
            for e in iter.by_ref() {
                match e {
                    NodeOrToken::Token(t) => match t.kind() {
                        SyntaxKind::T_LPAREN => {
                            depth += 1;
                            buf.push_str(t.text());
                        }
                        SyntaxKind::T_RPAREN => {
                            if depth == 1 {
                                break;
                            }
                            depth -= 1;
                            buf.push_str(t.text());
                        }
                        _ => buf.push_str(t.text()),
                    },
                    NodeOrToken::Node(n) => buf.push_str(&n.text().to_string()),
                }
                if depth == 0 {
                    break;
                }
            }

            let keys = buf
                .split(',')
                .map(|s| s.trim().to_string())
                .filter(|s| !s.is_empty())
                .collect::<Vec<_>>();
            if keys.is_empty() { None } else { Some(keys) }
        }
    }

    impl_ast_node!(Relation);

    /// Typed wrapper for an index declaration.
    #[derive(Debug, Clone)]
    pub struct Index {
        pub(crate) syntax: SyntaxNode<DdlogLanguage>,
    }

    impl Index {
        /// Name of the index if present.
        ///
        /// # Examples
        ///
        /// ```no_run
        /// # use crate::parser;
        /// let parsed = parser::parse("index Idx on User(id)");
        /// let idx = parsed.root().indexes().first().unwrap();
        /// assert_eq!(idx.name(), Some("Idx".to_string()));
        /// ```
        #[must_use]
        pub fn name(&self) -> Option<String> {
            self.syntax
                .children_with_tokens()
                .skip_while(|e| !matches!(e.kind(), SyntaxKind::K_INDEX))
                .skip(1)
                .find_map(|e| match e {
                    rowan::NodeOrToken::Token(t) if t.kind() == SyntaxKind::T_IDENT => {
                        Some(t.text().to_string())
                    }
                    _ => None,
                })
        }

        /// Target relation name.
        ///
        /// # Examples
        ///
        /// ```no_run
        /// # use crate::parser;
        /// let parsed = parser::parse("index Idx on User(id)");
        /// let idx = parsed.root().indexes().first().unwrap();
        /// assert_eq!(idx.relation(), Some("User".to_string()));
        /// ```
        #[must_use]
        pub fn relation(&self) -> Option<String> {
            self.syntax
                .children_with_tokens()
                .skip_while(|e| !matches!(e.kind(), SyntaxKind::K_ON))
                .skip(1)
                .find_map(|e| match e {
                    rowan::NodeOrToken::Token(t) if t.kind() == SyntaxKind::T_IDENT => {
                        Some(t.text().to_string())
                    }
                    _ => None,
                })
        }

        /// Column expressions included in the index.
        ///
        /// The method collects the text of each column expression, allowing for
        /// nested parentheses in cases like `lower(name)` or
        /// `func(col, other(col2))`.
        ///
        /// # Examples
        ///
        /// ```no_run
        /// # use crate::parser;
        /// let parsed = parser::parse("index Idx on User(lower(name))");
        /// let idx = parsed.root().indexes().first().unwrap();
        /// assert_eq!(idx.columns(), vec!["lower(name)".to_string()]);
        /// ```
        #[must_use]
        pub fn columns(&self) -> Vec<String> {
            use rowan::NodeOrToken;

            let mut iter = self.syntax.children_with_tokens().peekable();

            // Skip tokens up to the opening parenthesis after the relation name
            for e in &mut iter {
                if e.kind() == SyntaxKind::T_LPAREN {
                    break;
                }
            }

            let mut cols = Vec::new();
            let mut buf = String::new();
            let mut depth = 0usize;

            for e in iter {
                match e {
                    NodeOrToken::Token(t) => match t.kind() {
                        SyntaxKind::T_LPAREN => {
                            depth += 1;
                            buf.push_str(t.text());
                        }
                        SyntaxKind::T_RPAREN => {
                            if depth == 0 {
                                let col = buf.trim();
                                if !col.is_empty() {
                                    cols.push(col.to_string());
                                }
                                break;
                            }
                            depth -= 1;
                            buf.push_str(t.text());
                        }
                        SyntaxKind::T_COMMA if depth == 0 => {
                            let col = buf.trim();
                            if !col.is_empty() {
                                cols.push(col.to_string());
                            }
                            buf.clear();
                        }
                        _ => buf.push_str(t.text()),
                    },
                    NodeOrToken::Node(n) => buf.push_str(&n.text().to_string()),
                }
            }

            cols
        }
    }

    impl_ast_node!(Index);

    /// Typed wrapper for a rule declaration.
    #[derive(Debug, Clone)]
    pub struct Rule {
        pub(crate) syntax: SyntaxNode<DdlogLanguage>,
    }

    impl Rule {
        /// Text of the rule head atom.
        ///
        /// # Examples
        ///
        /// ```no_run
        /// # use crate::parser;
        /// let parsed = parser::parse("A(x) :- B(x).");
        /// let rule = parsed.root().rules().first().unwrap();
        /// assert_eq!(rule.head(), Some("A(x)".to_string()));
        /// ```
        #[must_use]
        pub fn head(&self) -> Option<String> {
            use rowan::NodeOrToken;

            let mut buf = String::new();
            for e in self.syntax.children_with_tokens() {
                match e {
                    NodeOrToken::Token(t) => match t.kind() {
                        SyntaxKind::T_IMPLIES | SyntaxKind::T_DOT => break,
                        _ => buf.push_str(t.text()),
                    },
                    NodeOrToken::Node(n) => buf.push_str(&n.text().to_string()),
                }
            }

            let text = buf.trim();
            if text.is_empty() {
                None
            } else {
                Some(text.to_string())
            }
        }

        /// Text of each body literal in order of appearance.
        ///
        /// # Examples
        ///
        /// ```no_run
        /// # use crate::parser;
        /// let parsed = parser::parse("A(x) :- B(x), C(y).");
        /// let rule = parsed.root().rules().first().unwrap();
        /// assert_eq!(rule.body_literals(), vec!["B(x)".to_string(), "C(y)".to_string()]);
        /// ```
        #[must_use]
        pub fn body_literals(&self) -> Vec<String> {
            use rowan::NodeOrToken;

            let mut iter = self
                .syntax
                .children_with_tokens()
                .skip_while(|e| e.kind() != SyntaxKind::T_IMPLIES);

            // Skip the ':-' token if present
            if matches!(iter.next().map(|e| e.kind()), Some(SyntaxKind::T_IMPLIES)) {
                let mut buf = String::new();
                let mut lits = Vec::new();
                for e in iter {
                    match e {
                        NodeOrToken::Token(t) => match t.kind() {
                            SyntaxKind::T_COMMA => {
                                let lit = buf.trim();
                                if !lit.is_empty() {
                                    lits.push(lit.to_string());
                                }
                                buf.clear();
                            }
                            SyntaxKind::T_DOT => {
                                let lit = buf.trim();
                                if !lit.is_empty() {
                                    lits.push(lit.to_string());
                                }
                                break;
                            }
                            _ => buf.push_str(t.text()),
                        },
                        NodeOrToken::Node(n) => buf.push_str(&n.text().to_string()),
                    }
                }
                lits
            } else {
                Vec::new()
            }
        }
    }

    impl_ast_node!(Rule);

    /// Typed wrapper for a function declaration or definition.
    #[derive(Debug, Clone)]
    pub struct Function {
        pub(crate) syntax: SyntaxNode<DdlogLanguage>,
    }

    impl Function {
        /// Name of the function if present.
        ///
        /// # Examples
        ///
        /// ```no_run
        /// # use crate::parser;
        /// let parsed = parser::parse("function foo() {}");
        /// let func = parsed.root().functions().first().unwrap();
        /// assert_eq!(func.name(), Some("foo".to_string()));
        /// ```
        #[must_use]
        pub fn name(&self) -> Option<String> {
            self.syntax
                .children_with_tokens()
                .skip_while(|e| !matches!(e.kind(), SyntaxKind::K_FUNCTION))
                .skip(1)
                .find_map(|e| match e {
                    rowan::NodeOrToken::Token(t) if t.kind() == SyntaxKind::T_IDENT => {
                        Some(t.text().to_string())
                    }
                    _ => None,
                })
        }

        /// Returns `true` if the function is declared with the `extern` keyword.
        ///
        /// # Examples
        ///
        /// ```no_run
        /// # use crate::parser::ast::{Function, Root};
        /// # let src = "extern function foo(x: i32): i32;";
        /// # let parsed = crate::parser::parse(src);
        /// # let root = parsed.root();
        /// # let func = root.functions().next().unwrap();
        /// assert!(func.is_extern());
        /// ```
        #[must_use]
        pub fn is_extern(&self) -> bool {
            self.syntax
                .children_with_tokens()
                .any(|e| e.kind() == SyntaxKind::K_EXTERN)
        }

        /// Returns the function parameters as pairs of parameter name and type.
        ///
        /// Delimiter errors encountered during parsing are currently ignored,
        /// but this behaviour may change in future versions to surface such diagnostics.
        ///
        /// # Examples
        ///
        /// ```no_run
        /// # use crate::parser::ast::{Function, Root};
        /// # let src = "function foo(x: i32, y: string): bool { ... }";
        /// # let parsed = crate::parser::parse(src);
        /// # let root = parsed.root();
        /// # let func = root.functions().next().unwrap();
        /// let params = func.parameters();
        /// assert_eq!(params, vec![("x".to_string(), "i32".to_string()), ("y".to_string(), "string".to_string())]);
        /// ```
        #[must_use]
        pub fn parameters(&self) -> Vec<(String, String)> {
            let (pairs, errors) = parse_name_type_pairs(self.syntax.children_with_tokens());
            // Delimiter errors are ignored for now. Future versions may surface them.
            let _ = errors;
            pairs
        }

        /// Return type text if specified.
        ///
        /// # Examples
        ///
        /// ```no_run
        /// # use crate::parser;
        /// let parsed = parser::parse("function f(): bool {}");
        /// let func = parsed.root().functions().first().unwrap();
        /// assert_eq!(func.return_type(), Some("bool".to_string()));
        /// ```
        #[must_use]
        pub fn return_type(&self) -> Option<String> {
            let mut iter = self.syntax.children_with_tokens().peekable();
            // Skip to ')' after parameters
            let mut depth = 0usize;
            for e in &mut iter {
                match e.kind() {
                    SyntaxKind::T_LPAREN => depth += 1,
                    SyntaxKind::T_RPAREN => {
                        depth -= 1;
                        if depth == 0 {
                            break;
                        }
                    }
                    _ => {}
                }
            }

            parse_type_after_colon(&mut iter)
        }
    }

    impl_ast_node!(Function);

    /// Typed wrapper for a transformer declaration.
    #[derive(Debug, Clone)]
    pub struct Transformer {
        pub(crate) syntax: SyntaxNode<DdlogLanguage>,
    }

    impl Transformer {
        /// Name of the transformer if present.
        ///
        /// # Examples
        ///
        /// ```no_run
        /// # use crate::parser;
        /// let parsed = parser::parse("extern transformer t(x: In): Out");
        /// let tr = parsed.root().transformers().first().unwrap();
        /// assert_eq!(tr.name(), Some("t".to_string()));
        /// ```
        #[must_use]
        pub fn name(&self) -> Option<String> {
            let mut iter = self.syntax.children_with_tokens();
            if !skip_to_transformer_keyword(&mut iter) {
                return None;
            }
            take_first_ident(iter)
        }

        /// Input relations as pairs of name and type.
        ///
        /// # Examples
        ///
        /// ```no_run
        /// # use crate::parser;
        /// let parsed = parser::parse("extern transformer t(x: In): Out");
        /// let tr = parsed.root().transformers().first().unwrap();
        /// assert_eq!(tr.inputs(), vec![("x".to_string(), "In".to_string())]);
        /// ```
        #[must_use]
        pub fn inputs(&self) -> Vec<(String, String)> {
            let (pairs, _errors) = parse_name_type_pairs(self.syntax.children_with_tokens());
            pairs
        }

        /// Output relation names.
        ///
        /// # Examples
        ///
        /// ```no_run
        /// # use crate::parser;
        /// let parsed = parser::parse("extern transformer t(x: In): Out1, Out2");
        /// let tr = parsed.root().transformers().first().unwrap();
        /// assert_eq!(tr.outputs(), vec!["Out1".to_string(), "Out2".to_string()]);
        /// ```
        #[must_use]
        pub fn outputs(&self) -> Vec<String> {
            parse_output_list(self.syntax.children_with_tokens())
        }
    }

    impl_ast_node!(Transformer);
}

#[cfg(test)]
mod tests {
    mod parser;
    use super::token_stream::TokenStream;
    use crate::{SyntaxKind, tokenize};
    use rstest::rstest;

    /// Tests that `skip_until` advances the token stream cursor past the specified span end.
    ///
    /// # Examples
    ///
    /// ```no_run
    /// let src = "import foo\n";
    /// let tokens = tokenize(src);
    /// let mut stream = TokenStream::new(&tokens, src);
    /// let end = stream.line_end(0);
    /// stream.skip_until(end);
    /// assert_eq!(stream.cursor(), tokens.len());
    /// ```
    #[rstest]
    fn skip_until_advances_past_span() {
        let src = "import foo\n";
        let tokens = tokenize(src);
        let mut stream = TokenStream::new(&tokens, src);
        let end = stream.line_end(0);
        stream.skip_until(end);
        assert_eq!(stream.cursor(), tokens.len());
    }

    /// Tests that `TokenStream::line_end` returns the position immediately after the end of the current line.
    ///
    /// # Examples
    ///
    /// ```no_run
    /// let src = "typedef A = string\nnext";
    /// let tokens = tokenize(src);
    /// let stream = TokenStream::new(&tokens, src);
    /// let start = 1; // token after 'typedef'
    /// let end = stream.line_end(start);
    /// let newline = src.find('\n').unwrap_or_else(|| panic!("newline missing"));
    /// assert_eq!(end, newline + 1);
    /// ```
    #[rstest]
    fn line_end_returns_span_end() {
        let src = "typedef A = string\nnext";
        let tokens = tokenize(src);
        let stream = TokenStream::new(&tokens, src);
        let start = 1; // token after 'typedef'
        let end = stream.line_end(start);
        let newline = src.find('\n').unwrap_or_else(|| panic!("newline missing"));
        assert_eq!(end, newline + 1);
    }

    /// Tests that `skip_ws_inline` correctly skips inline whitespace tokens in the token stream.
    ///
    /// # Examples
    ///
    /// ```no_run
    /// let src = "extern    type Foo";
    /// let tokens = tokenize(src);
    /// let mut stream = TokenStream::new(&tokens, src);
    /// stream.advance();
    /// stream.skip_ws_inline();
    /// assert!(matches!(
    ///     stream.peek().map(|t| t.0),
    ///     Some(SyntaxKind::K_TYPE)
    /// ));
    /// ```
    #[rstest]
    fn skip_ws_inline_skips_spaces() {
        let src = "extern    type Foo";
        let tokens = tokenize(src);
        let mut stream = TokenStream::new(&tokens, src);
        stream.advance();
        stream.skip_ws_inline();
        assert!(matches!(
            stream.peek().map(|t| t.0),
            Some(SyntaxKind::K_TYPE)
        ));
    }

    /// Tests that `line_end` returns the length of the source string when called with an out-of-bounds index.
    ///
    /// # Examples
    ///
    /// ```no_run
    /// let src = "typedef A = string\n";
    /// let tokens = tokenize(src);
    /// let stream = TokenStream::new(&tokens, src);
    /// let start = tokens.len();
    /// assert_eq!(stream.line_end(start), src.len());
    /// ```
    #[rstest]
    fn line_end_out_of_bounds_returns_len() {
        let src = "typedef A = string\n";
        let tokens = tokenize(src);
        let stream = TokenStream::new(&tokens, src);
        let start = tokens.len();
        assert_eq!(stream.line_end(start), src.len());
    }
}
