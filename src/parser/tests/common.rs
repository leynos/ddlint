//! Shared helpers for parser tests.
//!
//! Utilities here support concise feature-focused parser tests.

use crate::{
    parse,
    parser::ast::{Function, Import, Index, Relation, Transformer},
};

type SyntaxNode = rowan::SyntaxNode<crate::DdlogLanguage>;
type SyntaxElement = rowan::SyntaxElement<crate::DdlogLanguage>;

/// Collect the text of a syntax subtree.
///
/// This helper iteratively traverses the tree using an explicit stack so
/// deeply nested inputs do not risk recursion overflow. It enables
/// round-trip tests that assert the printed output matches the original
/// source.
pub(super) fn pretty_print(node: &SyntaxNode) -> String {
    let mut out = String::new();
    let mut stack = vec![SyntaxElement::Node(node.clone())];

    while let Some(item) = stack.pop() {
        match item {
            SyntaxElement::Token(t) => out.push_str(t.text()),
            SyntaxElement::Node(n) => {
                let children: Vec<SyntaxElement> = n.children_with_tokens().collect();
                for child in children.into_iter().rev() {
                    stack.push(child);
                }
            }
        }
    }

    out
}

/// Collapse runs of whitespace into single spaces.
///
/// ```
/// assert_eq!(normalise_whitespace("a  b\n c"), "a b c");
/// ```
pub(super) fn normalise_whitespace(text: &str) -> String {
    text.split_whitespace().collect::<Vec<_>>().join(" ")
}

/// Parse a program and extract the first item produced by `extractor`.
///
/// The helper asserts that parsing succeeds without errors and that the
/// extractor yields at least one item.
#[expect(clippy::expect_used, reason = "helpers used only in tests")]
fn parse_single_item<T: Clone, F: FnOnce(&crate::parser::ast::Root) -> Vec<T>>(
    src: &str,
    extractor: F,
) -> T {
    let parsed = parse(src);
    assert!(
        parsed.errors().is_empty(),
        "unexpected errors: {:?}",
        parsed.errors()
    );
    let items = extractor(parsed.root());
    items.first().cloned().expect("item missing")
}

/// Parse a program containing a single relation and return it.
pub(super) fn parse_relation(src: &str) -> Relation {
    parse_single_item(src, crate::parser::ast::Root::relations)
}

/// Parse a program containing a single index and return it.
pub(super) fn parse_index(src: &str) -> Index {
    parse_single_item(src, crate::parser::ast::Root::indexes)
}

/// Parse a program containing a single function and return it.
pub(super) fn parse_function(src: &str) -> Function {
    parse_single_item(src, crate::parser::ast::Root::functions)
}

/// Parse a program containing a single transformer and return it.
pub(super) fn parse_transformer(src: &str) -> Transformer {
    parse_single_item(src, crate::parser::ast::Root::transformers)
}

/// Parse a program containing a single import and return it.
pub(super) fn parse_import(src: &str) -> Import {
    parse_single_item(src, crate::parser::ast::Root::imports)
}

// Example helper for other AST nodes:
//
// pub(super) fn parse_rule(src: &str) -> Rule {
//     parse_single_item(src, |root| root.rules())
// }
//
// pub(super) fn parse_type_def(src: &str) -> TypeDef {
//     parse_single_item(src, |root| root.type_defs())
// }
//
// pub(super) fn parse_import(src: &str) -> Import {
//     parse_single_item(src, |root| root.imports())
// }
