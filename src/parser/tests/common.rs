//! Shared helpers for parser tests.
//!
//! Utilities here support concise feature-focused parser tests.

use crate::{
    parse,
    parser::ast::{Function, Index, Relation, Transformer},
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

/// Parse a program containing a single relation and return it.
#[expect(clippy::expect_used, reason = "helpers used only in tests")]
pub(super) fn parse_relation(src: &str) -> Relation {
    let parsed = parse(src);
    assert!(parsed.errors().is_empty());
    let relations = parsed.root().relations();
    relations.first().cloned().expect("relation missing")
}

/// Parse a program containing a single index and return it.
#[expect(clippy::expect_used, reason = "helpers used only in tests")]
pub(super) fn parse_index(src: &str) -> Index {
    let parsed = parse(src);
    assert!(parsed.errors().is_empty());
    let indexes = parsed.root().indexes();
    indexes.first().cloned().expect("index missing")
}

/// Parse a program containing a single function and return it.
#[expect(clippy::expect_used, reason = "helpers used only in tests")]
pub(super) fn parse_function(src: &str) -> Function {
    let parsed = parse(src);
    assert!(parsed.errors().is_empty());
    let funcs = parsed.root().functions();
    funcs.first().cloned().expect("function missing")
}

/// Parse a program containing a single transformer and return it.
#[expect(clippy::expect_used, reason = "helpers used only in tests")]
pub(super) fn parse_transformer(src: &str) -> Transformer {
    let parsed = parse(src);
    assert!(parsed.errors().is_empty());
    let transformers = parsed.root().transformers();
    transformers.first().cloned().expect("transformer missing")
}
