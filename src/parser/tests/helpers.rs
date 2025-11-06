//! Shared helpers for parser tests.
//!
//! Utilities here support concise feature-focused parser tests.

use crate::{
    SyntaxKind, parse,
    parser::ast::{Function, Import, Index, Relation, Transformer},
};

type SyntaxNode = rowan::SyntaxNode<crate::DdlogLanguage>;
type SyntaxElement = rowan::SyntaxElement<crate::DdlogLanguage>;

/// Collect the text of a syntax subtree.
///
/// This helper iteratively traverses the tree using an explicit stack,
/// ensuring deeply nested inputs do not risk recursion overflow. It enables
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
/// assert_eq!(normalize_whitespace("a  b\n c"), "a b c");
/// ```
pub(super) fn normalize_whitespace(text: &str) -> String {
    text.split_whitespace().collect::<Vec<_>>().join(" ")
}

/// Parse `src`, and assert that the program is well formed.
#[track_caller]
pub(super) fn parse_ok(src: &str) -> crate::Parsed {
    let parsed = parse(src);
    crate::test_util::assert_no_parse_errors(parsed.errors());
    assert_eq!(parsed.root().kind(), SyntaxKind::N_DATALOG_PROGRAM);
    parsed
}

/// Parse `src` expecting at least one error.
#[track_caller]
pub(super) fn parse_err(src: &str) -> crate::Parsed {
    let parsed = parse(src);
    assert!(
        !parsed.errors().is_empty(),
        "expected parse to fail but it succeeded"
    );
    // Keep CST shape invariant even on error paths.
    assert_eq!(parsed.root().kind(), SyntaxKind::N_DATALOG_PROGRAM);
    parsed
}

/// Parse and ensure the source round-trips via `pretty_print`.
#[track_caller]
pub(super) fn round_trip(src: &str) {
    let parsed = parse_ok(src);
    assert_eq!(pretty_print(parsed.root().syntax()), src);
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
    crate::test_util::assert_no_parse_errors(parsed.errors());
    assert_eq!(parsed.root().kind(), SyntaxKind::N_DATALOG_PROGRAM);
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
