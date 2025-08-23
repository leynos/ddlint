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

/// Parse a program and assert that no errors were produced.
///
/// The returned [`crate::Parsed`] can be further inspected by tests.
///
/// # Examples
///
/// ```ignore
/// use ddlint::parser::tests::common::parse_program;
///
/// let parsed = parse_program("input relation R(x: u32);");
/// assert!(parsed.errors().is_empty());
/// ```
pub(super) fn parse_program(src: &str) -> crate::Parsed {
    let parsed = parse(src);
    assert!(
        parsed.errors().is_empty(),
        "unexpected errors: {:?}",
        parsed.errors()
    );
    assert_eq!(parsed.root().kind(), SyntaxKind::N_DATALOG_PROGRAM);
    parsed
}

/// Parse a program and verify it round-trips via [`pretty_print`].
///
/// This asserts that the root node is a `N_DATALOG_PROGRAM`, no errors
/// occurred and the reconstructed source matches the original input.
///
/// # Examples
///
/// ```ignore
/// use ddlint::parser::tests::common::assert_program_round_trip;
///
/// assert_program_round_trip("input relation R(x: u32);");
/// ```
pub(super) fn assert_program_round_trip(src: &str) -> crate::Parsed {
    let parsed = parse_program(src);
    assert_eq!(pretty_print(parsed.root().syntax()), src);
    parsed
}

/// Assert that parsing produced at least one error.
///
/// # Examples
///
/// ```ignore
/// use ddlint::parser::tests::common::assert_parse_has_errors;
///
/// let parsed = ddlint::parse("?");
/// assert_parse_has_errors(&parsed);
/// ```
pub(super) fn assert_parse_has_errors(parsed: &crate::Parsed) {
    assert!(!parsed.errors().is_empty());
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
