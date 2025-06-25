//! Integration tests for the parser skeleton.
//!
//! These tests focus on verifying the CST construction and that the round-trip
//! property holds for simple inputs. Grammar-specific assertions will be added
//! once the parser rules are implemented.

use ddlint::{SyntaxKind, parse};
use rstest::{fixture, rstest};

/// Collect the text of a syntax subtree.
///
/// This helper iteratively traverses the tree using an explicit stack so
/// deeply nested inputs do not risk recursion overflow. It enables
/// round-trip tests that assert the printed output matches the original
/// source.
fn pretty_print(node: &rowan::SyntaxNode<ddlint::DdlogLanguage>) -> String {
    let mut out = String::new();
    let mut stack = vec![rowan::SyntaxElement::Node(node.clone())];

    while let Some(item) = stack.pop() {
        match item {
            rowan::SyntaxElement::Token(t) => out.push_str(t.text()),
            rowan::SyntaxElement::Node(n) => {
                let children: Vec<rowan::SyntaxElement<ddlint::DdlogLanguage>> =
                    n.children_with_tokens().collect();
                for child in children.into_iter().rev() {
                    stack.push(child);
                }
            }
        }
    }

    out
}

#[fixture]
fn simple_prog() -> &'static str {
    "input relation R(x: u32);"
}

#[fixture]
fn complex_prog() -> &'static str {
    "input relation R(x: u32);\noutput relation S(y: string);"
}

#[fixture]
fn empty_prog() -> &'static str {
    ""
}

/// Verifies that parsing and pretty-printing preserves the original input text
/// and produces the expected root node kind.
#[rstest]
fn parse_round_trip(simple_prog: &str) {
    let parsed = parse(simple_prog);
    let text = pretty_print(parsed.root().syntax());
    assert_eq!(text, simple_prog);
    assert_eq!(parsed.root().kind(), SyntaxKind::N_DATALOG_PROGRAM);
}

/// Ensures that invalid tokens are represented by `N_ERROR` nodes in the CST.
#[rstest]
fn error_token_produces_error_node() {
    let source = "?";
    let parsed = parse(source);
    let root = parsed.root().syntax();
    let has_error = root
        .children_with_tokens()
        .filter_map(|child| match child {
            rowan::NodeOrToken::Node(node) => Some(node),
            rowan::NodeOrToken::Token(_) => None,
        })
        .any(|node| node.kind() == SyntaxKind::N_ERROR);
    assert!(has_error);
}

#[rstest]
fn import_statement_standard_case() {
    let src = "import standard_library";
    let parsed = parse(src);
    assert!(parsed.errors().is_empty());
    let imports = parsed.root().imports();
    assert_eq!(imports.len(), 1);
    let Some(imp) = imports.first() else {
        panic!("expected import");
    };
    assert_eq!(imp.path(), "standard_library");
    assert!(imp.alias().is_none());
}

#[rstest]
fn import_statement_with_alias() {
    let src = "import collections::vector as vec";
    let parsed = parse(src);
    assert!(parsed.errors().is_empty());
    let imports = parsed.root().imports();
    assert_eq!(imports.len(), 1);
    let Some(imp) = imports.first() else {
        panic!("expected import");
    };
    assert_eq!(imp.path(), "collections::vector");
    assert_eq!(imp.alias(), Some("vec".to_string()));
}

#[rstest]
fn import_statement_invalid_missing_path() {
    let src = "import as missing_path";
    let parsed = parse(src);
    assert!(!parsed.errors().is_empty());
    let imports = parsed.root().imports();
    assert!(imports.is_empty());
}
