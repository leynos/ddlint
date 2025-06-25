//! Integration tests for the parser skeleton.
//!
//! These tests focus on verifying the CST construction and that the round-trip
//! property holds for simple inputs. Grammar-specific assertions will be added
//! once the parser rules are implemented.

use ddlint::{SyntaxKind, parse};
use rstest::{fixture, rstest};

fn pretty_print(node: &rowan::SyntaxNode<ddlint::DdlogLanguage>) -> String {
    let mut out = String::new();
    for element in node.children_with_tokens() {
        match element {
            rowan::NodeOrToken::Node(n) => out.push_str(&pretty_print(&n)),
            rowan::NodeOrToken::Token(t) => out.push_str(t.text()),
        }
    }
    out
}

#[fixture]
fn simple_prog() -> &'static str {
    "input relation R(x: u32);"
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

#[rstest]
fn error_token_produces_error_node() {
    let source = "?";
    let parsed = parse(source);
    let root = parsed.root().syntax();
    let mut found_error = false;
    for child in root.children_with_tokens() {
        if let rowan::NodeOrToken::Node(node) = child
            && node.kind() == SyntaxKind::N_ERROR
        {
            found_error = true;
        }
    }
    assert!(found_error);
}
