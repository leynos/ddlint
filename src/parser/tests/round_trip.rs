//! Round-trip parser tests.
//!
//! These tests ensure parsing followed by pretty-printing reproduces the
//! original source and that basic program structure is detected correctly.

use super::helpers::{parse_err, parse_ok, pretty_print, round_trip};
use crate::SyntaxKind;
use rstest::{fixture, rstest};

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

#[rstest]
fn parse_round_trip(simple_prog: &str) {
    round_trip(simple_prog);
}

#[rstest]
fn complex_program_round_trip(complex_prog: &str) {
    let parsed = parse_ok(complex_prog);
    let text = pretty_print(parsed.root().syntax());
    assert_eq!(text, complex_prog);
    let relations = parsed.root().relations();
    assert_eq!(relations.len(), 2);
    let [first, second] = relations.as_slice() else {
        panic!("expected two relations");
    };
    assert!(first.is_input());
    assert!(second.is_output());
    assert_eq!(first.name().as_deref(), Some("R"));
    assert_eq!(second.name().as_deref(), Some("S"));
}

#[rstest]
fn empty_program_has_no_items(empty_prog: &str) {
    let parsed = parse_ok(empty_prog);
    assert!(parsed.root().imports().is_empty());
    assert!(parsed.root().type_defs().is_empty());
    assert!(parsed.root().relations().is_empty());
    assert!(parsed.root().functions().is_empty());
    assert!(parsed.root().indexes().is_empty());
    assert!(parsed.root().rules().is_empty());
    assert_eq!(pretty_print(parsed.root().syntax()), "");
    assert_eq!(parsed.root().syntax().children().count(), 0);
}

#[rstest]
fn error_token_produces_error_node() {
    let source = "$( relation Foo(";
    let parsed = parse_err(source);
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
