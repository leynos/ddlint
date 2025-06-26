//! Integration tests for the parser skeleton.
//!
//! These tests focus on verifying the CST construction and that the round-trip
//! property holds for simple inputs. Grammar-specific assertions will be added
//! once the parser rules are implemented.

#![expect(clippy::expect_used, reason = "tests assert exact behaviour")]

use ddlint::{SyntaxKind, ast::Import, parse};
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
    use chumsky::error::SimpleReason;

    let src = "import as missing_path";
    let parsed = parse(src);
    let errors = parsed.errors();
    assert_eq!(errors.len(), 1);
    let Some(error) = errors.first() else {
        panic!("expected error");
    };
    assert!(matches!(error.reason(), SimpleReason::Unexpected));
    assert!(
        error
            .expected()
            .any(|e| e.as_ref().is_some_and(|k| *k == SyntaxKind::T_IDENT))
    );
    assert_eq!(error.found(), Some(&SyntaxKind::K_AS));
    let imports = parsed.root().imports();
    assert!(imports.is_empty());
}

#[rstest]
fn import_statement_multi_segment() {
    let src = "import foo::bar::baz";
    let parsed = parse(src);
    assert!(parsed.errors().is_empty());
    let paths: Vec<_> = parsed.root().imports().iter().map(Import::path).collect();
    assert_eq!(paths, ["foo::bar::baz"]);
}

#[rstest]
fn import_statement_whitespace_variations() {
    let src = "  import  foo  as  f  ";
    let parsed = parse(src);
    assert!(parsed.errors().is_empty());
    let imports = parsed.root().imports();
    let Some(imp) = imports.first() else {
        panic!("expected import");
    };
    assert_eq!(imp.path(), "foo");
    assert_eq!(imp.alias(), Some("f".into()));
}

#[rstest]
fn import_multiple_statements() {
    let src = "import a\nimport b as c";
    let parsed = parse(src);
    assert!(parsed.errors().is_empty());
    let imports = parsed.root().imports();
    let paths: Vec<_> = imports.iter().map(|i| (i.path(), i.alias())).collect();
    assert_eq!(paths, [("a".into(), None), ("b".into(), Some("c".into()))]);
}

#[rstest]
fn typedef_standard_case() {
    let src = "typedef Uuid = string";
    let parsed = parse(src);
    assert!(parsed.errors().is_empty());
    let defs = parsed.root().type_defs();
    let def = defs.first().expect("expected typedef");
    assert_eq!(def.name().as_deref(), Some("Uuid"));
    assert_eq!(def.definition(), Some("string".into()));
    assert!(!def.is_extern());
}

#[rstest]
fn typedef_complex_case() {
    let src = "typedef UserRecord = (name: string, age: u64, active: bool)";
    let parsed = parse(src);
    assert!(parsed.errors().is_empty());
    let defs = parsed.root().type_defs();
    let def = defs.first().expect("expected typedef");
    assert_eq!(def.name().as_deref(), Some("UserRecord"));
    assert_eq!(
        def.definition().as_deref(),
        Some("(name: string, age: u64, active: bool)")
    );
}

#[rstest]
fn extern_type_case() {
    let src = "extern type FfiHandle";
    let parsed = parse(src);
    assert!(parsed.errors().is_empty());
    let defs = parsed.root().type_defs();
    let def = defs.first().expect("expected typedef");
    assert_eq!(def.name().as_deref(), Some("FfiHandle"));
    assert!(def.definition().is_none());
    assert!(def.is_extern());
}
