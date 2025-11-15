use crate::{SyntaxKind, parse};

#[expect(clippy::expect_used, reason = "Using expect for clearer test failures")]
#[test]
fn rule_body_expression_creates_node() {
    let src = "R(x) :- 1 + 2 * 3.";
    let parsed = parse(src);
    assert!(parsed.errors().is_empty(), "unexpected parse errors" );
    let root = parsed.root().syntax();
    let mut expr_nodes = root
        .descendants()
        .filter(|n| n.kind() == SyntaxKind::N_EXPR_NODE);
    let node = expr_nodes.next().expect("expr node missing");
    assert!(expr_nodes.next().is_none());
    let text = node.text().to_string();
    assert_eq!(text.trim(), "1 + 2 * 3");
}

#[expect(clippy::expect_used, reason = "Using expect for clearer test failures")]
#[test]
fn multiple_literals_create_multiple_nodes() {
    let src = "R(x) :- Foo(x), if (cond) Bar() else Baz().";
    let parsed = parse(src);
    assert!(parsed.errors().is_empty(), "unexpected parse errors");
    let root = parsed.root().syntax();
    let mut expr_nodes = root
        .descendants()
        .filter(|n| n.kind() == SyntaxKind::N_EXPR_NODE);
    let first = expr_nodes.next().expect("first expr missing");
    let second = expr_nodes.next().expect("second expr missing");
    assert!(expr_nodes.next().is_none());
    assert_eq!(first.text().trim(), "Foo(x)");
    assert_eq!(second.text().trim(), "if (cond) Bar() else Baz()");
}
