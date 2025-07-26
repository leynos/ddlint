use crate::{SyntaxKind, parse};

#[expect(clippy::expect_used, reason = "Using expect for clearer test failures")]
#[test]
fn rule_body_expression_creates_node() {
    let src = "R(x) :- 1 + 2 * 3.";
    let parsed = parse(src);
    // The span scanner does not currently recover from invalid rule bodies, so
    // errors are expected. The expression parser should still process the body
    // tokens.
    assert!(!parsed.errors().is_empty());
    let root = parsed.root().syntax();
    let mut expr_nodes = root
        .descendants()
        .filter(|n| n.kind() == SyntaxKind::N_EXPR_NODE);
    let node = expr_nodes.next().expect("expr node missing");
    assert!(expr_nodes.next().is_none());
    let text = node.text().to_string();
    assert_eq!(text.trim(), "1 + 2 * 3");
}
