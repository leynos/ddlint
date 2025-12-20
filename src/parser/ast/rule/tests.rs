//! Tests for the rule AST wrapper.

use super::Expr;
use crate::parse;

#[expect(clippy::expect_used, reason = "Using expect for clearer test failures")]
#[test]
fn rule_head_and_body() {
    let parsed = parse("A(x) :- B(x).");
    let rule = parsed
        .root()
        .rules()
        .first()
        .cloned()
        .expect("rule missing");
    assert_eq!(rule.head().as_deref(), Some("A(x)"));
    assert_eq!(rule.body_literals(), vec!["B(x)".to_string()]);
}

#[expect(clippy::expect_used, reason = "test requires parsed expressions")]
#[test]
fn body_expressions_parse_control_flow() {
    let src = "R(x) :- for (item in Items(item)) Process(item), if (ready(x)) { Accept(x) } else { Skip() }.";
    let parsed = parse(src);
    crate::test_util::assert_no_parse_errors(parsed.errors());
    let rule = parsed
        .root()
        .rules()
        .first()
        .cloned()
        .expect("rule missing");
    let exprs = rule.body_expressions().expect("expressions should parse");
    assert_eq!(exprs.len(), 2);
    assert!(matches!(exprs.first(), Some(Expr::ForLoop { .. })));
    assert!(matches!(exprs.get(1), Some(Expr::IfElse { .. })));
}
