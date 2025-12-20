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

#[expect(clippy::expect_used, reason = "test requires parsed expressions")]
#[test]
fn parses_multi_head_rules_with_location_delay_and_diff() {
    let src = "A(a)@site(x) -<10>, B'(b) :- Src(a, b).";
    let parsed = parse(src);
    crate::test_util::assert_no_parse_errors(parsed.errors());
    let rule = parsed
        .root()
        .rules()
        .first()
        .cloned()
        .expect("rule missing");
    let heads = rule.heads().expect("heads should parse");
    assert_eq!(heads.len(), 2);

    let first = heads.first().expect("first head missing");
    assert!(matches!(first.location, Some(Expr::Call { .. })));
    assert!(matches!(first.atom, Expr::AtomDelay { delay: 10, .. }));

    let second = heads.get(1).expect("second head missing");
    assert!(second.location.is_none());
    assert!(matches!(second.atom, Expr::AtomDiff { .. }));
}

#[expect(clippy::expect_used, reason = "test requires parsed expressions")]
#[test]
fn lowers_by_ref_heads_to_ref_new() {
    let src = "&RefOrder{ id: id, amt: amt } :- incoming_order(id, amt).";
    let parsed = parse(src);
    crate::test_util::assert_no_parse_errors(parsed.errors());
    let rule = parsed
        .root()
        .rules()
        .first()
        .cloned()
        .expect("rule missing");
    let heads = rule.heads().expect("heads should parse");
    let head = heads.first().expect("head missing");
    assert!(matches!(
        head.atom,
        Expr::Call { ref callee, .. } if matches!(&**callee, Expr::Variable(name) if name == "ref_new")
    ));
}
