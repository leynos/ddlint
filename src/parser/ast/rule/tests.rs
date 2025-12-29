//! Tests for the rule AST wrapper.

use super::{Expr, RuleBodyTerm, RuleForLoop};
use crate::parse;

mod helpers {
    use super::*;

    /// Parse source, extract the first rule's flattened body terms, and assert
    /// that all terms are Expression variants with the expected count.
    #[expect(clippy::expect_used, reason = "test helper for clearer failures")]
    pub(super) fn assert_flattened_terms(src: &str, expected_len: usize) -> Vec<RuleBodyTerm> {
        let parsed = parse(src);
        crate::test_util::assert_no_parse_errors(parsed.errors());
        let rule = parsed
            .root()
            .rules()
            .first()
            .cloned()
            .expect("rule missing");
        let terms = rule.flattened_body_terms().expect("should parse");
        assert_eq!(terms.len(), expected_len);
        for (i, term) in terms.iter().enumerate() {
            assert!(
                matches!(term, RuleBodyTerm::Expression(_)),
                "expected Expression at index {i}, got {term:?}"
            );
        }
        terms
    }
}

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
    // Note: For-loops are now classified as RuleBodyTerm::ForLoop, not Expression,
    // so body_expressions() returns only the if-else expression.
    let src = "R(x) :- for (item in Items(item)) Process(item), if (ready(x)) { Accept(x) } else { Skip() }.";
    let parsed = parse(src);
    crate::test_util::assert_no_parse_errors(parsed.errors());
    let rule = parsed
        .root()
        .rules()
        .first()
        .cloned()
        .expect("rule missing");

    // body_expressions only returns Expression terms (not ForLoop terms)
    let exprs = rule.body_expressions().expect("expressions should parse");
    assert_eq!(exprs.len(), 1);
    assert!(matches!(exprs.first(), Some(Expr::IfElse { .. })));

    // body_terms returns all terms including ForLoop
    let terms = rule.body_terms().expect("terms should parse");
    assert_eq!(terms.len(), 2);
    assert!(matches!(terms.first(), Some(RuleBodyTerm::ForLoop(_))));
    assert!(matches!(terms.get(1), Some(RuleBodyTerm::Expression(_))));
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

#[expect(clippy::expect_used, reason = "test requires parsed terms")]
#[test]
fn classifies_top_level_for_loop_as_for_term() {
    let src = "R(x) :- for (item in Items(item)) Process(item).";
    let parsed = parse(src);
    crate::test_util::assert_no_parse_errors(parsed.errors());
    let rule = parsed
        .root()
        .rules()
        .first()
        .cloned()
        .expect("rule missing");
    let terms = rule.body_terms().expect("should parse");
    assert_eq!(terms.len(), 1);
    assert!(matches!(terms.first(), Some(RuleBodyTerm::ForLoop(_))));
}

#[expect(clippy::expect_used, reason = "test requires parsed terms")]
#[test]
fn for_loop_with_guard_extracts_guard() {
    let src = "R(x) :- for (item in Items(item) if ready(item)) Process(item).";
    let parsed = parse(src);
    crate::test_util::assert_no_parse_errors(parsed.errors());
    let rule = parsed
        .root()
        .rules()
        .first()
        .cloned()
        .expect("rule missing");
    let terms = rule.body_terms().expect("should parse");
    let Some(RuleBodyTerm::ForLoop(for_loop)) = terms.first() else {
        panic!("expected ForLoop term");
    };
    assert!(for_loop.guard.is_some());
}

#[expect(clippy::expect_used, reason = "test requires parsed terms")]
#[test]
fn nested_for_loops_produce_nested_structure() {
    let src = "R(a, b) :- for (a in A(a)) for (b in B(b)) Pair(a, b).";
    let parsed = parse(src);
    crate::test_util::assert_no_parse_errors(parsed.errors());
    let rule = parsed
        .root()
        .rules()
        .first()
        .cloned()
        .expect("rule missing");
    let terms = rule.body_terms().expect("should parse");
    assert_eq!(terms.len(), 1);
    let Some(RuleBodyTerm::ForLoop(outer)) = terms.first() else {
        panic!("expected outer ForLoop term");
    };
    assert_eq!(outer.body_terms.len(), 1);
    assert!(matches!(
        outer.body_terms.first(),
        Some(RuleBodyTerm::ForLoop(_))
    ));
}

#[test]
fn flattened_body_terms_expands_for_loops() {
    // Should have: Items(item), ready(item), Process(item)
    let src = "R(x) :- for (item in Items(item) if ready(item)) Process(item).";
    let _terms = helpers::assert_flattened_terms(src, 3);
}

#[test]
fn flattened_nested_for_loops_produce_flat_sequence() {
    // Should have: A(a), B(b), Pair(a, b)
    let src = "R(a, b) :- for (a in A(a)) for (b in B(b)) Pair(a, b).";
    let _terms = helpers::assert_flattened_terms(src, 3);
}

#[test]
fn rule_for_loop_flatten_produces_correct_sequence() {
    // Unit test for RuleForLoop::flatten() without parsing
    use crate::parser::ast::Pattern;

    let for_loop = RuleForLoop {
        pattern: Pattern::Var {
            declared: false,
            name: "item".to_string(),
        },
        iterable: Expr::Variable("Source".to_string()),
        guard: Some(Expr::Variable("guard".to_string())),
        body_terms: vec![RuleBodyTerm::Expression(Expr::Variable("Body".to_string()))],
    };

    let flattened = for_loop.flatten();
    assert_eq!(flattened.len(), 3);

    // First: iterable
    assert!(matches!(
        flattened.first(),
        Some(RuleBodyTerm::Expression(Expr::Variable(name))) if name == "Source"
    ));
    // Second: guard
    assert!(matches!(
        flattened.get(1),
        Some(RuleBodyTerm::Expression(Expr::Variable(name))) if name == "guard"
    ));
    // Third: body
    assert!(matches!(
        flattened.get(2),
        Some(RuleBodyTerm::Expression(Expr::Variable(name))) if name == "Body"
    ));
}
