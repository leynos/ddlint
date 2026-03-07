//! Tests for top-level `for` desugaring into semantic rules.

use super::super::helpers::{parse_err, parse_ok};
use crate::parser::ast::SemanticRuleOrigin;
use crate::parser::top_level_for::UNSUPPORTED_TOP_LEVEL_FOR_STATEMENT;
use crate::test_util::{ErrorPattern, find_matching_error};
use rstest::rstest;

#[rstest]
#[case(
    "for (x in Items(x)) Process(x).",
    1,
    0,
    &["(call Process x)"],
    &["(call Items x)"],
)]
#[case(
    "A(x) :- B(x). for (y in C(y)) D(y).",
    1,
    1,
    &["(call D y)"],
    &[],
)]
#[case(
    "for (x in Items(x)) Process(x).\nfor (y in More(y)) Process2(y).",
    2,
    0,
    &["(call Process x)", "(call Process2 y)"],
    &[],
)]
#[case("for (x in Items(x)) pair.0(x).", 1, 0, &[], &[])]
#[case("for (x in Items(x)) pair. 0(x).", 1, 0, &[], &[])]
#[case("    for (x in Items(x)) Process(x).", 1, 0, &[], &[])]
#[case("R(x) :- for (item in Items(item)) Process(item).", 0, 1, &[], &[])]
fn top_level_for_desugaring_cases(
    #[case] src: &str,
    #[case] expected_semantic_rules: usize,
    #[case] expected_cst_rules: usize,
    #[case] expected_heads: &[&str],
    #[case] expected_body: &[&str],
) {
    let parsed = parse_ok(src);
    assert_eq!(parsed.root().rules().len(), expected_cst_rules);
    assert_eq!(parsed.semantic_rules().len(), expected_semantic_rules);

    if expected_semantic_rules > 0 {
        assert!(
            parsed
                .semantic_rules()
                .iter()
                .all(|rule| rule.origin() == SemanticRuleOrigin::TopLevelFor),
            "all semantic rules should originate from top-level `for`"
        );
    }

    if !expected_heads.is_empty() {
        let heads = parsed
            .semantic_rules()
            .iter()
            .map(|rule| rule.head().to_sexpr())
            .collect::<Vec<_>>();
        let expected = expected_heads
            .iter()
            .map(|head| (*head).to_string())
            .collect::<Vec<_>>();
        assert_eq!(heads, expected);
    }

    if !expected_body.is_empty() {
        let Some(rule) = parsed.semantic_rules().first() else {
            panic!("missing semantic rule for body assertion");
        };
        let body = rule
            .body()
            .iter()
            .map(crate::parser::ast::Expr::to_sexpr)
            .collect::<Vec<_>>();
        let expected = expected_body
            .iter()
            .map(|item| (*item).to_string())
            .collect::<Vec<_>>();
        assert_eq!(body, expected);
    }
}

#[test]
fn top_level_for_with_guard_and_nested_loop_desugars_in_order() {
    let src = "for (a in A(a) if ready(a)) for (b in B(b)) Pair(a, b).";
    let parsed = parse_ok(src);
    assert_eq!(parsed.semantic_rules().len(), 1);

    let Some(rule) = parsed.semantic_rules().first() else {
        panic!("missing semantic rule");
    };
    let body = rule
        .body()
        .iter()
        .map(crate::parser::ast::Expr::to_sexpr)
        .collect::<Vec<_>>();

    assert_eq!(rule.head().to_sexpr(), "(call Pair a b)");
    assert_eq!(
        body,
        vec![
            "(call A a)".to_string(),
            "(call ready a)".to_string(),
            "(call B b)".to_string(),
        ]
    );
}

#[test]
fn unsupported_top_level_for_body_reports_diagnostic() {
    let src = "for (x in Items(x)) if (ready(x)) Process(x).";
    let parsed = parse_err(src);
    let errors = parsed.errors();
    assert_eq!(
        errors.len(),
        1,
        "expected exactly one diagnostic for unsupported top-level `for`, got: {errors:?}"
    );
    let pattern = ErrorPattern::from(UNSUPPORTED_TOP_LEVEL_FOR_STATEMENT);
    let Some(index) = find_matching_error(errors, &pattern) else {
        panic!("expected top-level for lowering diagnostic, got: {errors:?}");
    };
    let Some(error) = errors.get(index) else {
        panic!("diagnostic index out of range");
    };
    let Some(for_offset) = src.find("for") else {
        panic!("`for` keyword missing in test source");
    };
    assert!(
        error.span().start <= for_offset && for_offset < error.span().end,
        "expected diagnostic span {:?} to include `for` at {for_offset}",
        error.span()
    );
    assert!(
        index == 0,
        "expected unsupported-body diagnostic to be first, got index {index}"
    );
    assert!(parsed.semantic_rules().is_empty());
    assert!(parsed.root().rules().is_empty());
}
