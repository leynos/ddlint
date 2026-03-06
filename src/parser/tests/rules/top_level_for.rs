//! Tests for top-level `for` desugaring into semantic rules.

use super::super::helpers::{parse_err, parse_ok};
use crate::parser::ast::SemanticRuleOrigin;
use crate::parser::top_level_for::UNSUPPORTED_TOP_LEVEL_FOR_STATEMENT;
use crate::test_util::{ErrorPattern, find_matching_error};

#[test]
fn top_level_for_desugars_into_semantic_rule() {
    let src = "for (x in Items(x)) Process(x).";
    let parsed = parse_ok(src);
    assert!(parsed.root().rules().is_empty());
    assert_eq!(parsed.semantic_rules().len(), 1);

    #[expect(clippy::expect_used, reason = "test expects one semantic rule")]
    let rule = parsed
        .semantic_rules()
        .first()
        .expect("missing semantic rule");
    assert_eq!(rule.origin(), SemanticRuleOrigin::TopLevelFor);
    assert_eq!(rule.head().to_sexpr(), "(call Process x)");
    assert_eq!(rule.body().len(), 1);
    #[expect(clippy::expect_used, reason = "test expects one body term")]
    let first_body = rule.body().first().expect("missing body term");
    assert_eq!(first_body.to_sexpr(), "(call Items x)");
}

#[test]
fn top_level_for_with_guard_and_nested_loop_desugars_in_order() {
    let src = "for (a in A(a) if ready(a)) for (b in B(b)) Pair(a, b).";
    let parsed = parse_ok(src);
    assert_eq!(parsed.semantic_rules().len(), 1);

    #[expect(clippy::expect_used, reason = "test expects one semantic rule")]
    let rule = parsed
        .semantic_rules()
        .first()
        .expect("missing semantic rule");
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
fn top_level_for_after_dot_separator_desugars() {
    let src = "A(x) :- B(x). for (y in C(y)) D(y).";
    let parsed = parse_ok(src);
    assert_eq!(parsed.root().rules().len(), 1);
    assert_eq!(parsed.semantic_rules().len(), 1);
    #[expect(clippy::expect_used, reason = "test expects one semantic rule")]
    let rule = parsed
        .semantic_rules()
        .first()
        .expect("missing semantic rule");
    assert_eq!(rule.head().to_sexpr(), "(call D y)");
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
    #[expect(
        clippy::expect_used,
        reason = "index from find_matching_error is valid"
    )]
    let error = errors.get(index).expect("diagnostic index out of range");
    #[expect(clippy::expect_used, reason = "test source includes leading `for`")]
    let for_offset = src
        .find("for")
        .expect("`for` keyword missing in test source");
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
}

#[test]
fn top_level_multiple_for_desugars_in_order() {
    let src = "for (x in Items(x)) Process(x).\nfor (y in More(y)) Process2(y).";
    let parsed = parse_ok(src);
    assert_eq!(parsed.semantic_rules().len(), 2);
    let heads = parsed
        .semantic_rules()
        .iter()
        .map(|rule| rule.head().to_sexpr())
        .collect::<Vec<_>>();
    assert_eq!(
        heads,
        vec![
            "(call Process x)".to_string(),
            "(call Process2 y)".to_string()
        ]
    );
}

#[test]
fn top_level_for_body_with_tuple_index_head_desugars() {
    let src = "for (x in Items(x)) pair.0(x).";
    let parsed = parse_ok(src);
    assert!(parsed.root().rules().is_empty());
    assert_eq!(parsed.semantic_rules().len(), 1);
}

#[test]
fn top_level_for_body_with_spaced_tuple_index_head_desugars() {
    let src = "for (x in Items(x)) pair. 0(x).";
    let parsed = parse_ok(src);
    assert!(parsed.root().rules().is_empty());
    assert_eq!(parsed.semantic_rules().len(), 1);
}

#[test]
fn top_level_for_with_leading_indentation_desugars() {
    let src = "    for (x in Items(x)) Process(x).";
    let parsed = parse_ok(src);
    assert!(parsed.root().rules().is_empty());
    assert_eq!(parsed.semantic_rules().len(), 1);
}

#[test]
fn rule_body_for_still_accepted() {
    let src = "R(x) :- for (item in Items(item)) Process(item).";
    let parsed = parse_ok(src);
    assert_eq!(parsed.root().rules().len(), 1);
    assert!(parsed.semantic_rules().is_empty());
}
