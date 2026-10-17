//! Rule body loop structure, guards, and flattening.

use ddlint::parser::ast::{AggregationSource, Rule, RuleBodyTerm};

use super::{
    assert_body_terms_error_contains, assert_body_terms_ok, assert_expression_term_sexpr,
    mixed_rule, parse_and_extract_rule,
};

#[test]
fn for_loop_in_rule_body_is_classified_as_for_term() {
    let src = "ItemsProcessed(count) :- for (entry in Items(entry)) Process(entry).";
    let rule = parse_and_extract_rule(src).expect("source should parse and contain a rule");
    let terms = assert_body_terms_ok(&rule);
    assert_eq!(terms.len(), 1);
    assert!(matches!(terms.first(), Some(RuleBodyTerm::ForLoop(_))));
}

#[test]
fn for_loop_with_guard_has_guard_expression() {
    let src = "FilteredItems(x) :- for (x in Items(x) if valid(x)) Accept(x).";
    let rule = parse_and_extract_rule(src).expect("source should parse and contain a rule");
    let terms = assert_body_terms_ok(&rule);
    let Some(RuleBodyTerm::ForLoop(for_loop)) = terms.first() else {
        panic!("expected ForLoop term");
    };
    assert!(for_loop.guard.is_some());
    assert_eq!(
        for_loop
            .guard
            .as_ref()
            .map(ddlint::parser::ast::Expr::to_sexpr)
            .as_deref(),
        Some("(call valid x)")
    );
}

#[test]
fn nested_for_loops_preserve_structure() {
    let src = "Pairs(a, b) :- for (a in A(a)) for (b in B(b)) Match(a, b).";
    let rule = parse_and_extract_rule(src).expect("source should parse and contain a rule");
    let terms = assert_body_terms_ok(&rule);
    assert_eq!(terms.len(), 1);

    let Some(RuleBodyTerm::ForLoop(outer)) = terms.first() else {
        panic!("expected outer ForLoop term");
    };
    assert_eq!(outer.iterable.to_sexpr(), "(call A a)");
    assert_eq!(outer.body_terms.len(), 1);

    let Some(RuleBodyTerm::ForLoop(inner)) = outer.body_terms.first() else {
        panic!("expected inner ForLoop term");
    };
    assert_eq!(inner.iterable.to_sexpr(), "(call B b)");
}

#[rstest::rstest]
#[case::guarded_loop(
    "R(x) :- for (x in Source(x) if pred(x)) Target(x).",
    ["(call Source x)", "(call pred x)", "(call Target x)"]
)]
#[case::nested_loops(
    "R(a, b) :- for (a in A(a)) for (b in B(b)) Pair(a, b).",
    ["(call A a)", "(call B b)", "(call Pair a b)"]
)]
fn flattened_for_loops_expand_to_expected_components(
    #[case] src: &str,
    #[case] expected: [&str; 3],
) {
    let rule = parse_and_extract_rule(src).expect("source should parse and contain a rule");
    let flattened = match rule.flattened_body_terms() {
        Ok(terms) => terms,
        Err(errors) => panic!("should flatten successfully: {errors:?}"),
    };

    assert_eq!(flattened.len(), expected.len());
    for (index, expected_sexpr) in expected.iter().enumerate() {
        assert_expression_term_sexpr(&flattened, index, expected_sexpr);
    }
}

#[rstest::rstest]
fn mixed_for_loop_and_regular_terms(#[from(mixed_rule)] rule_res: Result<Rule, String>) {
    let rule = rule_res.expect("mixed rule fixture should parse and contain a rule");
    let terms = assert_body_terms_ok(&rule);
    assert_eq!(terms.len(), 3);

    // First term: Pre(x)
    assert!(matches!(terms.first(), Some(RuleBodyTerm::Expression(_))));

    // Second term: for loop
    assert!(matches!(terms.get(1), Some(RuleBodyTerm::ForLoop(_))));

    // Third term: Post(x, y)
    assert!(matches!(terms.get(2), Some(RuleBodyTerm::Expression(_))));
}

#[test]
fn for_loop_with_aggregation_in_body() {
    let src =
        "Totals(user, total) :- for (order in Orders(order)) group_by(sum(order.amt), order.user).";
    let rule = parse_and_extract_rule(src).expect("source should parse and contain a rule");
    let terms = assert_body_terms_ok(&rule);
    assert_eq!(terms.len(), 1);

    let Some(RuleBodyTerm::ForLoop(for_loop)) = terms.first() else {
        panic!("expected ForLoop term");
    };

    // The for-loop body should contain the aggregation
    assert_eq!(for_loop.body_terms.len(), 1);
    assert!(matches!(
        for_loop.body_terms.first(),
        Some(RuleBodyTerm::Aggregation(agg)) if agg.source == AggregationSource::GroupBy
    ));
}

#[test]
fn for_loop_pattern_is_preserved() {
    let src = "R(k, v) :- for ((k, v) in Pairs(k, v)) Accept(k, v).";
    let rule = parse_and_extract_rule(src).expect("source should parse and contain a rule");
    let terms = assert_body_terms_ok(&rule);

    let Some(RuleBodyTerm::ForLoop(for_loop)) = terms.first() else {
        panic!("expected ForLoop term");
    };

    // Pattern should be a tuple
    assert_eq!(for_loop.pattern.to_source(), "(k, v)");
}

#[test]
fn rejects_multiple_aggregations_inside_for_loop() {
    // Aggregation inside for-loop should count toward the one-aggregation limit.
    let src = "R(x) :- for (x in X(x)) group_by(sum(x), x), group_by(count(x), x).";
    let rule = parse_and_extract_rule(src).expect("source should parse and contain a rule");
    assert_body_terms_error_contains(
        &rule,
        "at most one aggregation (group_by or Aggregate) is permitted per rule body",
    );
}

#[test]
fn rejects_aggregation_inside_and_outside_for_loop() {
    // Aggregation inside for-loop plus aggregation outside should trigger error.
    let src = "R(x) :- for (x in X(x)) group_by(sum(x), x), group_by(count(x), k).";
    let rule = parse_and_extract_rule(src).expect("source should parse and contain a rule");
    assert_body_terms_error_contains(
        &rule,
        "at most one aggregation (group_by or Aggregate) is permitted per rule body",
    );
}

#[test]
fn for_loop_with_sequence_body_flattens_correctly() {
    // Test for-loop with semicolon-sequence body: (Pre(x); Inner(x))
    // Note: DDlog uses `;` for sequences, not `,` in braces
    let src = "R(x) :- for (x in Source(x)) (Pre(x); Inner(x)).";
    let rule = parse_and_extract_rule(src).expect("source should parse and contain a rule");
    let terms = assert_body_terms_ok(&rule);
    assert_eq!(terms.len(), 1);

    let Some(RuleBodyTerm::ForLoop(for_loop)) = terms.first() else {
        panic!("expected ForLoop term");
    };

    // Sequence body should produce multiple body_terms
    assert_eq!(
        for_loop.body_terms.len(),
        2,
        "sequence body should produce 2 body_terms"
    );

    // Flattening should produce: Source(x), Pre(x), Inner(x)
    let flattened = rule
        .flattened_body_terms()
        .expect("should flatten successfully");
    assert_eq!(flattened.len(), 3);
    assert_expression_term_sexpr(&flattened, 0, "(call Source x)");
    assert_expression_term_sexpr(&flattened, 1, "(call Pre x)");
    assert_expression_term_sexpr(&flattened, 2, "(call Inner x)");
}

#[test]
fn mixed_body_nested_for_loop_preserves_structure() {
    // Test mixed content inside a for-loop body: Pre; nested for (with body that chains to Post)
    // Using semicolon sequences as DDlog requires
    // Note: Due to right-associativity of `;`, the parse is:
    //   Pre(a) ; (for (b in B(b)) (Match(a, b) ; Post(a)))
    // But the for-loop body classifier unwraps sequences, so the outer for-loop body is:
    //   [Pre(a), for (b in B(b)) Match(a, b), Post(a)]
    let src = "Pairs(a, b) :- for (a in A(a)) (Pre(a); for (b in B(b)) Match(a, b); Post(a)).";
    let rule = parse_and_extract_rule(src).expect("source should parse and contain a rule");
    let terms = assert_body_terms_ok(&rule);

    // Still a single top-level for-loop term
    assert_eq!(terms.len(), 1);
    let Some(RuleBodyTerm::ForLoop(outer)) = terms.first() else {
        panic!("expected outer ForLoop term for mixed-body nested for-loop rule");
    };

    // The outer for-loop body has 2 elements due to how Seq right-associates:
    // Pre(a), then (for (b in B(b)) Match(a, b); Post(a))
    // But the nested for's body is just Match(a,b) and Post(a) is a sibling
    // Total after flattening the Seq: Pre(a), ForLoop, Post(a)
    // Actually due to right-associativity: Pre(a) ; rest where rest = for ... ; Post
    // The for-loop gets Match as body, then ; Post(a) is another Seq term
    assert!(
        outer.body_terms.len() >= 2,
        "expected at least 2 body_terms in outer for-loop, got {}",
        outer.body_terms.len()
    );

    // First: Pre(a) - an expression
    assert!(
        matches!(outer.body_terms.first(), Some(RuleBodyTerm::Expression(_))),
        "expected Pre(a) as expression term"
    );

    // Check that there's a nested for-loop somewhere in the body
    let has_nested_for = outer
        .body_terms
        .iter()
        .any(|t| matches!(t, RuleBodyTerm::ForLoop(_)));
    assert!(has_nested_for, "expected at least one nested ForLoop term");
}

#[rstest::rstest]
fn mixed_for_loop_and_regular_terms_flattened(#[from(mixed_rule)] rule_res: Result<Rule, String>) {
    // Verify flattened_body_terms preserves order for mixed for-loop and regular terms.
    let rule = rule_res.expect("mixed rule fixture should parse and contain a rule");
    let flattened = rule
        .flattened_body_terms()
        .expect("should flatten successfully");

    // Should be: Pre(x), Items(y), Process(y), Post(x, y)
    assert_eq!(flattened.len(), 4);
    assert_expression_term_sexpr(&flattened, 0, "(call Pre x)");
    assert_expression_term_sexpr(&flattened, 1, "(call Items y)");
    assert_expression_term_sexpr(&flattened, 2, "(call Process y)");
    assert_expression_term_sexpr(&flattened, 3, "(call Post x y)");
}
