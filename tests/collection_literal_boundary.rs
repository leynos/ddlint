//! Behavioural tests for the collection literal lowering boundary.
//!
//! These tests verify that `parse()` preserves vector and map literals as raw
//! `Expr::VecLit` and `Expr::MapLit` nodes rather than lowering them into
//! builder-call sequences, and that `Parsed::semantic_rules()` does not
//! synthesize collection-specific semantic rules.

#![allow(clippy::expect_used, reason = "test assertions use expect for clarity")]

use ddlint::parse;
use ddlint::parser::ast::Expr;
use ddlint::test_util::assert_no_parse_errors;
use rstest::rstest;

/// Verify that a rule body containing a collection literal parses without errors,
/// yields exactly one rule, and the second body expression is the expected variant.
#[rstest]
#[case("Out(x) :- In(x), [1, 2].", "VecLit", 2)]
#[case("Out(x) :- In(x), {a: 1}.", "MapLit", 1)]
fn parse_preserves_collection_literal_in_rule_body(
    #[case] input: &str,
    #[case] expected_variant: &str,
    #[case] expected_count: usize,
) {
    let parsed = parse(input);

    // Parse should succeed with no errors.
    assert_no_parse_errors(parsed.errors());

    // Should yield exactly one CST rule.
    let rules = parsed.root().rules();
    assert_eq!(rules.len(), 1, "expected exactly one rule");

    let rule = rules.first().expect("expected at least one rule");

    // The rule should parse its body expressions successfully.
    let body_exprs = rule
        .body_expressions()
        .unwrap_or_else(|errs| panic!("expected body_expressions to succeed, got {errs:?}"));

    // The first body expression is the atom `In(x)`, the second is the collection literal.
    assert_eq!(body_exprs.len(), 2, "expected two body expressions");

    // The second body expression should be the expected collection literal variant.
    let collection_expr = body_exprs.get(1).expect("expected second body expression");

    match expected_variant {
        "VecLit" => {
            assert!(
                matches!(collection_expr, Expr::VecLit(_)),
                "expected Expr::VecLit for rule body vector literal, got {collection_expr:?}"
            );
            if let Expr::VecLit(elements) = collection_expr {
                assert_eq!(
                    elements.len(),
                    expected_count,
                    "expected {expected_count} elements in vector literal"
                );
            }
        }
        "MapLit" => {
            assert!(
                matches!(collection_expr, Expr::MapLit(_)),
                "expected Expr::MapLit for rule body map literal, got {collection_expr:?}"
            );
            if let Expr::MapLit(entries) = collection_expr {
                assert_eq!(
                    entries.len(),
                    expected_count,
                    "expected {expected_count} entries in map literal"
                );
            }
        }
        _ => panic!("unexpected variant: {expected_variant}"),
    }
}

/// Verify that `Parsed::semantic_rules()` remains empty when collection
/// literals appear in rule bodies, proving that collection literals do not
/// participate in parse-time semantic lowering.
#[test]
fn parse_does_not_synthesize_semantic_rules_for_collection_literals() {
    let parsed = parse(
        r"
        Out1(v) :- In(x), v = [1, 2, 3].
        Out2(m) :- In(x), m = {a: 1, b: 2}.
        ",
    );

    // Parse should succeed with no errors.
    assert_no_parse_errors(parsed.errors());

    // `Parsed::semantic_rules()` should remain empty because collection
    // literals do not trigger parse-time semantic lowering like top-level
    // `for` statements do.
    assert!(
        parsed.semantic_rules().is_empty(),
        "collection literals must not produce parse-time semantic rules, got {:?}",
        parsed.semantic_rules()
    );

    // The CST should contain exactly two explicit rules.
    let rules = parsed.root().rules();
    assert_eq!(rules.len(), 2, "expected two explicit rules in CST");
}

/// Verify that nested collection literals in rule bodies are preserved as
/// nested `VecLit` and `MapLit` nodes rather than being lowered.
#[test]
fn parse_preserves_nested_collection_literals_in_rule_body() {
    let parsed = parse("Out(x) :- In(x), [[1], {a: {b: 2}}].");

    // Parse should succeed with no errors.
    assert_no_parse_errors(parsed.errors());

    // `Parsed::semantic_rules()` should remain empty because collection
    // literals in rule bodies must not trigger parse-time semantic lowering.
    assert!(
        parsed.semantic_rules().is_empty(),
        "collection literals must not produce parse-time semantic rules, got {:?}",
        parsed.semantic_rules()
    );

    let rules = parsed.root().rules();
    assert_eq!(rules.len(), 1, "expected exactly one rule");

    let rule = rules.first().expect("expected at least one rule");
    let body_exprs = rule
        .body_expressions()
        .unwrap_or_else(|errs| panic!("expected body_expressions to succeed, got {errs:?}"));

    // Body: In(x), [[1], {a: {b: 2}}]
    // The second body expression is the nested vector literal.
    assert_eq!(
        body_exprs.len(),
        2,
        "expected exactly two body terms, got {}",
        body_exprs.len()
    );

    // The second body term should be a VecLit containing nested structures.
    let nested_vec = body_exprs.get(1).expect("expected second body expression");
    assert!(
        matches!(nested_vec, Expr::VecLit(_)),
        "expected outer Expr::VecLit, got {nested_vec:?}"
    );

    if let Expr::VecLit(outer_elements) = nested_vec {
        assert_eq!(
            outer_elements.len(),
            2,
            "expected two elements in outer vector"
        );

        // First element: [1] should be VecLit.
        let first_elem = outer_elements
            .first()
            .expect("expected first element in vector");
        assert!(
            matches!(first_elem, Expr::VecLit(_)),
            "expected nested Expr::VecLit, got {first_elem:?}"
        );

        // Second element: {a: {b: 2}} should be MapLit.
        let second_elem = outer_elements
            .get(1)
            .expect("expected second element in vector");
        assert!(
            matches!(second_elem, Expr::MapLit(_)),
            "expected nested Expr::MapLit, got {second_elem:?}"
        );

        // Verify inner map nesting: {a: {b: 2}}.
        if let Expr::MapLit(entries) = second_elem {
            assert_eq!(entries.len(), 1, "expected one entry in outer map");
            let (_key, value) = entries.first().expect("expected first map entry");
            assert!(
                matches!(value, Expr::MapLit(_)),
                "expected inner Expr::MapLit, got {value:?}"
            );
        }
    }
}
