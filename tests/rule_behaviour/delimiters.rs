//! Rule-body delimiter regressions for nested parentheses and field access.

use super::{RuleBodyTerm, assert_body_terms_ok, parse_and_extract_rule};

// -----------------------------------------------------------------------------
// Regression tests for nested delimiter handling in body bounds detection
// -----------------------------------------------------------------------------

#[test]
fn field_access_dot_inside_parentheses_not_treated_as_terminator() {
    // Regression test: T_DOT inside parentheses should not be treated as the
    // rule-terminating dot. This was the original bug that caused parse errors.
    let src =
        "Totals(user, total) :- for (order in Orders(order)) group_by(sum(order.amt), order.user).";
    let rule = parse_and_extract_rule(src);
    let terms = assert_body_terms_ok(&rule);
    assert_eq!(terms.len(), 1);

    let Some(RuleBodyTerm::ForLoop(for_loop)) = terms.first() else {
        panic!("expected ForLoop term");
    };
    // The aggregation with field access should be correctly parsed
    assert!(matches!(
        for_loop.body_terms.first(),
        Some(RuleBodyTerm::Aggregation(_))
    ));
}

#[test]
fn multiple_field_accesses_in_rule_body() {
    // More complex case: multiple field accesses in a rule body
    // Tests that dots in field access expressions don't terminate the rule body
    let src = "R(a, b) :- Source(x, y), filter(x.valid and y.active).";
    let rule = parse_and_extract_rule(src);
    let terms = assert_body_terms_ok(&rule);
    assert_eq!(terms.len(), 2);
}

#[test]
fn implies_inside_parentheses_not_treated_as_body_start() {
    // Test that `=>` (imply operator) inside parentheses doesn't cause issues.
    let src = "R(x) :- filter((a => b)), Source(x).";
    let rule = parse_and_extract_rule(src);
    let terms = assert_body_terms_ok(&rule);
    assert_eq!(terms.len(), 2);
}

#[test]
fn nested_parens_with_field_access_preserve_body_bounds() {
    // Test that dots inside nested parentheses are handled correctly
    let src = "R(x) :- Source(x), filter(x.field), Result(x).";
    let rule = parse_and_extract_rule(src);
    let terms = assert_body_terms_ok(&rule);
    assert_eq!(terms.len(), 3);
}
