//! Behavioural tests covering rule parsing with aggregations, `FlatMap`, and for-loop
//! constructs.

mod aggregations;
mod body_bounds;
mod for_loops;
mod heads;

use ddlint::parser::ast::{Rule, RuleBodyTerm};

#[path = "../support/rule_behaviour.rs"]
mod rule_setup;
use rule_setup::{mixed_rule, parse_and_extract_rule};

/// Call `body_terms()` expecting `Ok`, returning terms on success or panicking.
fn assert_body_terms_ok(rule: &Rule) -> Vec<RuleBodyTerm> {
    match rule.body_terms() {
        Ok(terms) => terms,
        Err(errs) => panic!("body terms should parse: {errs:?}"),
    }
}

/// Call `body_terms()` expecting `Err`, asserting the error list contains `expected_msg`.
#[track_caller]
fn assert_body_terms_error_contains(rule: &Rule, expected_msg: &str) {
    let Err(errors) = rule.body_terms() else {
        panic!("expected body_terms error");
    };
    let has_error = errors
        .iter()
        .any(|e| format!("{e:?}").contains(expected_msg));
    assert!(
        has_error,
        "expected error containing '{expected_msg}', got: {errors:?}"
    );
}

/// Assert that a flattened body term at a given index is an Expression
/// with the expected S-expression representation.
fn assert_expression_term_sexpr(terms: &[RuleBodyTerm], index: usize, expected_sexpr: &str) {
    let Some(RuleBodyTerm::Expression(expr)) = terms.get(index) else {
        panic!("expected expression term at index {index}");
    };
    assert_eq!(expr.to_sexpr(), expected_sexpr);
}
