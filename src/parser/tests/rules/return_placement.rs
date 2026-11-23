//! Tests for `return` placement inside rule bodies.
//!
//! A rule body has no return value, so the parser rejects `return` at its
//! keyword span unless a function or closure body encloses it. These tests
//! cover the rejection, both permitted positions, and recovery after a
//! misplaced keyword.

use rstest::rstest;

use super::super::helpers::{parse_err, parse_ok, parse_single_rule};
use crate::{
    parser::ast::{Expr, RuleBodyTerm},
    test_util::{RETURN_IN_RULE_BODY_ERROR, assert_parse_error, find_matching_error},
};

/// Parse `src` and require exactly one diagnostic at the `return` keyword.
///
/// Deriving the span from `find` keeps the expectation tied to the byte
/// offset rather than the character count, so multibyte prefixes are covered.
#[track_caller]
fn parse_rejected_at_return(src: &str) -> crate::Parsed {
    let parsed = parse_err(src);
    let start = src
        .find("return")
        .expect("every rejection case must contain `return`");
    assert_parse_error(
        parsed.errors(),
        RETURN_IN_RULE_BODY_ERROR,
        start,
        start + "return".len(),
    );
    parsed
}

/// `return` anywhere in a rule-body literal is rejected at the keyword.
#[rstest]
#[case("Output(head_x) :- Source(return head_x).")]
#[case("R(x) :- return x.")]
#[case("R(x) :- Source(return).")]
#[case("R(x) :- A(x), y = return x.")]
#[case("R(x) :- Source((return x)).")]
#[case("R(x) :- Source(if cond { return x } else { x }).")]
#[case("R(x) :- for (item in Items(x)) Use(return item).")]
#[case("R(x) :- Source(|ignored| ignored, return x).")]
#[case("R(x) :- Source(\"é\"), return x.")]
fn rejects_return_without_a_function_or_closure_body(#[case] src: &str) {
    parse_rejected_at_return(src);
}

/// A function body and a closure body both keep `return` legal.
///
/// The literal splitter does not track pipes, so closures here take exactly
/// one parameter and contain no top-level comma.
#[rstest]
#[case("function f(): u32 { return 1; }\nOutput(head_x) :- Source(head_x).")]
#[case("Output(head_x) :- Source(|ignored| return head_x).")]
#[case("Output(head_x) :- Source(|ignored| Seed(|other| return head_x)).")]
fn permits_return_in_a_function_or_closure_body(#[case] src: &str) { parse_ok(src); }

/// One misplaced `return` must not discard the following rule.
#[test]
fn recovers_the_rule_after_a_misplaced_return() {
    let src = "Bad(x) :- Source(return x).\nGood(x) :- Source(x).";
    let parsed = parse_rejected_at_return(src);
    let rule_count = parsed.root().rules().len();
    assert_eq!(
        rule_count, 2,
        "recovery must keep both rules in the CST, got {rule_count}"
    );
}

/// Term classification applies the same rule-body restriction as the scanner.
#[test]
fn body_terms_reject_a_bare_return() {
    let src = "R(x) :- Source(return x).";
    let parsed = parse_err(src);
    let Some(rule) = parsed.root().rules().first().cloned() else {
        panic!("recovery must keep the rule in the CST");
    };

    let errors = match rule.body_terms() {
        Ok(terms) => panic!("a bare return must not classify, got {terms:?}"),
        Err(errors) => errors,
    };
    let expected = RETURN_IN_RULE_BODY_ERROR.into();
    assert!(
        find_matching_error(&errors, &expected).is_some(),
        "classification must report the rule-body diagnostic, got {errors:?}"
    );
}

/// A closure body stays legal for term classification too.
#[test]
fn body_terms_accept_a_return_in_a_closure_body() {
    let Some(rule) = parse_single_rule("R(x) :- Source(|ignored| return x).") else {
        panic!("the rule should parse");
    };
    let terms = match rule.body_terms() {
        Ok(terms) => terms,
        Err(errors) => panic!("a closure body must classify, got {errors:?}"),
    };
    assert!(
        matches!(
            terms.first(),
            Some(RuleBodyTerm::Expression(Expr::Apply { .. }))
        ),
        "the literal should stay an application, got {terms:?}"
    );
}
