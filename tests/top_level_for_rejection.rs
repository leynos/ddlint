//! Behavioural tests for top-level `for` rejection (conformance register item 8).
//!
//! Verifies end-to-end that the parser emits a diagnostic for top-level `for`
//! while other declarations and rule-body `for` remain unaffected.

use ddlint::parse;
use ddlint::test_util::{ErrorPattern, find_matching_error};

/// Diagnostic substring shared across tests to avoid duplication.
const TOP_LEVEL_FOR_PATTERN: &str =
    "top-level `for` is not supported; use `for` inside rule bodies instead";

#[test]
fn top_level_for_produces_diagnostic_and_no_rule() {
    let src = concat!(
        "input relation Items(x: u32)\n",
        "for (x in Items(x)) Process(x).\n",
    );
    let parsed = parse(src);

    let errors = parsed.errors();
    let pattern = ErrorPattern::from(TOP_LEVEL_FOR_PATTERN);
    assert!(
        find_matching_error(errors, &pattern).is_some(),
        "expected top-level `for` diagnostic, got: {errors:?}"
    );

    assert!(
        parsed.root().rules().is_empty(),
        "top-level `for` must not produce a rule"
    );

    assert_eq!(
        parsed.root().relations().len(),
        1,
        "relation declaration must still parse successfully"
    );
}

#[test]
fn top_level_for_multiline_does_not_leak_body_as_rule() {
    let src = concat!(
        "input relation Items(x: u32)\n",
        "for (x in Items(x))\n",
        "Process(x).\n",
    );
    let parsed = parse(src);

    let errors = parsed.errors();
    let pattern = ErrorPattern::from(TOP_LEVEL_FOR_PATTERN);
    assert!(
        find_matching_error(errors, &pattern).is_some(),
        "expected top-level `for` diagnostic, got: {errors:?}"
    );

    assert!(
        parsed.root().rules().is_empty(),
        "multiline top-level `for` body must not produce a rule"
    );
}

#[test]
fn rule_body_for_unaffected() {
    let src = "R(x) :- for (item in Items(item)) Process(item).";
    let parsed = parse(src);

    assert!(
        parsed.errors().is_empty(),
        "rule-body `for` must not produce errors: {:?}",
        parsed.errors()
    );

    assert_eq!(
        parsed.root().rules().len(),
        1,
        "expected one rule with body `for`"
    );
}
