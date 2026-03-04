//! Behavioural tests for top-level `for` desugaring (conformance register item 8).
//!
//! Verifies end-to-end that top-level `for` statements emit semantic rules
//! without polluting CST rule spans, while explicit rule-body `for` remains
//! unchanged.

use ddlint::parse;
use ddlint::test_util::{ErrorPattern, find_matching_error};

/// Diagnostic substring shared across tests to avoid duplication.
const UNSUPPORTED_FOR_BODY_PATTERN: &str =
    "top-level `for` body must end in an atom-like expression";

#[test]
fn top_level_for_desugars_without_cst_rule_nodes() {
    let src = concat!(
        "input relation Items(x: u32)\n",
        "for (x in Items(x)) Process(x).\n",
    );
    let parsed = parse(src);
    assert!(
        parsed.errors().is_empty(),
        "unexpected parse errors for top-level `for`: {:?}",
        parsed.errors()
    );

    assert!(
        parsed.root().rules().is_empty(),
        "top-level `for` should not create CST `N_RULE` nodes"
    );
    assert_eq!(
        parsed.semantic_rules().len(),
        1,
        "expected exactly one semantic rule from top-level `for`"
    );
    #[expect(clippy::expect_used, reason = "test expects one semantic rule")]
    let rule = parsed
        .semantic_rules()
        .first()
        .expect("missing semantic rule");
    #[expect(clippy::expect_used, reason = "test expects one body term")]
    let first_body = rule.body().first().expect("missing body term");
    assert_eq!(rule.head().to_sexpr(), "(call Process x)");
    assert_eq!(first_body.to_sexpr(), "(call Items x)");

    assert_eq!(
        parsed.root().relations().len(),
        1,
        "relation declaration must still parse successfully"
    );
}

#[test]
fn top_level_for_multiline_desugars_without_leaking_body_rule() {
    let src = concat!(
        "input relation Items(x: u32)\n",
        "for (x in Items(x))\n",
        "Process(x).\n",
    );
    let parsed = parse(src);
    assert!(
        parsed.errors().is_empty(),
        "unexpected parse errors for multiline top-level `for`: {:?}",
        parsed.errors()
    );

    assert!(
        parsed.root().rules().is_empty(),
        "multiline top-level `for` body must not produce a CST rule"
    );
    assert_eq!(
        parsed.semantic_rules().len(),
        1,
        "multiline top-level `for` should still desugar once"
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
    assert!(
        parsed.semantic_rules().is_empty(),
        "rule-body `for` must not generate top-level semantic rules"
    );
}

#[test]
fn unsupported_top_level_for_body_emits_targeted_diagnostic() {
    let src = "for (x in Items(x)) if (ready(x)) Process(x).";
    let parsed = parse(src);
    let pattern = ErrorPattern::from(UNSUPPORTED_FOR_BODY_PATTERN);
    assert!(
        find_matching_error(parsed.errors(), &pattern).is_some(),
        "expected unsupported top-level body diagnostic, got: {:?}",
        parsed.errors()
    );
    assert!(parsed.semantic_rules().is_empty());
}
