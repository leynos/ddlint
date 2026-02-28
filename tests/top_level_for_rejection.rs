//! Behavioural tests for top-level `for` rejection (conformance register item 8).
//!
//! Verifies end-to-end that the parser emits a diagnostic for top-level `for`
//! while other declarations and rule-body `for` remain unaffected.

use ddlint::parse;

#[test]
fn top_level_for_produces_diagnostic_and_no_rule() {
    let src = concat!(
        "input relation Items(x: u32)\n",
        "for (x in Items(x)) Process(x).\n",
    );
    let parsed = parse(src);

    let errors = parsed.errors();
    let has_top_level_for_error = errors.iter().any(|e| {
        let rendered = format!("{e:?}");
        rendered.contains("top-level") && rendered.contains("for")
    });
    assert!(
        has_top_level_for_error,
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
