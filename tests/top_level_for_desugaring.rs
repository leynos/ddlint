//! Behavioural tests for top-level `for` desugaring (conformance register item 8).
//!
//! Verifies end-to-end that top-level `for` statements emit semantic rules
//! without polluting CST rule spans, while explicit rule-body `for` remains
//! unchanged.

use ddlint::parse;
use ddlint::test_util::{ErrorPattern, find_matching_error};
use rstest::{fixture, rstest};

/// Diagnostic substring shared across tests to avoid duplication.
const UNSUPPORTED_FOR_BODY_PATTERN: &str =
    "top-level `for` body must end in an atom-like expression";
const UNTERMINATED_FOR_PATTERN: &str =
    "unterminated top-level `for` statement; expected trailing `.`";

#[fixture]
fn parse_case(#[default("")] src: &str) -> ddlint::Parsed {
    parse(src)
}

#[rstest]
#[case::single_line_desugar(
    concat!(
        "input relation Items(x: u32)\n",
        "for (x in Items(x)) Process(x).\n",
    ),
    1,
    0,
    1,
    None,
    Some("(call Process x)"),
    Some("(call Items x)")
)]
#[case::multiline_desugar(
    concat!(
        "input relation Items(x: u32)\n",
        "for (x in Items(x))\n",
        "Process(x).\n",
    ),
    1,
    0,
    1,
    None,
    None,
    None
)]
#[case::dot_separated_after_rule("A(x) :- B(x). for (y in C(y)) D(y).", 1, 1, 0, None, None, None)]
#[case::tuple_index_with_spaced_dot("for (x in Items(x)) pair. 0(x).", 1, 0, 0, None, None, None)]
#[case::unterminated_statement(
    "for (x in Items(x)) Process(x)",
    0,
    0,
    0,
    Some(UNTERMINATED_FOR_PATTERN),
    None,
    None
)]
#[case::unsupported_non_atom_head(
    "for (x in Items(x)) x + 1.",
    0,
    0,
    0,
    Some(UNSUPPORTED_FOR_BODY_PATTERN),
    None,
    None
)]
#[case::unsupported_if_body(
    "for (x in Items(x)) if (ready(x)) Process(x).",
    0,
    0,
    0,
    Some(UNSUPPORTED_FOR_BODY_PATTERN),
    None,
    None
)]
fn top_level_for_behaviour(
    #[case] src: &str,
    #[case] expected_semantic_rules: usize,
    #[case] expected_cst_rules: usize,
    #[case] expected_relations: usize,
    #[case] expected_error_pattern: Option<&str>,
    #[case] expected_head: Option<&str>,
    #[case] expected_first_body: Option<&str>,
) {
    let parsed = parse(src);

    if let Some(pattern) = expected_error_pattern {
        let pattern = ErrorPattern::from(pattern);
        assert!(
            find_matching_error(parsed.errors(), &pattern).is_some(),
            "expected diagnostic `{pattern:?}`, got: {:?}",
            parsed.errors()
        );
    } else {
        assert!(
            parsed.errors().is_empty(),
            "unexpected parse errors for top-level `for`: {:?}",
            parsed.errors()
        );
    }

    assert_eq!(
        parsed.semantic_rules().len(),
        expected_semantic_rules,
        "unexpected semantic rule count"
    );
    assert_eq!(
        parsed.root().rules().len(),
        expected_cst_rules,
        "unexpected CST rule count"
    );
    assert_eq!(
        parsed.root().relations().len(),
        expected_relations,
        "unexpected relation declaration count"
    );

    if let Some(expected_head) = expected_head {
        #[expect(clippy::expect_used, reason = "test expects one semantic rule")]
        let rule = parsed
            .semantic_rules()
            .first()
            .expect("missing semantic rule");
        assert_eq!(rule.head().to_sexpr(), expected_head);
    }

    if let Some(expected_first_body) = expected_first_body {
        #[expect(clippy::expect_used, reason = "test expects one semantic rule")]
        let rule = parsed
            .semantic_rules()
            .first()
            .expect("missing semantic rule");
        #[expect(clippy::expect_used, reason = "test expects one body term")]
        let first_body = rule.body().first().expect("missing body term");
        assert_eq!(first_body.to_sexpr(), expected_first_body);
    }
}

#[rstest]
fn rule_body_for_unaffected(
    #[with("R(x) :- for (item in Items(item)) Process(item).")] parse_case: ddlint::Parsed,
) {
    assert!(
        parse_case.errors().is_empty(),
        "rule-body `for` must not produce errors: {:?}",
        parse_case.errors()
    );
    assert_eq!(
        parse_case.root().rules().len(),
        1,
        "expected one rule with body `for`"
    );
    assert!(
        parse_case.semantic_rules().is_empty(),
        "rule-body `for` must not generate top-level semantic rules"
    );
}

fn assert_invalid_top_level_for(src: &str, pattern: &str, label: &str) {
    let parsed = parse(src);
    let error_pattern = ErrorPattern::from(pattern);
    assert!(
        find_matching_error(parsed.errors(), &error_pattern).is_some(),
        "expected {label} diagnostic, got: {:?}",
        parsed.errors()
    );
    assert!(
        parsed.semantic_rules().is_empty(),
        "{label} must not emit semantic rules"
    );
    assert!(
        parsed.root().rules().is_empty(),
        "{label} must not produce CST rules"
    );
}

#[test]
fn unterminated_top_level_for_reports_error() {
    assert_invalid_top_level_for(
        "for (x in Items(x)) Process(x)",
        UNTERMINATED_FOR_PATTERN,
        "unterminated top-level `for`",
    );
}

#[test]
fn unsupported_top_level_for_non_atom_head_reports_diagnostic() {
    assert_invalid_top_level_for(
        "for (x in Items(x)) x + 1.",
        UNSUPPORTED_FOR_BODY_PATTERN,
        "unsupported top-level `for` head",
    );
}
