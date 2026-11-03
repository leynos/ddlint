//! Behavioural tests for top-level `for` desugaring (conformance register item 8).
//!
//! Verifies end-to-end that top-level `for` statements emit semantic rules
//! without polluting CST rule spans, while explicit rule-body `for` remains
//! unchanged.

use ddlint::{
    parse,
    test_util::{ErrorPattern, find_matching_error},
};
use fixture_lints::allow_fixture_expansion_lints;
use rstest::{fixture, rstest};

/// Diagnostic substring shared across tests to avoid duplication.
const UNSUPPORTED_FOR_HEAD_PATTERN: &str =
    "top-level `for` body must end in an atom-like expression";
const UNSUPPORTED_FOR_BODY_PATTERN: &str =
    "top-level `for` body contains unsupported control flow before the head";
const UNTERMINATED_FOR_PATTERN: &str =
    "unterminated top-level `for` statement; expected trailing `.`";

#[allow_fixture_expansion_lints]
#[fixture]
fn parse_case(#[default("")] src: &str) -> ddlint::Parsed { parse(src) }

/// One top-level `for` desugaring case.
///
/// Cases that only assert rule counts start from [`TopLevelForCase::counts`]
/// and override the optional sexpr expectations they care about.
struct TopLevelForCase {
    src: &'static str,
    semantic_rules: usize,
    cst_rules: usize,
    relations: usize,
    error_pattern: Option<&'static str>,
    head: Option<&'static str>,
    first_body: Option<&'static str>,
}

impl TopLevelForCase {
    /// Builds a case that asserts only the rule counts.
    const fn counts(
        src: &'static str,
        semantic_rules: usize,
        cst_rules: usize,
        relations: usize,
    ) -> Self {
        Self {
            src,
            semantic_rules,
            cst_rules,
            relations,
            error_pattern: None,
            head: None,
            first_body: None,
        }
    }
}

#[rstest]
#[case::single_line_desugar(TopLevelForCase {
    head: Some("(call Process x)"),
    first_body: Some("(call Items x)"),
    ..TopLevelForCase::counts(
        concat!(
            "input relation Items(x: u32)\n",
            "for (x in Items(x)) Process(x).\n",
        ),
        1,
        0,
        1,
    )
})]
#[case::multiline_desugar(TopLevelForCase::counts(
    concat!(
        "input relation Items(x: u32)\n",
        "for (x in Items(x))\n",
        "Process(x).\n",
    ),
    1,
    0,
    1,
))]
#[case::dot_separated_after_rule(TopLevelForCase::counts(
    "A(x) :- B(x). for (y in C(y)) D(y).",
    1,
    1,
    0,
))]
#[case::tuple_index_with_spaced_dot(TopLevelForCase::counts(
    "for (x in Items(x)) pair. 0(x).",
    1,
    0,
    0,
))]
#[case::unterminated_statement(TopLevelForCase {
    error_pattern: Some(UNTERMINATED_FOR_PATTERN),
    ..TopLevelForCase::counts("for (x in Items(x)) Process(x)", 0, 0, 0)
})]
#[case::unsupported_non_atom_head(TopLevelForCase {
    error_pattern: Some(UNSUPPORTED_FOR_HEAD_PATTERN),
    ..TopLevelForCase::counts("for (x in Items(x)) x + 1.", 0, 0, 0)
})]
#[case::unsupported_if_body(TopLevelForCase {
    error_pattern: Some(UNSUPPORTED_FOR_BODY_PATTERN),
    ..TopLevelForCase::counts("for (x in Items(x)) if (ready(x)) Process(x).", 0, 0, 0)
})]
fn top_level_for_behaviour(#[case] case: TopLevelForCase) {
    let TopLevelForCase {
        src,
        semantic_rules,
        cst_rules,
        relations,
        error_pattern,
        head,
        first_body,
    } = case;
    let parsed = parse(src);

    if let Some(pattern_source) = error_pattern {
        let expected_error = ErrorPattern::from(pattern_source);
        assert!(
            find_matching_error(parsed.errors(), &expected_error).is_some(),
            "expected diagnostic `{expected_error:?}`, got: {:?}",
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
        semantic_rules,
        "unexpected semantic rule count"
    );
    assert_eq!(
        parsed.root().rules().len(),
        cst_rules,
        "unexpected CST rule count"
    );
    assert_eq!(
        parsed.root().relations().len(),
        relations,
        "unexpected relation declaration count"
    );

    if let Some(head_sexpr) = head {
        let rule = parsed
            .semantic_rules()
            .first()
            .expect("missing semantic rule");
        assert_eq!(rule.head().to_sexpr(), head_sexpr);
    }

    if let Some(body_sexpr) = first_body {
        let rule = parsed
            .semantic_rules()
            .first()
            .expect("missing semantic rule");
        let body_term = rule.body().first().expect("missing body term");
        assert_eq!(body_term.to_sexpr(), body_sexpr);
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
        UNSUPPORTED_FOR_HEAD_PATTERN,
        "unsupported top-level `for` head",
    );
}
