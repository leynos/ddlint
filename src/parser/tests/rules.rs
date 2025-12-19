//! Rule parsing tests.
//!
//! Ensures rule heads, bodies, and error cases are handled.

use super::helpers::{parse_err, parse_ok, pretty_print};
use crate::parser::ast::rule::split_assignment;
use crate::parser::ast::{AggregationSource, AstNode, Expr, Pattern, RuleBodyTerm};
use crate::test_util::{ErrorPattern, assert_parse_error, call, var};
use rstest::{fixture, rstest};

#[fixture]
fn simple_rule() -> &'static str {
    "ActiveUser(user_id) :- User(user_id, _, true)."
}

#[fixture]
fn multi_literal_rule() -> &'static str {
    "UserLogin(username, session_id) :- User(user_id, username, _), UserSession(user_id, session_id, _)."
}

#[fixture]
fn fact_rule() -> &'static str {
    "SystemAlert(\"System is now online.\")."
}

#[fixture]
fn for_loop_rule() -> &'static str {
    "ItemsProcessed(count) :- for (entry in Items(entry)) Process(entry)."
}

#[fixture]
fn for_loop_with_if_iterable() -> &'static str {
    "ItemsProcessed(count) :- for (entry in if cond(entry) { Items(entry) } else { Others(entry) }) Process(entry)."
}

#[fixture]
fn for_loop_with_guard_rule() -> &'static str {
    "FilteredItems(entry) :- for (entry in Items(entry) if true) Process(entry)."
}

#[fixture]
fn nested_for_loop_rule() -> &'static str {
    "PairProcessed(a, b) :- for (a in A(a)) for (b in B(b)) ProcessPair(a, b)."
}

/// Assert that `body_terms()` reports the expected arity error for an
/// aggregation literal found in `src`.
fn assert_aggregation_arity_error(src: &str, literal: &str, expected_error: &str) {
    let parsed = parse_ok(src);
    #[expect(clippy::expect_used, reason = "tests require a single rule")]
    let rule = parsed
        .root()
        .rules()
        .first()
        .cloned()
        .expect("rule missing");
    let errors = match rule.body_terms() {
        Ok(terms) => panic!("expected aggregation arity error, got {terms:?}"),
        Err(errs) => errs,
    };
    let Some(start) = src.find(literal) else {
        panic!("{literal} literal missing");
    };
    let end = start + literal.len();
    assert_parse_error(&errors, expected_error, start, end);
}

#[rstest]
#[case::simple_rule(simple_rule(), false)]
#[case::multi_literal_rule(multi_literal_rule(), false)]
#[case::fact_rule(fact_rule(), false)]
#[case::for_loop_rule(for_loop_rule(), false)]
#[case::for_loop_if_iterable(for_loop_with_if_iterable(), false)]
#[case::for_loop_with_guard(for_loop_with_guard_rule(), false)]
#[case::nested_for_loop(nested_for_loop_rule(), false)]
fn rule_parsing_tests(#[case] rule_input: &str, #[case] should_have_errors: bool) {
    let parsed = if should_have_errors {
        parse_err(rule_input)
    } else {
        parse_ok(rule_input)
    };
    let rules = parsed.root().rules();
    assert_eq!(rules.len(), 1);
    #[expect(clippy::expect_used, reason = "tests require a single rule")]
    let rule = rules.first().expect("rule missing");
    assert_eq!(
        pretty_print(rule.syntax()),
        rule_input,
        "round trip mismatch"
    );
}

#[expect(
    clippy::expect_used,
    reason = "tests assert split_assignment succeeds for representative inputs"
)]
#[test]
fn split_assignment_ignores_equals_in_parens() {
    let src = "foo = bar(baz = 1)";
    let parts =
        split_assignment(src).expect("expected split_assignment to find a top-level assignment");

    assert_eq!(parts.pattern, "foo");
    assert_eq!(parts.value, "bar(baz = 1)");
}

#[expect(
    clippy::expect_used,
    reason = "tests assert split_assignment succeeds for representative inputs"
)]
#[test]
fn split_assignment_ignores_equals_in_brackets_and_braces() {
    let src_brackets = r#"foo = map["a=b"]"#;
    let parts = split_assignment(src_brackets)
        .expect("expected split_assignment to split at the top-level =");
    assert_eq!(parts.pattern, "foo");
    assert_eq!(parts.value, r#"map["a=b"]"#);

    let src_braces = "foo = { key = value }";
    let parts = split_assignment(src_braces)
        .expect("expected split_assignment to split at the top-level =");
    assert_eq!(parts.pattern, "foo");
    assert_eq!(parts.value, "{ key = value }");
}

#[expect(
    clippy::expect_used,
    reason = "tests assert split_assignment succeeds for representative inputs"
)]
#[test]
fn split_assignment_trims_pattern_and_value_whitespace() {
    let src = "   foo   =   bar(baz)   ";
    let parts =
        split_assignment(src).expect("expected split_assignment to split at the top-level =");

    assert_eq!(parts.pattern, "foo");
    assert_eq!(parts.value, "bar(baz)");

    let src_mixed = "foo=   bar   ";
    let parts =
        split_assignment(src_mixed).expect("expected split_assignment to split at the top-level =");
    assert_eq!(parts.pattern, "foo");
    assert_eq!(parts.value, "bar");
}

#[expect(
    clippy::expect_used,
    reason = "tests require a single parsed rule for assignment assertions"
)]
fn assert_body_assignment(
    src: &str,
    expected_terms_count: usize,
    assignment_index: usize,
) -> (Pattern, Expr) {
    let parsed = parse_ok(src);
    let rule = parsed
        .root()
        .rules()
        .first()
        .cloned()
        .expect("rule missing");
    let terms = match rule.body_terms() {
        Ok(terms) => terms,
        Err(errs) => panic!("body terms should parse: {errs:?}"),
    };
    assert_eq!(terms.len(), expected_terms_count);
    let assignment = match terms.get(assignment_index) {
        Some(RuleBodyTerm::Assignment(assign)) => assign,
        other => panic!("expected assignment term, got {other:?}"),
    };
    (assignment.pattern.clone(), assignment.value.clone())
}

#[test]
fn body_terms_capture_flatmap_assignments() {
    let src = "Flat(ip) :- Source(addrs), var ip = FlatMap(extract_ips(addrs)).";
    let (pattern, value) = assert_body_assignment(src, 2, 1);
    assert_eq!(
        pattern,
        Pattern::Var {
            declared: true,
            name: "ip".to_string()
        }
    );
    assert_eq!(
        value,
        call("FlatMap", vec![call("extract_ips", vec![var("addrs")])])
    );
}

#[test]
fn body_terms_capture_tuple_bind_assignments() {
    let src = "Joined(key, value) :- Source(pairs), (key, value) = FlatMap(extract_pairs(pairs)).";
    let (pattern, _value) = assert_body_assignment(src, 2, 1);
    assert_eq!(
        pattern,
        Pattern::Tuple(vec![
            Pattern::Var {
                declared: false,
                name: "key".to_string(),
            },
            Pattern::Var {
                declared: false,
                name: "value".to_string(),
            },
        ])
    );
}

#[test]
fn body_terms_shift_assignment_pattern_error_spans() {
    let pattern_src = r#""${x}""#;
    let Err(pattern_errors) = crate::parser::pattern::parse_pattern(pattern_src) else {
        panic!("expected pattern parser to reject interpolated string patterns");
    };
    let base_span = pattern_errors
        .iter()
        .find(|err| format!("{err:?}").contains("interpolated strings are not allowed"))
        .unwrap_or_else(|| panic!("expected interpolation diagnostic, got {pattern_errors:?}"))
        .span();

    let src = format!("Bad() :- Source(), {pattern_src} = FlatMap(foo()).");
    let parsed = parse_err(&src);
    #[expect(clippy::expect_used, reason = "tests require a single rule")]
    let rule = parsed
        .root()
        .rules()
        .first()
        .cloned()
        .expect("rule missing");
    let errors = match rule.body_terms() {
        Ok(terms) => panic!("expected body terms error, got {terms:?}"),
        Err(errs) => errs,
    };

    let pattern_start = src
        .find(pattern_src)
        .unwrap_or_else(|| panic!("expected pattern source {pattern_src:?} in rule"));
    let expected_span = (pattern_start + base_span.start)..(pattern_start + base_span.end);
    let shifted = errors
        .iter()
        .find(|err| format!("{err:?}").contains("interpolated strings are not allowed"))
        .unwrap_or_else(|| panic!("expected interpolation diagnostic, got {errors:?}"));
    assert_eq!(shifted.span(), expected_span);
}

#[test]
fn body_terms_shift_assignment_value_error_spans() {
    let value_src = "1 2";
    let Err(value_errors) = crate::parser::expression::parse_expression(value_src) else {
        panic!("expected expression parser to reject invalid value source");
    };
    let base_span = value_errors
        .first()
        .unwrap_or_else(|| panic!("expected expression parse error, got {value_errors:?}"))
        .span();

    let src = format!("Bad() :- Source(), x = {value_src}.");
    let parsed = parse_err(&src);
    #[expect(clippy::expect_used, reason = "tests require a single rule")]
    let rule = parsed
        .root()
        .rules()
        .first()
        .cloned()
        .expect("rule missing");
    let errors = match rule.body_terms() {
        Ok(terms) => panic!("expected body terms error, got {terms:?}"),
        Err(errs) => errs,
    };

    let value_start = src
        .find(value_src)
        .unwrap_or_else(|| panic!("expected value source {value_src:?} in rule"));
    let expected_span = (value_start + base_span.start)..(value_start + base_span.end);
    assert!(
        errors.iter().any(|err| err.span() == expected_span),
        "expected shifted value error span {expected_span:?}, got {errors:?}",
    );
}

#[test]
fn body_terms_detect_group_by_aggregation() {
    let src =
        "Totals(user, total) :- Orders(user, amt), group_by(sum(amt), user), total = __group.";
    let parsed = parse_ok(src);
    #[expect(clippy::expect_used, reason = "tests require a single rule")]
    let rule = parsed
        .root()
        .rules()
        .first()
        .cloned()
        .expect("rule missing");
    let terms = match rule.body_terms() {
        Ok(terms) => terms,
        Err(errs) => panic!("body terms should parse: {errs:?}"),
    };
    assert_eq!(terms.len(), 3);
    let aggregation = match terms.get(1) {
        Some(RuleBodyTerm::Aggregation(agg)) => agg,
        other => panic!("expected aggregation term, got {other:?}"),
    };
    assert_eq!(aggregation.source, AggregationSource::GroupBy);
    assert_eq!(aggregation.project, call("sum", vec![var("amt")]));
    assert_eq!(aggregation.key, var("user"));
}

#[expect(
    clippy::expect_used,
    reason = "tests assert aggregation is present when parsing Aggregate literals"
)]
#[test]
fn body_terms_detect_legacy_aggregate_aggregation() {
    let src = "Totals(user, total) :- \
               Orders(user, amt), \
               Aggregate((user), sum(amt)), \
               total = __group.";
    let parsed = parse_ok(src);
    #[expect(clippy::expect_used, reason = "tests require a single rule")]
    let rule = parsed
        .root()
        .rules()
        .first()
        .cloned()
        .expect("rule missing");
    let terms = match rule.body_terms() {
        Ok(terms) => terms,
        Err(errs) => panic!("body terms should parse: {errs:?}"),
    };
    let aggregation = terms
        .iter()
        .find_map(|term| {
            if let RuleBodyTerm::Aggregation(agg) = term {
                Some(agg)
            } else {
                None
            }
        })
        .expect("expected a RuleBodyTerm::Aggregation from Aggregate(..)");

    assert_eq!(
        aggregation.source,
        AggregationSource::LegacyAggregate,
        "expected AggregationSource::LegacyAggregate for Aggregate"
    );
    assert_eq!(aggregation.project, call("sum", vec![var("amt")]));
    match &aggregation.key {
        Expr::Group(inner) => assert_eq!(**inner, var("user")),
        other => panic!("expected grouped key Expr, got {other:?}"),
    }
}

#[test]
fn body_terms_error_on_group_by_wrong_arity() {
    let src = "Totals(u, total) :- Orders(u, amt), group_by(sum(amt)).";
    assert_aggregation_arity_error(
        src,
        "group_by(sum(amt))",
        "group_by expects exactly two arguments",
    );
}

#[test]
fn body_terms_error_on_legacy_aggregate_wrong_arity() {
    let src = "Totals(user, total) :- \
               Orders(user, amt), \
               Aggregate((user), sum(amt), extra_arg), \
               total = __group.";
    assert_aggregation_arity_error(
        src,
        "Aggregate((user), sum(amt), extra_arg)",
        "Aggregate expects exactly two arguments",
    );
}

#[rstest]
#[case(
    ":- User(user_id, username, _).",
    ErrorPattern::from("Unexpected"),
    0,
    2
)]
#[case(
    "UserLogin(username, session_id) :- .",
    ErrorPattern::from("Unexpected"),
    35,
    36
)]
#[case(
    "UserLogin(username, session_id) User(user_id, username, _).",
    ErrorPattern::from("Unexpected"),
    32,
    36
)]
#[case(
    "UserLogin(username, session_id) :- User(user_id, username, _)",
    ErrorPattern::from("Unexpected"),
    0,
    61
)]
#[case("This is not a rule!", ErrorPattern::from("Unexpected"), 5, 7)]
fn invalid_rule_cases(
    #[case] input: &str,
    #[case] pattern: ErrorPattern,
    #[case] start: usize,
    #[case] end: usize,
) {
    let parsed = parse_err(input);
    let errors = parsed.errors();
    #[expect(clippy::expect_used, reason = "tests expect at least one parse error")]
    let first = errors.first().expect("expected parse error");
    assert_parse_error(std::slice::from_ref(first), pattern, start, end);
}
