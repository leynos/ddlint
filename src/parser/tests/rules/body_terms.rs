//! Tests for rule body term classification.

use super::super::helpers::{parse_err, parse_ok};
use crate::parser::ast::{Expr, Pattern, RuleBodyTerm};
use crate::test_util::{call, var};

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
    let parsed = parse_err(src.as_str());
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
    let parsed = parse_err(src.as_str());
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
