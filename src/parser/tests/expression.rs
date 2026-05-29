//! Tests for the Pratt expression parser.

mod fixtures;

use fixtures::{
    error_cases, expression_cases, invalid_for_loop_sources, literal_cases,
    match_pattern_error_cases, postfix_error_cases,
};

use crate::parser::expression::parse_expression;
use crate::test_util::{assert_parse_error, assert_unclosed_delimiter_error};

#[test]
fn parses_expressions() {
    for case in expression_cases() {
        let expr = parse_expression(case.src)
            .unwrap_or_else(|errs| panic!("source {:?} errors: {errs:?}", case.src));
        assert_eq!(expr, case.expected, "source {:?}", case.src);
    }
}

#[test]
fn rejects_invalid_for_loop_syntax() {
    for src in invalid_for_loop_sources() {
        let Err(errors) = parse_expression(src) else {
            panic!("expected parse failure for {src}");
        };
        assert!(
            !errors.is_empty(),
            "parser returned Err but without diagnostics for {src}"
        );
    }
}

#[test]
fn parses_literals() {
    for case in literal_cases() {
        let expr = parse_expression(case.src)
            .unwrap_or_else(|errs| panic!("source {:?} errors: {errs:?}", case.src));
        assert_eq!(expr, case.expected, "source {:?}", case.src);
    }
}

#[test]
fn rejects_expression_exceeding_max_depth() {
    let depth = 257;
    let source = format!("{}0{}", "(".repeat(depth), ")".repeat(depth));
    let Err(errors) = parse_expression(&source) else {
        panic!("expected depth error");
    };
    assert_parse_error(&errors, "expression nesting too deep", depth - 1, depth);
}

#[test]
fn reports_struct_literal_disallowed_in_if_condition() {
    let Err(errors) = parse_expression("if Point { x: 1 } else { y }") else {
        panic!("expected parse error");
    };
    assert_parse_error(
        &errors,
        "struct literal syntax is not allowed in this context",
        3,
        8,
    );
}

#[test]
fn reports_errors() {
    for case in error_cases() {
        match parse_expression(case.src) {
            Ok(_) => panic!("expected parse error"),
            Err(errs) => assert!(
                errs.len() >= case.min_errs,
                "source {:?} produced too few errors: {errs:?}",
                case.src
            ),
        }
    }
}

#[test]
fn match_pattern_delimiter_errors() {
    for case in match_pattern_error_cases() {
        let Err(errors) = parse_expression(case.src) else {
            panic!("expected error");
        };
        let Some(first) = errors.first() else {
            panic!("error missing");
        };
        let rendered = format!("{first:?}");
        assert!(
            rendered.contains(case.msg),
            "expected error to contain pattern '{}', got '{}'",
            case.msg,
            rendered,
        );
        assert_eq!(first.span(), case.start..case.end);
    }
}

fn reports_exactly_one_error(src: &str) {
    match parse_expression(src) {
        Ok(_) => panic!("expected parse error"),
        Err(errs) => assert_eq!(errs.len(), 1, "expected exactly one error, got {errs:?}"),
    }
}

#[test]
fn rejects_chained_type_ops_with_single_diag() {
    for src in ["x: T: U", "x as T as U", "x: T as U", "x as T: U"] {
        reports_exactly_one_error(src);
    }
}

#[test]
fn postfix_expression_errors() {
    for case in postfix_error_cases() {
        let Err(errors) = parse_expression(case.src) else {
            panic!("expected error");
        };
        if case.unclosed {
            assert_unclosed_delimiter_error(&errors, case.msg, case.start, case.end);
        } else {
            assert_parse_error(&errors, case.msg, case.start, case.end);
        }
    }
}
