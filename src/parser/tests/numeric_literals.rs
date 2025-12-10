//! Tests for width-qualified numeric literal parsing and validation.

use crate::parser::ast::{Expr, IntBase, Literal, NumberLiteral};
use crate::parser::expression::{parse_expression, parse_numeric_literal};
use crate::test_util::{assert_parse_error, lit_num};
use rstest::rstest;

fn unwrap_int(expr: Expr) -> (Option<u32>, bool, IntBase, String) {
    match expr {
        Expr::Literal(Literal::Number(NumberLiteral::Int(int))) => {
            (int.width, int.signed, int.base, int.value.to_string())
        }
        other => panic!("expected int literal, got {other:?}"),
    }
}

/// Expected properties of an integer literal for testing.
#[derive(Clone, Copy)]
struct ExpectedIntLiteral {
    width: Option<u32>,
    signed: bool,
    base: IntBase,
    value: &'static str,
}

/// Assert that parsing an input yields an integer literal with the expected properties.
fn assert_int_literal(input: &str, expected: ExpectedIntLiteral) {
    let expr = match parse_expression(input) {
        Ok(expr) => expr,
        Err(errors) => panic!("parse of {input:?} should succeed, got {errors:?}"),
    };
    let (width, signed, base, value) = unwrap_int(expr);
    assert_eq!(
        width, expected.width,
        "{input}: width mismatch: expected {:?}, got {width:?}",
        expected.width
    );
    assert_eq!(
        signed, expected.signed,
        "{input}: signedness mismatch: expected signed={}, got signed={signed}",
        expected.signed
    );
    assert_eq!(
        base, expected.base,
        "{input}: base mismatch: expected {:?}, got {base:?}",
        expected.base
    );
    assert_eq!(
        value, expected.value,
        "{input}: value mismatch: expected {}, got {value}",
        expected.value
    );
}

#[rstest]
#[case::width_qualified_hex("8'hFF", ExpectedIntLiteral { width: Some(8), signed: false, base: IntBase::Hex, value: "255" })]
#[case::signed_decimal_negative("16'sd-1", ExpectedIntLiteral { width: Some(16), signed: true, base: IntBase::Decimal, value: "-1" })]
#[case::unqualified_hex("0x1e", ExpectedIntLiteral { width: None, signed: false, base: IntBase::Hex, value: "30" })]
#[case::width_qualified_binary("4'b1010", ExpectedIntLiteral { width: Some(4), signed: false, base: IntBase::Binary, value: "10" })]
#[case::width_qualified_octal("8'o377", ExpectedIntLiteral { width: Some(8), signed: false, base: IntBase::Octal, value: "255" })]
#[case::explicit_plus_hex("8'h+FF", ExpectedIntLiteral { width: Some(8), signed: false, base: IntBase::Hex, value: "255" })]
#[case::signed_decimal_positive("16'sd+1", ExpectedIntLiteral { width: Some(16), signed: true, base: IntBase::Decimal, value: "1" })]
#[case::underscores_in_digits("16'hFF_FF", ExpectedIntLiteral { width: Some(16), signed: false, base: IntBase::Hex, value: "65535" })]
#[case::unqualified_binary("0b1010", ExpectedIntLiteral { width: None, signed: false, base: IntBase::Binary, value: "10" })]
#[case::unqualified_octal("0o17", ExpectedIntLiteral { width: None, signed: false, base: IntBase::Octal, value: "15" })]
#[case::signed_1bit_min("1'sd-1", ExpectedIntLiteral { width: Some(1), signed: true, base: IntBase::Decimal, value: "-1" })]
#[case::signed_1bit_zero("1'sd0", ExpectedIntLiteral { width: Some(1), signed: true, base: IntBase::Decimal, value: "0" })]
#[case::signed_2bit_min("2'sd-2", ExpectedIntLiteral { width: Some(2), signed: true, base: IntBase::Decimal, value: "-2" })]
#[case::signed_2bit_max("2'sd1", ExpectedIntLiteral { width: Some(2), signed: true, base: IntBase::Decimal, value: "1" })]
#[case::signed_3bit_min("3'sd-4", ExpectedIntLiteral { width: Some(3), signed: true, base: IntBase::Decimal, value: "-4" })]
#[case::signed_3bit_max("3'sd3", ExpectedIntLiteral { width: Some(3), signed: true, base: IntBase::Decimal, value: "3" })]
fn parses_integer_literals(#[case] input: &str, #[case] expected: ExpectedIntLiteral) {
    assert_int_literal(input, expected);
}

#[test]
fn rejects_out_of_range_unsigned_literal() {
    let errors = match parse_expression("1'b10") {
        Ok(expr) => panic!("expected failure, got {expr:?}"),
        Err(errors) => errors,
    };
    assert_parse_error(&errors, "does not fit", 0, 5);
}

#[test]
fn rejects_zero_width_literal() {
    let errors = match parse_expression("0'h0") {
        Ok(expr) => panic!("expected failure, got {expr:?}"),
        Err(errors) => errors,
    };
    assert_parse_error(&errors, "width must be non-zero", 0, 4);
}

#[test]
fn parses_width_qualified_float() {
    let expr = match parse_expression("3.14'f32") {
        Ok(expr) => expr,
        Err(errors) => panic!("parse should succeed, got {errors:?}"),
    };
    match expr {
        Expr::Literal(Literal::Number(NumberLiteral::Float(float))) => {
            assert_eq!(float.width, Some(32));
            assert_eq!(float.raw, "3.14'f32");
        }
        other => panic!("expected float literal, got {other:?}"),
    }
}

#[test]
fn rejects_unsupported_float_width() {
    let errors = match parse_expression("1.0'f16") {
        Ok(expr) => panic!("expected failure, got {expr:?}"),
        Err(errors) => errors,
    };
    assert_parse_error(&errors, "unsupported float width", 0, 7);
}

#[test]
fn lit_num_helper_uses_structured_literal() {
    let expr = lit_num("42");
    let (width, signed, base, value) = unwrap_int(expr);
    assert_eq!(width, None);
    assert!(!signed);
    assert_eq!(base, IntBase::Decimal);
    assert_eq!(value, "42");
}

fn assert_numeric_parse_error(input: &str, expected_substring: &str) {
    let err = match parse_numeric_literal(input) {
        Ok(lit) => panic!("expected failure for {input:?}, got {lit:?}"),
        Err(err) => err,
    };
    assert!(
        err.message().contains(expected_substring),
        "expected '{expected_substring}' error for {input:?}, got: {}",
        err.message()
    );
}

#[rstest]
#[case::missing_digits("8'h", "missing digits")]
#[case::missing_digits_after_signed("8'sd", "missing digits")]
#[case::missing_base("8'", "missing a base")]
#[case::invalid_base("8'x0", "invalid numeric base")]
#[case::invalid_digits("8'hGG", "invalid digits")]
#[case::signed_out_of_range("2'sd2", "does not fit")]
fn rejects_malformed_literals(#[case] input: &str, #[case] expected_substring: &str) {
    assert_numeric_parse_error(input, expected_substring);
}

#[test]
fn rejects_negative_unsigned() {
    let errors = match parse_expression("8'h-1") {
        Ok(expr) => panic!("expected failure, got {expr:?}"),
        Err(errors) => errors,
    };
    assert_parse_error(&errors, "cannot encode negative", 0, 5);
}

fn unwrap_float(expr: Expr) -> (Option<u32>, String) {
    match expr {
        Expr::Literal(Literal::Number(NumberLiteral::Float(float))) => (float.width, float.raw),
        other => panic!("expected float literal, got {other:?}"),
    }
}

#[test]
fn parses_unqualified_floats() {
    let test_cases = [
        ("1.0", None, "1.0"),
        ("1e10", None, "1e10"),
        ("1.0e-3", None, "1.0e-3"),
    ];

    for (input, expected_width, expected_raw) in test_cases {
        let expr = match parse_expression(input) {
            Ok(expr) => expr,
            Err(errors) => panic!("parse of {input:?} should succeed, got {errors:?}"),
        };
        let (width, raw) = unwrap_float(expr);
        assert_eq!(width, expected_width, "{input}: width mismatch");
        assert_eq!(raw, expected_raw, "{input}: raw mismatch");
    }
}

#[test]
fn valid_token_invalid_value_emits_single_diagnostic() {
    // Test that a tokenizer-valid but parser-invalid literal emits one error.
    // "1'b10" tokenizes as a number but fails validation (value 2 doesn't fit 1 bit).
    let errors = match parse_expression("1'b10") {
        Ok(expr) => panic!("expected failure, got {expr:?}"),
        Err(errors) => errors,
    };
    assert_eq!(errors.len(), 1, "should emit exactly one diagnostic");
}
