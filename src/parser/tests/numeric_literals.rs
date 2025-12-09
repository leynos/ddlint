//! Tests for width-qualified numeric literal parsing and validation.

use crate::parser::ast::{Expr, IntBase, Literal, NumberLiteral};
use crate::parser::expression::{parse_expression, parse_numeric_literal};
use crate::test_util::{assert_parse_error, lit_num};

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

#[test]
fn parses_integer_literals() {
    let test_cases = [
        (
            "8'hFF",
            ExpectedIntLiteral {
                width: Some(8),
                signed: false,
                base: IntBase::Hex,
                value: "255",
            },
        ),
        (
            "16'sd-1",
            ExpectedIntLiteral {
                width: Some(16),
                signed: true,
                base: IntBase::Decimal,
                value: "-1",
            },
        ),
        (
            "0x1e",
            ExpectedIntLiteral {
                width: None,
                signed: false,
                base: IntBase::Hex,
                value: "30",
            },
        ),
        (
            "4'b1010",
            ExpectedIntLiteral {
                width: Some(4),
                signed: false,
                base: IntBase::Binary,
                value: "10",
            },
        ),
        (
            "8'o377",
            ExpectedIntLiteral {
                width: Some(8),
                signed: false,
                base: IntBase::Octal,
                value: "255",
            },
        ),
        (
            "8'h+FF",
            ExpectedIntLiteral {
                width: Some(8),
                signed: false,
                base: IntBase::Hex,
                value: "255",
            },
        ),
        (
            "16'sd+1",
            ExpectedIntLiteral {
                width: Some(16),
                signed: true,
                base: IntBase::Decimal,
                value: "1",
            },
        ),
        (
            "16'hFF_FF",
            ExpectedIntLiteral {
                width: Some(16),
                signed: false,
                base: IntBase::Hex,
                value: "65535",
            },
        ),
        (
            "0b1010",
            ExpectedIntLiteral {
                width: None,
                signed: false,
                base: IntBase::Binary,
                value: "10",
            },
        ),
        (
            "0o17",
            ExpectedIntLiteral {
                width: None,
                signed: false,
                base: IntBase::Octal,
                value: "15",
            },
        ),
    ];

    for (input, expected) in test_cases {
        assert_int_literal(input, expected);
    }
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

#[test]
fn rejects_missing_digits() {
    let err = match parse_numeric_literal("8'h") {
        Ok(lit) => panic!("expected failure, got {lit:?}"),
        Err(err) => err,
    };
    assert!(
        err.message().contains("missing digits"),
        "expected 'missing digits' error, got: {}",
        err.message()
    );
}

#[test]
fn rejects_invalid_base() {
    let err = match parse_numeric_literal("8'x0") {
        Ok(lit) => panic!("expected failure, got {lit:?}"),
        Err(err) => err,
    };
    assert!(
        err.message().contains("invalid numeric base"),
        "expected 'invalid numeric base' error, got: {}",
        err.message()
    );
}

#[test]
fn rejects_invalid_digits() {
    let err = match parse_numeric_literal("8'hGG") {
        Ok(lit) => panic!("expected failure, got {lit:?}"),
        Err(err) => err,
    };
    assert!(
        err.message().contains("invalid digits"),
        "expected 'invalid digits' error, got: {}",
        err.message()
    );
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
