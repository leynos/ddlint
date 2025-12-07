//! Tests for width-qualified numeric literal parsing and validation.

use crate::parser::ast::{Expr, IntBase, Literal, NumberLiteral};
use crate::parser::expression::parse_expression;
use crate::test_util::{assert_parse_error, lit_num};

fn unwrap_int(expr: Expr) -> (Option<u32>, bool, IntBase, String) {
    match expr {
        Expr::Literal(Literal::Number(NumberLiteral::Int(int))) => {
            (int.width, int.signed, int.base, int.value.to_string())
        }
        other => panic!("expected int literal, got {other:?}"),
    }
}

#[test]
fn parses_unsigned_width_hex_literal() {
    let expr = match parse_expression("8'hFF") {
        Ok(expr) => expr,
        Err(errors) => panic!("parse should succeed, got {errors:?}"),
    };
    let (width, signed, base, value) = unwrap_int(expr);
    assert_eq!(width, Some(8));
    assert!(!signed);
    assert_eq!(base, IntBase::Hex);
    assert_eq!(value, "255");
}

#[test]
fn parses_signed_width_decimal_literal() {
    let expr = match parse_expression("16'sd-1") {
        Ok(expr) => expr,
        Err(errors) => panic!("parse should succeed, got {errors:?}"),
    };
    let (width, signed, base, value) = unwrap_int(expr);
    assert_eq!(width, Some(16));
    assert!(signed);
    assert_eq!(base, IntBase::Decimal);
    assert_eq!(value, "-1");
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
    assert_parse_error(&errors, "width must be positive", 0, 4);
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
fn parses_plain_hex_without_float_confusion() {
    let expr = match parse_expression("0x1e") {
        Ok(expr) => expr,
        Err(errors) => panic!("parse should succeed, got {errors:?}"),
    };
    let (width, signed, base, value) = unwrap_int(expr);
    assert_eq!(width, None);
    assert!(!signed);
    assert_eq!(base, IntBase::Hex);
    assert_eq!(value, "30");
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
