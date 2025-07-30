//! Tests for expression span utilities.

use crate::parser::expression_span::{ExpressionError, validate_expression};

#[test]
fn validate_expression_reports_out_of_bounds() {
    let src = "a + b";
    let span = 0..src.len() + 5;
    match validate_expression(src, span.clone()) {
        Err(ExpressionError::OutOfBounds { span: sp }) => assert_eq!(sp, span),
        _ => panic!("expected out-of-bounds error"),
    }
}
