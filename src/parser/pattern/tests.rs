//! Unit tests for the pattern parser.

use super::*;
use crate::parser::ast::Pattern;

#[expect(clippy::expect_used, reason = "tests assert patterns parse")]
#[test]
fn parses_var_and_wildcard_patterns() {
    assert_eq!(
        parse_pattern("var x").expect("pattern should parse"),
        Pattern::Var {
            declared: true,
            name: "x".to_string()
        }
    );
    assert_eq!(
        parse_pattern("_").expect("pattern should parse"),
        Pattern::Wildcard
    );
}

#[expect(clippy::expect_used, reason = "tests assert patterns parse")]
#[test]
fn parses_tuple_patterns() {
    assert_eq!(
        parse_pattern("(x, y)").expect("pattern should parse"),
        Pattern::Tuple(vec![
            Pattern::Var {
                declared: false,
                name: "x".to_string()
            },
            Pattern::Var {
                declared: false,
                name: "y".to_string()
            }
        ])
    );
}

#[expect(clippy::expect_used, reason = "tests assert patterns parse")]
#[test]
fn parses_struct_patterns() {
    let pat = parse_pattern("Point { x: var a, y: _ }").expect("pattern should parse");
    assert_eq!(pat.to_source(), "Point { x: var a, y: _ }".to_string());
}

#[expect(clippy::expect_used, reason = "tests assert patterns parse")]
#[test]
fn parses_typed_patterns() {
    let pat = parse_pattern("var x: Vec<u32>").expect("pattern should parse");
    assert_eq!(pat.to_source(), "var x: Vec<u32>".to_string());
}

#[test]
fn rejects_interpolated_string_literals() {
    let Err(errs) = parse_pattern(r#""${x}""#) else {
        panic!("expected error");
    };
    assert!(
        errs.iter()
            .any(|e| format!("{e:?}").contains("interpolated strings are not allowed")),
        "expected interpolation diagnostic, got {errs:?}"
    );
}
