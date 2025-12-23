//! Tests for vector and map literal parsing.
//!
//! Covers parsing of `[e1, e2, ...]` vector literals and `{k: v, ...}` map
//! literals, including edge cases like empty collections, trailing commas,
//! nested structures, and disambiguation from brace groups.

use crate::parser::ast::{BinaryOp, Expr};
use crate::parser::expression::parse_expression;
use crate::test_util::{
    call, field, lit_bool, lit_num, lit_str, map_entry, map_lit, struct_expr, tuple, var, vec_lit,
};
use rstest::rstest;

// ============================================================================
// Vector literal tests
// ============================================================================

#[rstest]
#[case::empty("[]", vec_lit(vec![]))]
#[case::single_num("[1]", vec_lit(vec![lit_num("1")]))]
#[case::multiple_nums("[1, 2, 3]", vec_lit(vec![lit_num("1"), lit_num("2"), lit_num("3")]))]
#[case::trailing_comma("[1,]", vec_lit(vec![lit_num("1")]))]
#[case::trailing_comma_multi("[1, 2,]", vec_lit(vec![lit_num("1"), lit_num("2")]))]
#[case::variables("[x, y]", vec_lit(vec![var("x"), var("y")]))]
#[case::booleans("[true, false]", vec_lit(vec![lit_bool(true), lit_bool(false)]))]
#[case::strings("[\"a\", \"b\"]", vec_lit(vec![lit_str("a"), lit_str("b")]))]
#[case::function_calls("[f(), g(x)]", vec_lit(vec![call("f", vec![]), call("g", vec![var("x")])]))]
fn parses_vector_literals(#[case] src: &str, #[case] expected: Expr) {
    let expr =
        parse_expression(src).unwrap_or_else(|errs| panic!("source {src:?} errors: {errs:?}"));
    assert_eq!(expr, expected);
}

#[rstest]
#[case::nested_empty("[[]]", vec_lit(vec![vec_lit(vec![])]))]
#[case::nested_single("[[1]]", vec_lit(vec![vec_lit(vec![lit_num("1")])]))]
#[case::nested_multiple(
    "[[1], [2, 3]]",
    vec_lit(vec![
        vec_lit(vec![lit_num("1")]),
        vec_lit(vec![lit_num("2"), lit_num("3")]),
    ])
)]
#[case::deeply_nested(
    "[[[1]]]",
    vec_lit(vec![vec_lit(vec![vec_lit(vec![lit_num("1")])])])
)]
fn parses_nested_vectors(#[case] src: &str, #[case] expected: Expr) {
    let expr =
        parse_expression(src).unwrap_or_else(|errs| panic!("source {src:?} errors: {errs:?}"));
    assert_eq!(expr, expected);
}

// ============================================================================
// Map literal tests
// ============================================================================

#[rstest]
#[case::empty("{}", map_lit(vec![]))]
#[case::single_entry("{a: 1}", map_lit(vec![map_entry(var("a"), lit_num("1"))]))]
#[case::multiple_entries(
    "{a: 1, b: 2}",
    map_lit(vec![
        map_entry(var("a"), lit_num("1")),
        map_entry(var("b"), lit_num("2")),
    ])
)]
#[case::trailing_comma("{a: 1,}", map_lit(vec![map_entry(var("a"), lit_num("1"))]))]
#[case::trailing_comma_multi(
    "{a: 1, b: 2,}",
    map_lit(vec![
        map_entry(var("a"), lit_num("1")),
        map_entry(var("b"), lit_num("2")),
    ])
)]
#[case::numeric_keys("{1: x, 2: y}", map_lit(vec![
    map_entry(lit_num("1"), var("x")),
    map_entry(lit_num("2"), var("y")),
]))]
#[case::string_keys(
    "{\"key\": value}",
    map_lit(vec![map_entry(lit_str("key"), var("value"))])
)]
#[case::boolean_values(
    "{a: true, b: false}",
    map_lit(vec![
        map_entry(var("a"), lit_bool(true)),
        map_entry(var("b"), lit_bool(false)),
    ])
)]
fn parses_map_literals(#[case] src: &str, #[case] expected: Expr) {
    let expr =
        parse_expression(src).unwrap_or_else(|errs| panic!("source {src:?} errors: {errs:?}"));
    assert_eq!(expr, expected);
}

#[rstest]
#[case::empty_key(
    "{{}: 1}",
    map_lit(vec![map_entry(map_lit(vec![]), lit_num("1"))])
)]
#[case::nested_value(
    "{a: {b: 2}}",
    map_lit(vec![map_entry(
        var("a"),
        map_lit(vec![map_entry(var("b"), lit_num("2"))]),
    )])
)]
#[case::nested_key_and_value(
    "{{x: 1}: {y: 2}}",
    map_lit(vec![map_entry(
        map_lit(vec![map_entry(var("x"), lit_num("1"))]),
        map_lit(vec![map_entry(var("y"), lit_num("2"))]),
    )])
)]
fn parses_nested_maps(#[case] src: &str, #[case] expected: Expr) {
    let expr =
        parse_expression(src).unwrap_or_else(|errs| panic!("source {src:?} errors: {errs:?}"));
    assert_eq!(expr, expected);
}

// ============================================================================
// Brace group backward compatibility
// ============================================================================

#[rstest]
#[case::simple_var("{ x }", Expr::Group(Box::new(var("x"))))]
#[case::simple_num("{ 1 }", Expr::Group(Box::new(lit_num("1"))))]
#[case::function_call("{ f() }", Expr::Group(Box::new(call("f", vec![]))))]
#[case::tuple("{ (x, y) }", Expr::Group(Box::new(tuple(vec![var("x"), var("y")]))))]
#[case::binary_expr(
    "{ 1 + 2 }",
    Expr::Group(Box::new(Expr::Binary {
        op: BinaryOp::Add,
        lhs: Box::new(lit_num("1")),
        rhs: Box::new(lit_num("2")),
    }))
)]
#[case::low_precedence_op(
    "{ a and b }",
    Expr::Group(Box::new(Expr::Binary {
        op: BinaryOp::And,
        lhs: Box::new(var("a")),
        rhs: Box::new(var("b")),
    }))
)]
fn parses_brace_groups(#[case] src: &str, #[case] expected: Expr) {
    let expr =
        parse_expression(src).unwrap_or_else(|errs| panic!("source {src:?} errors: {errs:?}"));
    assert_eq!(expr, expected);
}

// ============================================================================
// Mixed collection contexts
// ============================================================================

#[rstest]
#[case::vector_in_map(
    "{items: [1, 2]}",
    map_lit(vec![map_entry(
        var("items"),
        vec_lit(vec![lit_num("1"), lit_num("2")]),
    )])
)]
#[case::map_in_vector(
    "[{a: 1}]",
    vec_lit(vec![map_lit(vec![map_entry(var("a"), lit_num("1"))])])
)]
#[case::struct_with_vector(
    "Point { items: [1, 2] }",
    struct_expr("Point", vec![field("items", vec_lit(vec![lit_num("1"), lit_num("2")]))])
)]
#[case::struct_with_map(
    "Config { data: {a: 1} }",
    struct_expr("Config", vec![field("data", map_lit(vec![map_entry(var("a"), lit_num("1"))]))])
)]
fn parses_mixed_collections(#[case] src: &str, #[case] expected: Expr) {
    let expr =
        parse_expression(src).unwrap_or_else(|errs| panic!("source {src:?} errors: {errs:?}"));
    assert_eq!(expr, expected);
}

// ============================================================================
// S-expression output tests
// ============================================================================

#[test]
fn vector_to_sexpr_empty() {
    let v = vec_lit(vec![]);
    assert_eq!(v.to_sexpr(), "(vec)");
}

#[test]
fn vector_to_sexpr_single() {
    let v = vec_lit(vec![lit_num("1")]);
    assert_eq!(v.to_sexpr(), "(vec 1)");
}

#[test]
fn vector_to_sexpr_multiple() {
    let v = vec_lit(vec![lit_num("1"), var("x")]);
    assert_eq!(v.to_sexpr(), "(vec 1 x)");
}

#[test]
fn map_to_sexpr_empty() {
    let m = map_lit(vec![]);
    assert_eq!(m.to_sexpr(), "(map)");
}

#[test]
fn map_to_sexpr_single() {
    let m = map_lit(vec![map_entry(var("a"), lit_num("1"))]);
    assert_eq!(m.to_sexpr(), "(map (a 1))");
}

#[test]
fn map_to_sexpr_multiple() {
    let m = map_lit(vec![
        map_entry(var("a"), lit_num("1")),
        map_entry(var("b"), lit_num("2")),
    ]);
    assert_eq!(m.to_sexpr(), "(map (a 1) (b 2))");
}

// ============================================================================
// Error cases
// ============================================================================

#[rstest]
#[case::vector_unclosed("[1, 2")]
#[case::map_unclosed("{a: 1")]
#[case::map_comma_without_colon("{a, b}")]
#[case::map_missing_value("{a:}")]
fn reports_collection_errors(#[case] src: &str) {
    let Err(errors) = parse_expression(src) else {
        panic!("expected parse failure for {src}");
    };
    assert!(
        !errors.is_empty(),
        "expected at least one error for {src}, got none"
    );
}
