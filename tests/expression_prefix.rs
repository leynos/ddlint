//! Integration tests for prefix forms and diagnostics.
//!
//! These tests exercise brace grouping, tuple disambiguation, closures,
//! struct literals, and literal parsing through the public `parse_expression`
//! API.

use ddlint::parser::ast::Expr;
use ddlint::parser::expression::parse_expression;
use ddlint::test_util::{
    assert_delimiter_error, assert_parse_error, closure, field, lit_bool, lit_interned_str,
    lit_num, lit_raw_interpolated_str, lit_raw_str, lit_str, map_lit, match_arm, match_expr,
    return_expr, struct_expr, tuple, var, vec_lit,
};
use rstest::rstest;

#[rstest]
#[case("{}", map_lit(vec![]))]
#[case("{1}", Expr::Group(Box::new(lit_num("1"))))]
#[case("(1)", Expr::Group(Box::new(lit_num("1"))))]
#[case("[]", vec_lit(vec![]))]
#[case("(1,)", tuple(vec![lit_num("1")]))]
#[case("|x| x", closure(vec!["x"], var("x")))]
#[case("|a,b| a", closure(vec!["a", "b"], var("a")))]
#[case(
    "Point { x: 1, y: 2 }",
    struct_expr("Point", vec![field("x", lit_num("1")), field("y", lit_num("2"))]),
)]
#[case("{ x }", Expr::Group(Box::new(var("x"))))]
#[case("\"hi\"", lit_str("hi"))]
#[case("[|raw|]", lit_raw_str("raw"))]
#[case("$[|raw ${x}|]", lit_raw_interpolated_str("raw ${x}"))]
#[case("i\"hi\"", lit_interned_str("hi"))]
#[case("false", lit_bool(false))]
#[case(
    "match (flag) { true -> 1 }",
    match_expr(var("flag"), vec![match_arm("true", lit_num("1"))]),
)]
// Guard against the single-arm fast path skipping trailing-comma validation.
#[case(
    "match (flag) { true -> 1, }",
    match_expr(var("flag"), vec![match_arm("true", lit_num("1"))]),
)]
#[case(
    "match (flag) { true -> false, false -> true }",
    match_expr(
        var("flag"),
        vec![
            match_arm("true", lit_bool(false)),
            match_arm("false", lit_bool(true)),
        ],
    ),
)]
#[case(
    "match (flag) { true -> false, false -> true, }",
    match_expr(
        var("flag"),
        vec![
            match_arm("true", lit_bool(false)),
            match_arm("false", lit_bool(true)),
        ],
    ),
)]
#[case(
    "match (item) { Point { x: x } -> x, _ -> 0 }",
    match_expr(
        var("item"),
        vec![
            match_arm("Point { x: x }", var("x")),
            match_arm("_", lit_num("0")),
        ],
    ),
)]
#[case(
    "match (x) { \"\\${literal}\" -> 1 }",
    match_expr(
        var("x"),
        vec![match_arm("\"\\${literal}\"", lit_num("1"))],
    ),
)]
fn parses_prefix_forms(#[case] src: &str, #[case] expected: Expr) {
    let expr = parse_expression(src).unwrap_or_else(|e| panic!("source {src:?} errors: {e:?}"));
    assert_eq!(expr, expected);
}

#[rstest]
#[case("return", return_expr(None))]
#[case("return value", return_expr(Some(var("value"))))]
#[case(
    "return (x, y)",
    return_expr(Some(tuple(vec![var("x"), var("y")]))),
)]
#[case("{ return }", Expr::Group(Box::new(return_expr(None))))]
#[case("(return)", Expr::Group(Box::new(return_expr(None))))]
// Return with no explicit value before a terminator (`)`, `}`, `,`, `;`, or
// `->`) yields unit `()` so match arms, loop bodies, and grouped expressions can
// omit it safely.
#[case(
    "match (x) { _ -> return }",
    match_expr(var("x"), vec![match_arm("_", return_expr(None))]),
)]
fn parses_return_prefix_forms(#[case] src: &str, #[case] expected: Expr) {
    let expr = parse_expression(src).unwrap_or_else(|e| panic!("source {src:?} errors: {e:?}"));
    assert_eq!(expr, expected);
}

#[rstest]
// Note: `{}` is now a valid empty map literal, so it's not an error.
#[case("|x x", "expected pipe", 3, 4, false)]
#[case("match (x) {}", "expected at least one match arm", 11, 12, false)]
#[case(
    "match (x) { _ -> x",
    "expected ',' or '}' after match arm",
    18,
    18,
    false
)]
fn prefix_form_errors(
    #[case] src: &str,
    #[case] msg: &str,
    #[case] start: usize,
    #[case] end: usize,
    #[case] unclosed: bool,
) {
    let Err(errors) = parse_expression(src) else {
        panic!("expected error");
    };
    if unclosed {
        assert_delimiter_error(&errors, msg, start, end);
    } else {
        let Some(error) = errors.first() else {
            panic!("error missing");
        };
        let single = vec![error.clone()];
        assert_parse_error(&single, msg, start, end);
    }
}

#[test]
fn match_pattern_rejects_interpolated_string() {
    let src = r#"match (x) { "${y}" -> 1 }"#;
    let Err(errors) = parse_expression(src) else {
        panic!("expected error");
    };
    let Some(start) = src.find("\"${y}\"") else {
        panic!("string literal span should exist in {src}");
    };
    let end = start + "\"${y}\"".len();
    assert_parse_error(
        &errors,
        "interpolated strings are not allowed in patterns",
        start,
        end,
    );
}

#[test]
fn for_pattern_rejects_interpolated_raw_string() {
    let src = "for ($[|user ${name}|] in items) user";
    let Err(errors) = parse_expression(src) else {
        panic!("expected error");
    };
    let Some(start) = src.find("$[|user ${name}|]") else {
        panic!("raw string span should exist in {src}");
    };
    let end = start + "$[|user ${name}|]".len();
    assert_parse_error(
        &errors,
        "interpolated strings are not allowed in patterns",
        start,
        end,
    );
}

#[test]
fn match_pattern_rejects_uppercase_ident_without_braces() {
    let src = "match (x) { Point -> 1 }";
    let Err(errors) = parse_expression(src) else {
        panic!("expected error");
    };
    let Some(start) = src.find("Point") else {
        panic!("identifier span should exist in {src}");
    };
    let end = start + "Point".len();
    assert_parse_error(
        &errors,
        "expected '{' to start a struct pattern",
        start,
        end,
    );
}

#[test]
fn for_pattern_rejects_uppercase_ident_without_braces() {
    let src = "for (Point in items) item";
    let Err(errors) = parse_expression(src) else {
        panic!("expected error");
    };
    let Some(start) = src.find("Point") else {
        panic!("identifier span should exist in {src}");
    };
    let end = start + "Point".len();
    assert_parse_error(
        &errors,
        "expected '{' to start a struct pattern",
        start,
        end,
    );
}
