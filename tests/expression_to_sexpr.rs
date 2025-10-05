//! Focused tests for the `Expr::to_sexpr` formatting.

#![expect(
    clippy::expect_used,
    clippy::expect_fun_call,
    reason = "tests assert exact behaviour"
)]

use ddlint::parser::expression::parse_expression;
use rstest::rstest;

#[rstest]
#[case(r#""foo bar""#)]
#[case(r#""quote \"marks\"""#)]
#[case(r#""(paren)""#)]
fn string_literals_preserve_quoting(#[case] src: &str) {
    let expr = parse_expression(src).expect(&format!("source {src:?} errors"));
    let inner = src
        .strip_prefix('"')
        .and_then(|s| s.strip_suffix('"'))
        .expect("unterminated string");
    assert_eq!(expr.to_sexpr(), format!("{inner:?}"));
}

#[rstest]
#[case("foo(bar(baz()))", "(call foo (call bar (call baz)))")]
#[case("foo.bar(baz()).qux()", "(method (method foo bar (call baz)) qux)")]
fn nested_invocations_format_stably(#[case] src: &str, #[case] expected: &str) {
    let expr = parse_expression(src).expect(&format!("source {src:?} errors"));
    assert_eq!(expr.to_sexpr(), expected);
}

#[rstest]
#[case("Foo { a: 1, b: 2 }", "(struct Foo (a 1) (b 2))")]
#[case("Foo { b: 2, a: 1 }", "(struct Foo (b 2) (a 1))")]
fn struct_field_order_is_stable(#[case] src: &str, #[case] expected: &str) {
    let expr = parse_expression(src).expect(&format!("source {src:?} errors"));
    assert_eq!(expr.to_sexpr(), expected);
}

#[rstest]
#[case("-5 + 2", "(+ (- 5) 2)")]
#[case("1 * 2 + 3", "(+ (* 1 2) 3)")]
#[case("a.b.c", "(field (field a b) c)")]
#[case("(x)", "(group x)")]
#[case("(x, 1)", "(tuple x 1)")]
#[case("f()", "(call f)")]
#[case("f(1, 2)", "(call f 1 2)")]
#[case("a.b(1).c[7,0]", "(bitslice (field (method a b 1) c) 7 0)")]
#[case("not x", "(not x)")]
#[case("x and y", "(and x y)")]
#[case("x or y", "(or x y)")]
#[case("for (item in items) item", "(for item items item)")]
#[case(
    "for (entry in items if entry.active) process(entry)",
    "(for entry items (field entry active) (call process entry))"
)]
#[case(
    "for (entry in items(entry) if entry.active) process(entry)",
    "(for entry (call items entry) (field entry active) (call process entry))"
)]
fn more_variants(#[case] src: &str, #[case] expected: &str) {
    let expr = parse_expression(src).expect(&format!("source {src:?} errors"));
    assert_eq!(expr.to_sexpr(), expected);
}

#[rstest]
#[case("|x, y| x + y", "(closure (x y) (+ x y))")]
fn closures_format(#[case] src: &str, #[case] expected: &str) {
    let expr = parse_expression(src).expect(&format!("source {src:?} errors"));
    assert_eq!(expr.to_sexpr(), expected);
}
