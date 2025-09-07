//! Focused tests for the `Expr::to_sexpr` formatting.

use ddlint::parser::expression::parse_expression;
use rstest::rstest;

#[rstest]
#[case(r#""foo bar""#)]
#[case(r#""quote \"marks\"""#)]
#[case(r#""(paren)""#)]
fn string_literals_preserve_quoting(#[case] src: &str) {
    let expr = parse_expression(src).unwrap_or_else(|e| panic!("source {src:?} errors: {e:?}"));
    let inner = src
        .strip_prefix('"')
        .and_then(|s| s.strip_suffix('"'))
        .map_or_else(|| panic!("unterminated string"), |v| v);
    assert_eq!(expr.to_sexpr(), format!("{inner:?}"));
}

#[rstest]
#[case("foo(bar(baz()))", "(call foo (call bar (call baz)))")]
#[case("foo.bar(baz()).qux()", "(method (method foo bar (call baz)) qux)")]
fn nested_invocations_format_stably(#[case] src: &str, #[case] expected: &str) {
    let expr = parse_expression(src).unwrap_or_else(|e| panic!("source {src:?} errors: {e:?}"));
    assert_eq!(expr.to_sexpr(), expected);
}

#[rstest]
#[case("Foo { a: 1, b: 2 }", "(struct Foo (a 1) (b 2))")]
#[case("Foo { b: 2, a: 1 }", "(struct Foo (b 2) (a 1))")]
fn struct_field_order_is_stable(#[case] src: &str, #[case] expected: &str) {
    let expr = parse_expression(src).unwrap_or_else(|e| panic!("source {src:?} errors: {e:?}"));
    assert_eq!(expr.to_sexpr(), expected);
}
