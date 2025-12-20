//! Tests for assignment splitting in rule bodies.

use crate::parser::ast::rule::split_assignment;

#[expect(
    clippy::expect_used,
    reason = "tests assert split_assignment succeeds for representative inputs"
)]
#[test]
fn split_assignment_ignores_equals_in_parens() {
    let src = "foo = bar(baz = 1)";
    let parts =
        split_assignment(src).expect("expected split_assignment to find a top-level assignment");

    assert_eq!(parts.pattern, "foo");
    assert_eq!(parts.value, "bar(baz = 1)");
}

#[expect(
    clippy::expect_used,
    reason = "tests assert split_assignment succeeds for representative inputs"
)]
#[test]
fn split_assignment_ignores_equals_in_brackets_and_braces() {
    let src_brackets = r#"foo = map["a=b"]"#;
    let parts = split_assignment(src_brackets)
        .expect("expected split_assignment to split at the top-level =");
    assert_eq!(parts.pattern, "foo");
    assert_eq!(parts.value, r#"map["a=b"]"#);

    let src_braces = "foo = { key = value }";
    let parts = split_assignment(src_braces)
        .expect("expected split_assignment to split at the top-level =");
    assert_eq!(parts.pattern, "foo");
    assert_eq!(parts.value, "{ key = value }");
}

#[expect(
    clippy::expect_used,
    reason = "tests assert split_assignment succeeds for representative inputs"
)]
#[test]
fn split_assignment_trims_pattern_and_value_whitespace() {
    let src = "   foo   =   bar(baz)   ";
    let parts =
        split_assignment(src).expect("expected split_assignment to split at the top-level =");

    assert_eq!(parts.pattern, "foo");
    assert_eq!(parts.value, "bar(baz)");

    let src_mixed = "foo=   bar   ";
    let parts =
        split_assignment(src_mixed).expect("expected split_assignment to split at the top-level =");
    assert_eq!(parts.pattern, "foo");
    assert_eq!(parts.value, "bar");
}
