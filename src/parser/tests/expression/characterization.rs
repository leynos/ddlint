//! Characterization snapshot of expression parsing outside braces.
//!
//! `V-B7` requires that the block-expression work changes nothing outside
//! braces. This module records how every non-brace valid expression fixture
//! renders and every diagnostic each non-brace error fixture reports, so a
//! later accidental change appears as a snapshot diff instead of passing
//! unnoticed.
//!
//! Brace-bearing fixtures are excluded because their expectations change by
//! design. The snapshots are not a wholly pre-change baseline: bracket map
//! literals such as `[a -> 1]`, and the diagnostics for `[]` and
//! `[a, b -> c]`, are recorded with their post-change behaviour, so a diff in
//! those rows is expected whenever that syntax changes.

use super::fixtures::{error_cases, expression_cases};
use crate::parser::expression::parse_expression;

/// Error returned when a fixture no longer behaves as the characterization
/// recorded, naming the source that broke it.
type RenderError = Box<dyn std::error::Error>;

/// Returns `true` when `src` lies outside the characterization's scope.
fn is_out_of_scope(src: &str) -> bool { src.contains('{') }

/// Render each in-scope valid fixture as `source => s-expression`.
///
/// A fixture that stops parsing means the characterization no longer covers
/// the expression surface it was recorded for, so the failing source is
/// reported instead of silently dropping the row.
fn render_valid_expressions() -> Result<Vec<String>, RenderError> {
    expression_cases()
        .into_iter()
        .filter(|case| !is_out_of_scope(case.src))
        .map(|case| {
            let expr = parse_expression(case.src)
                .map_err(|errs| format!("source {:?} errors: {errs:?}", case.src))?;
            Ok(format!("{:?} => {}", case.src, expr.to_sexpr()))
        })
        .collect()
}

/// Render every diagnostic each in-scope error fixture reports, in order.
///
/// Recording the whole list stops a source that gains a second diagnostic
/// from passing while its first diagnostic stays unchanged.
fn render_error_diagnostics() -> Result<Vec<String>, RenderError> {
    let mut rendered = Vec::new();
    for case in error_cases()
        .into_iter()
        .filter(|case| !is_out_of_scope(case.src))
    {
        let errors = parse_expression(case.src)
            .err()
            .ok_or_else(|| format!("expected parse failure for {:?}", case.src))?;
        if errors.is_empty() {
            return Err(format!("expected a diagnostic for {:?}", case.src).into());
        }
        let messages: Vec<String> = errors.iter().map(|error| format!("{error:?}")).collect();
        rendered.push(format!("{:?} => {}", case.src, messages.join(" | ")));
    }
    Ok(rendered)
}

#[test]
fn non_brace_expression_rendering_is_characterized() {
    let rendered = render_valid_expressions()
        .expect("every non-brace valid fixture should still parse and render");

    assert!(
        rendered.len() > 20,
        "characterization should cover the bulk of the non-brace fixtures, got {}",
        rendered.len()
    );
    assert!(
        rendered
            .iter()
            .any(|row| row == "\"add(x, 1)\" => (call add x 1)"),
        "call shape must survive: {rendered:?}"
    );
    assert!(
        rendered
            .iter()
            .any(|row| row == "\"(1, 2)\" => (tuple 1 2)"),
        "tuple shape must survive: {rendered:?}"
    );

    let snapshot = rendered.join("\n");
    insta::assert_snapshot!(snapshot, @r#"
    "x" => x
    "foo()" => (call foo)
    "pkg::Foo()" => (call pkg::Foo)
    "pkg::foo()" => (call pkg::foo)
    "add(x, 1)" => (call add x 1)
    "(1, 2)" => (tuple 1 2)
    "(1, 2, 3)" => (tuple 1 2 3)
    "(1,)" => (tuple 1)
    "()" => (tuple)
    "(1)" => (group 1)
    "[1]" => (vec 1)
    "[1, 2]" => (vec 1 2)
    "[1, 2, 3]" => (vec 1 2 3)
    "[1,]" => (vec 1)
    "[1, 2,]" => (vec 1 2)
    "[x, y + 1]" => (vec x (+ y 1))
    "[a -> 1]" => (map (entry a 1))
    "[a -> 1, b -> 2]" => (map (entry a 1) (entry b 2))
    "[a -> 1,]" => (map (entry a 1))
    "[x -> y, z -> w]" => (map (entry x y) (entry z w))
    "|x, y| x + y" => (closure (x y) (+ x y))
    "|| 1" => (closure () 1)
    "|x,| x" => (closure (x) x)
    "(|| 1, |x| x)" => (tuple (closure () 1) (closure (x) x))
    "foo.bar(x)" => (method foo bar x)
    "foo.bar(x).baz" => (field (method foo bar x) baz)
    "foo.bar" => (field foo bar)
    "foo.bar.baz(x)" => (method (field foo bar) baz x)
    "foo.bar.baz().qux" => (field (method (field foo bar) baz) qux)
    "foo.bar().baz(x)" => (method (method foo bar) baz x)
    "foo.bar.baz" => (field (field foo bar) baz)
    "foo.bar().baz.qux(x)" => (method (field (method foo bar) baz) qux x)
    "(f)(x)" => (call (group f) x)
    "e[1,0]" => (bitslice e 1 0)
    "t.0" => (tuple-index t 0)
    "e'(x)" => (diff (call e x))
    "e'[1,0]" => (diff (bitslice e 1 0))
    "S'(x)" => (diff (call S x))
    "e-<5>" => (delay 5 e)
    "e-<0>" => (delay 0 e)
    "A() -<10>" => (delay 10 (call A))
    "if flag value" => (if flag value (tuple))
    "for (item in items) item" => (for item items item)
    "for (entry in items if entry.active) process(entry)" => (for entry items (field entry active) (call process entry))
    "#);
}

#[test]
fn non_brace_error_diagnostics_are_characterized() {
    let rendered = render_error_diagnostics()
        .expect("every non-brace error fixture should still fail with a diagnostic");

    assert!(
        !rendered.is_empty(),
        "characterization should record at least one error fixture"
    );

    let snapshot = rendered.join("\n");
    insta::assert_snapshot!(snapshot, @r#"
    "1 +" => Simple { span: 2..3, reason: Custom("missing right-hand side for operator"), expected: {}, found: None, label: None }
    "(1 + 2" => Simple { span: 6..6, reason: Custom("expected T_RPAREN"), expected: {}, found: None, label: None }
    "1 ? 2" => Simple { span: 2..3, reason: Custom("unexpected token: T_QUESTION"), expected: {}, found: None, label: None } | Simple { span: 4..5, reason: Custom("unexpected token: T_NUMBER"), expected: {}, found: None, label: None }
    "x :" => Simple { span: 2..3, reason: Custom("missing right-hand side for operator"), expected: {}, found: None, label: None }
    "x as" => Simple { span: 2..4, reason: Custom("missing right-hand side for operator"), expected: {}, found: None, label: None }
    "x =" => Simple { span: 2..3, reason: Custom("missing right-hand side for operator"), expected: {}, found: None, label: None }
    "x ;" => Simple { span: 2..3, reason: Custom("missing right-hand side for operator"), expected: {}, found: None, label: None }
    "x =>" => Simple { span: 2..4, reason: Custom("missing right-hand side for operator"), expected: {}, found: None, label: None }
    "" => Simple { span: 0..0, reason: Custom("invalid expression"), expected: {}, found: None, label: None }
    "e[1 0]" => Simple { span: 4..5, reason: Custom("expected T_COMMA"), expected: {}, found: None, label: None }
    "e[,0]" => Simple { span: 2..3, reason: Custom("unexpected token: T_COMMA"), expected: {}, found: None, label: None }
    "e[1,]" => Simple { span: 4..5, reason: Custom("unexpected token: T_RBRACKET"), expected: {}, found: None, label: None }
    "e[1,,0]" => Simple { span: 4..5, reason: Custom("unexpected token: T_COMMA"), expected: {}, found: None, label: None }
    "t." => Simple { span: 2..2, reason: Custom("expected identifier or tuple index after '.'"), expected: {}, found: None, label: None }
    "t.-1" => Simple { span: 2..3, reason: Custom("expected identifier or tuple index after '.'"), expected: {}, found: None, label: None }
    "t.+1" => Simple { span: 2..3, reason: Custom("expected identifier or tuple index after '.'"), expected: {}, found: None, label: None }
    "t..0" => Simple { span: 2..3, reason: Custom("expected identifier or tuple index after '.'"), expected: {}, found: None, label: None }
    "if cond else value" => Simple { span: 8..12, reason: Custom("expected expression for 'then' branch of 'if'"), expected: {}, found: None, label: None }
    "if cond value else" => Simple { span: 14..18, reason: Custom("expected expression for 'else' branch of 'if'"), expected: {}, found: None, label: None }
    "if" => Simple { span: 2..2, reason: Custom("expected condition expression after 'if'"), expected: {}, found: None, label: None }
    "[]" => Simple { span: 1..2, reason: Custom("empty vector literals are not DDlog syntax; use vec_empty() instead"), expected: {}, found: None, label: None }
    "[1, 2" => Simple { span: 5..5, reason: Custom("expected T_RBRACKET"), expected: {}, found: None, label: None }
    "[a, b -> c]" => Simple { span: 4..5, reason: Custom("cannot mix vector elements and map entries in one literal"), expected: {}, found: None, label: None }
    "#);
}
