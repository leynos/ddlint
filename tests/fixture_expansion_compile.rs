//! Compile checks for the fixture expansion lint suppression.
//!
//! The first pass case proves that `#[allow_fixture_expansion_lints]` masks the
//! `unused_braces` diagnostic that `rstest` introduces for a single-line,
//! single-expression fixture. The second pass case proves that the attribute is
//! transparent for an item that is not a function. The first two compile-fail
//! cases prove that the suppression is absent without the attribute and that it
//! does not mask a genuine `unused_braces` finding in a multi-statement
//! fixture; the third proves that a single statement with a trailing semicolon
//! is not treated as a qualifying expression body, and the fourth proves that
//! an unparsable non-function item keeps its own `unused_braces` diagnostic.

#[test]
fn fixture_expansion_lint_scope_is_enforced() {
    let cases = trybuild::TestCases::new();
    cases.pass("tests/ui/fixture_expansion_pass.rs");
    cases.pass("tests/ui/fixture_expansion_non_function_pass.rs");
    cases.compile_fail("tests/ui/fixture_expansion_fail.rs");
    cases.compile_fail("tests/ui/fixture_expansion_masking.rs");
    cases.compile_fail("tests/ui/fixture_expansion_semicolon.rs");
    cases.compile_fail("tests/ui/fixture_expansion_non_function_fail.rs");
}
