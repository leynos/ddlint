//! Compile checks for the fixture expansion lint suppression.
//!
//! The pass case proves that `#[allow_fixture_expansion_lints]` masks the
//! `unused_braces` diagnostic that `rstest` introduces for a single-line,
//! single-expression fixture. The three compile-fail cases prove that the
//! suppression is absent without the attribute, that it does not mask a
//! genuine `unused_braces` finding in a multi-statement fixture, and that a
//! single statement with a trailing semicolon is not treated as a qualifying
//! expression body.

#[test]
fn fixture_expansion_lint_scope_is_enforced() {
    let cases = trybuild::TestCases::new();
    cases.pass("tests/ui/fixture_expansion_pass.rs");
    cases.compile_fail("tests/ui/fixture_expansion_fail.rs");
    cases.compile_fail("tests/ui/fixture_expansion_masking.rs");
    cases.compile_fail("tests/ui/fixture_expansion_semicolon.rs");
}
