//! Compile-time checks for the const public APIs promoted by the lint
//! baseline.
//!
//! The pass fixture binds each API's result in a `const` item, so removing
//! `const` from an API turns the fixture into a compile error.

#[test]
fn representative_const_apis_are_usable_in_const_items() {
    let cases = trybuild::TestCases::new();
    cases.pass("tests/ui/const_api_pass.rs");
}
