//! Compile-fail case: the suppression must not mask a genuine finding.
//!
//! The fixture has multiple statements, so `#[allow_fixture_expansion_lints]`
//! leaves it unchanged. Its `return { value };` is a real `unused_braces`
//! finding and the crate-level denial still rejects it.

#![deny(unused_braces)]

use fixture_lints::allow_fixture_expansion_lints;
use rstest::{fixture, rstest};

#[allow_fixture_expansion_lints]
#[fixture]
fn source() -> &'static str {
    let value = "input relation R(x: u32);";
    return { value };
}

#[rstest]
fn consumes_fixture(source: &str) {
    assert_eq!(source, "input relation R(x: u32);");
}

fn main() {}