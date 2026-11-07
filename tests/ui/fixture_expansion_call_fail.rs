//! Compile-fail case: an unannotated function-call fixture is denied.
//!
//! Without `#[allow_fixture_expansion_lints]`, `rstest` re-emits the call
//! expression body as `{ { make() } }` and rustc denies the redundant braces.

#![deny(unused_braces)]

use rstest::{fixture, rstest};

/// Produce the value the fixture returns.
fn make() -> &'static str { "input relation R(x: u32);" }

#[fixture]
fn source() -> &'static str { make() }

#[rstest]
fn consumes_fixture(source: &str) {
    assert_eq!(source, "input relation R(x: u32);");
}

fn main() {}