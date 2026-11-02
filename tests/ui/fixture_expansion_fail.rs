//! Compile-fail case: an unannotated single-expression fixture is denied.
//!
//! Without `#[allow_fixture_expansion_lints]`, `rstest` re-emits the fixture
//! body as `{ { "input" } }` and rustc denies the redundant braces.

#![deny(unused_braces)]

use rstest::{fixture, rstest};

#[fixture]
fn source() -> &'static str { "input relation R(x: u32);" }

#[rstest]
fn consumes_fixture(source: &str) {
    assert_eq!(source, "input relation R(x: u32);");
}

fn main() {}