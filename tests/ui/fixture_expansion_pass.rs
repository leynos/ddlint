//! Pass case: a single-expression fixture suppresses `unused_braces`.
//!
//! The fixture body stays on one line so `rstest` re-emits it as
//! `{ { "input" } }`, which rustc would otherwise deny.

#![deny(unused_braces)]

use fixture_lints::allow_fixture_expansion_lints;
use rstest::{fixture, rstest};

#[allow_fixture_expansion_lints]
#[fixture]
fn source() -> &'static str { "input relation R(x: u32);" }

#[rstest]
fn consumes_fixture(source: &str) {
    assert_eq!(source, "input relation R(x: u32);");
}

fn main() {}