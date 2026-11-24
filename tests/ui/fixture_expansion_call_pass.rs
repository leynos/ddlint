//! Pass case: a function-call fixture body suppresses `unused_braces`.
//!
//! The fixture is a single call expression rather than a literal, so `rstest`
//! re-emits it as `{ { make() } }`, which rustc would otherwise deny. The
//! suppression must cover this body shape as well as a literal body.

#![deny(unused_braces)]

use fixture_lints::allow_fixture_expansion_lints;
use rstest::{fixture, rstest};

/// Produce the value the fixture returns.
fn make() -> &'static str { "input relation R(x: u32);" }

#[allow_fixture_expansion_lints]
#[fixture]
fn source() -> &'static str { make() }

#[rstest]
fn consumes_fixture(source: &str) {
    assert_eq!(source, "input relation R(x: u32);");
}

fn main() {}