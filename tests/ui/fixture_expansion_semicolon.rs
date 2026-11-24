//! Compile-fail case: a trailing semicolon disqualifies the suppression.
//!
//! The fixture body is a single genuine braced return ending in `;`, so it is
//! `Stmt::Expr(_, Some(;))` rather than the `Stmt::Expr(_, None)` the helper
//! qualifies. `#[allow_fixture_expansion_lints]` leaves the fixture unchanged
//! and rustc denies the real `unused_braces` finding.

#![deny(unused_braces)]

use fixture_lints::allow_fixture_expansion_lints;
use rstest::{fixture, rstest};

#[allow_fixture_expansion_lints]
#[fixture]
fn source() -> &'static str { return { "input relation R(x: u32);" }; }

#[rstest]
fn consumes_fixture(source: &str) {
    assert_eq!(source, "input relation R(x: u32);");
}

fn main() {}