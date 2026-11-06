//! Compile-fail case: a non-function item keeps its own diagnostics.
//!
//! The macro parses only functions, so an annotated const passes through
//! unchanged and the crate-level `unused_braces` denial still rejects its
//! redundant braces.

#![deny(unused_braces)]

use fixture_lints::allow_fixture_expansion_lints;

#[allow_fixture_expansion_lints]
const VALUE: u8 = { 1 };

fn main() {
    let _ = VALUE;
}