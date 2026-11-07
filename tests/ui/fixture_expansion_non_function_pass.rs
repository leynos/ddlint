//! Pass case: the attribute is transparent for a non-function item.
//!
//! `#[allow_fixture_expansion_lints]` qualifies only single-expression
//! functions, so an annotated const must pass through unchanged.

#![deny(unused_braces)]

use fixture_lints::allow_fixture_expansion_lints;

#[allow_fixture_expansion_lints]
const MARKER: u8 = 1;

fn main() {
    let _ = MARKER;
}