//! Smoke binding for the `rstest-bdd` dependency.
//!
//! Compiles and runs one scenario so the dependency wiring is proven before
//! the full primary-key feature is added.

use std::cell::RefCell;

use ddlint::parse;
use fixture_lints::allow_fixture_expansion_lints;
use rstest::fixture;
use rstest_bdd::StepResult;
use rstest_bdd_macros::{given, scenario, then, when};

/// State shared between the smoke scenario's steps.
#[derive(Default)]
struct SmokeWorld {
    /// Source written by the `Given` step.
    source: RefCell<String>,
    /// Binder count recorded by the `When` step.
    binder_count: RefCell<Option<usize>>,
}

#[allow_fixture_expansion_lints]
#[fixture]
fn world() -> SmokeWorld { SmokeWorld::default() }

#[given("the source \"{source}\"")]
fn given_source(world: &SmokeWorld, source: String) { *world.source.borrow_mut() = source; }

#[when("its first primary key is inspected")]
fn inspect_primary_key(world: &SmokeWorld) -> StepResult<(), String> {
    let parsed = parse(&world.source.borrow());
    let count = parsed
        .root()
        .relations()
        .into_iter()
        .next()
        .and_then(|relation| relation.primary_key().ok().flatten())
        .map_or(0, |key| key.binders().len());
    *world.binder_count.borrow_mut() = Some(count);
    Ok(())
}

#[then("the binder count is {expected}")]
fn binder_count(world: &SmokeWorld, expected: usize) -> StepResult<(), String> {
    let actual = (*world.binder_count.borrow())
        .ok_or_else(|| "the primary key has not been inspected".to_owned())?;
    if actual != expected {
        return Err(format!("expected {expected} binders, got {actual}"));
    }
    Ok(())
}

#[scenario(
    path = "tests/features/relation_primary_key_smoke.feature",
    name = "A single binder is exposed"
)]
fn single_binder_is_exposed(world: SmokeWorld) {
    let count = world.binder_count.into_inner();
    assert_eq!(count, Some(1), "the scenario must record one binder");
}
