//! Contract for the doctest warning policy: `make test` must forward the
//! denial to rustdoc.
//!
//! Rustdoc compiles each doctest snippet as its own crate and reads
//! `RUSTDOCFLAGS`, not `RUSTFLAGS`, so the `test` recipe has to assign the
//! policy itself. This module judges the repository's own recipe, and the
//! checker that judges it, so canned cases prove each missing piece is
//! reported.

use rstest::rstest;

use super::{
    Fixture,
    make::{Host, MakeRunner, Target},
    process::real_make,
};

/// The pieces the doctest command must carry: the `RUSTDOCFLAGS` assignment
/// itself, the caller's value inherited ahead of the policy, and the two
/// arguments that build the snippets with warnings denied.
const REQUIRED_PIECES: &[&str] = &[
    "RUSTDOCFLAGS=\"",
    "${RUSTDOCFLAGS:+$RUSTDOCFLAGS }",
    "-Zunstable-options",
    "--doctest-build-arg=-Dwarnings",
];

/// The command `make -n test` prints for its doctest recipe, kept as a fixture
/// so the checker is judged against the shape the repository really uses.
const REAL_SHAPE: &str = concat!(
    "RUSTFLAGS=\"${RUSTFLAGS:+$RUSTFLAGS }-D warnings -Zthreads=8\" ",
    "RUSTDOCFLAGS=\"${RUSTDOCFLAGS:+$RUSTDOCFLAGS }",
    "-Zunstable-options --doctest-build-arg=-Dwarnings\" ",
    "cargo test --workspace --doc --all-features",
);

/// Returns the complaint about a doctest command that does not forward the
/// warning policy, naming the pieces it lacks.
///
/// ```text
/// doctest_policy_problem(REAL_SHAPE) -> None
/// doctest_policy_problem("cargo test --doc") -> Some("missing ...")
/// ```
pub fn doctest_policy_problem(command: &str) -> Option<String> {
    let missing: Vec<&str> = REQUIRED_PIECES
        .iter()
        .copied()
        .filter(|piece| !command.contains(piece))
        .collect();
    let missing_list = missing.join(", ");
    (!missing_list.is_empty()).then(|| {
        format!("the --doc command drops the doctest warning policy; missing {missing_list}")
    })
}

/// Returns the complaint about the repository's own `make -n test` doctest
/// command, or an error when the output carries no such command.
///
/// # Errors
///
/// Returns the reason when `make -n test` cannot run or prints no `--doc`
/// command, so a missing recipe fails rather than passing over nothing.
pub fn live_doctest_problem(runner: MakeRunner) -> Result<Option<String>, String> {
    let stdout = runner(Target("test"), Host::Linux)?;
    let command = stdout
        .lines()
        .find(|line| line.contains("--doc"))
        .ok_or_else(|| "`make -n test` prints no --doc command".to_owned())?;
    Ok(doctest_policy_problem(command))
}

/// Every doctest snippet compiles separately under `RUSTDOCFLAGS`, so the real
/// recipe must carry the warning denial itself.
#[test]
fn the_doctest_recipe_forwards_the_warning_policy() -> Result<(), String> {
    live_doctest_problem(real_make)?.map_or(Ok(()), Err)
}

/// Scenario: canned doctest commands, each missing one piece of the policy.
///
/// Invariant: the real shape passes, and dropping the whole assignment or any
/// single piece is reported, so the checker itself is tested rather than only
/// the Makefile.
#[rstest]
#[case::the_real_shape(Fixture(REAL_SHAPE), false)]
#[case::no_assignment(Fixture("cargo test --workspace --doc --all-features"), true)]
#[case::no_unstable_options(
    Fixture(concat!(
        "RUSTDOCFLAGS=\"${RUSTDOCFLAGS:+$RUSTDOCFLAGS }",
        "--doctest-build-arg=-Dwarnings\" ",
        "cargo test --workspace --doc --all-features",
    )),
    true
)]
#[case::no_doctest_build_arg(
    Fixture(concat!(
        "RUSTDOCFLAGS=\"${RUSTDOCFLAGS:+$RUSTDOCFLAGS }",
        "-Zunstable-options\" ",
        "cargo test --workspace --doc --all-features",
    )),
    true
)]
#[case::no_inherited_flags(
    Fixture(concat!(
        "RUSTDOCFLAGS=\"-Zunstable-options ",
        "--doctest-build-arg=-Dwarnings\" ",
        "cargo test --workspace --doc --all-features",
    )),
    true
)]
fn the_checker_reports_each_missing_piece(#[case] command: Fixture, #[case] has_problem: bool) {
    assert_eq!(
        doctest_policy_problem(command.0).is_some(),
        has_problem,
        "command: {}",
        command.0
    );
}
