//! Contract for the workspace rustdoc build: `make lint` must build the
//! workspace documentation with warnings denied under `--cfg docsrs`.
//!
//! Rustdoc reads `RUSTDOCFLAGS`, and the lint recipe assigns the policy
//! separately from `RUSTFLAGS`. This module judges the repository's own recipe,
//! and the checker that judges it, so canned cases prove that each missing
//! piece is reported.

use rstest::rstest;

use super::{
    Fixture,
    make::{Host, MakeRunner, Target},
    process::real_make,
};

/// The pieces the `RUSTDOCFLAGS` assignment must carry: the docs.rs
/// configuration and the warning denial.
const REQUIRED_ASSIGNMENT_PIECES: &[&str] = &["--cfg docsrs", "-D warnings"];

/// The Cargo invocation the recipe must run.
const REQUIRED_COMMAND: &str = "cargo doc --workspace --no-deps";

/// The command `make -n lint` prints for its documentation build, kept as a
/// fixture so the checker is judged against the shape the repository uses.
const REAL_SHAPE: &str = concat!(
    "RUSTFLAGS=\"${RUSTFLAGS:+$RUSTFLAGS }-Zthreads=8\" ",
    "RUSTDOCFLAGS=\"--cfg docsrs -D warnings\" ",
    "cargo doc --workspace --no-deps",
);

/// Returns the value assigned to `RUSTDOCFLAGS`, when the command assigns one.
fn rustdocflags_value(command: &str) -> Option<&str> {
    let (_, rest) = command.split_once("RUSTDOCFLAGS=\"")?;
    rest.split_once('"').map(|(value, _)| value)
}

/// Returns the complaint about a rustdoc command that does not build the
/// workspace documentation under the warning policy, naming each omission.
///
/// ```text
/// rustdoc_policy_problem(REAL_SHAPE) -> None
/// rustdoc_policy_problem("cargo doc --workspace") -> Some("missing ...")
/// ```
pub fn rustdoc_policy_problem(command: &str) -> Option<String> {
    let mut missing: Vec<String> = Vec::new();
    match rustdocflags_value(command) {
        None => missing.push("a RUSTDOCFLAGS assignment".to_owned()),
        Some(value) => missing.extend(
            REQUIRED_ASSIGNMENT_PIECES
                .iter()
                .copied()
                .filter(|piece| !value.contains(piece))
                .map(|piece| format!("`{piece}` in RUSTDOCFLAGS")),
        ),
    }
    if !command.contains(REQUIRED_COMMAND) {
        missing.push(format!("`{REQUIRED_COMMAND}`"));
    }
    let missing_list = missing.join(", ");
    (!missing_list.is_empty()).then(|| {
        format!(
            concat!(
                "the rustdoc command drops the documentation warning policy; ",
                "missing {}"
            ),
            missing_list,
        )
    })
}

/// Returns the complaint about the repository's own `make -n lint` rustdoc
/// command, or an error when the output carries no such command.
///
/// # Errors
///
/// Returns the reason when `make -n lint` cannot run or prints no `cargo doc`
/// command, so a missing recipe fails rather than passing over nothing.
pub fn live_rustdoc_problem(runner: MakeRunner) -> Result<Option<String>, String> {
    let stdout = runner(Target("lint"), Host::Linux)?;
    let command = stdout
        .lines()
        .find(|line| line.contains("doc --workspace"))
        .ok_or_else(|| "`make -n lint` prints no cargo doc command".to_owned())?;
    Ok(rustdoc_policy_problem(command))
}

/// The documentation build must deny warnings and match the docs.rs
/// configuration, or a broken intra-doc link scrolls past the gate.
#[test]
fn the_lint_recipe_builds_the_workspace_documentation() -> Result<(), String> {
    live_rustdoc_problem(real_make)?.map_or(Ok(()), Err)
}

/// Scenario: canned rustdoc commands, each missing one piece of the policy.
///
/// Invariant: the real shape passes, and dropping the whole assignment or any
/// single piece is reported, so the checker itself is tested rather than only
/// the Makefile.
#[rstest]
#[case::the_real_shape(Fixture(REAL_SHAPE), false)]
#[case::no_assignment(Fixture("cargo doc --workspace --no-deps"), true)]
#[case::no_docsrs_configuration(
    Fixture(concat!(
        "RUSTDOCFLAGS=\"-D warnings\" ",
        "cargo doc --workspace --no-deps",
    )),
    true
)]
#[case::no_warning_denial(
    Fixture(concat!(
        "RUSTDOCFLAGS=\"--cfg docsrs\" ",
        "cargo doc --workspace --no-deps",
    )),
    true
)]
#[case::no_no_deps(
    Fixture("RUSTDOCFLAGS=\"--cfg docsrs -D warnings\" cargo doc --workspace"),
    true
)]
fn the_checker_reports_each_missing_piece(#[case] command: Fixture, #[case] has_problem: bool) {
    assert_eq!(
        rustdoc_policy_problem(command.0).is_some(),
        has_problem,
        "command: {}",
        command.0
    );
}
