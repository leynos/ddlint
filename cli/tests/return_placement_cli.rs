//! Process-level tests for `return` placement through the `ddlint` binary.
//!
//! The binary parses every path it is given, so these tests assert the
//! user-visible rejection and both permitted positions end to end.

use std::{
    error::Error,
    path::{Path, PathBuf},
    process::{Command, Output},
};

use cap_std::{ambient_authority, fs::Dir};
use rstest::rstest;

/// The user-visible diagnostic for a `return` outside a function or closure.
const RETURN_IN_RULE_BODY_MESSAGE: &str =
    "'return' is not permitted in a rule body; use it only in a function or closure body";

/// Return `Err` with `message` unless `holds`.
///
/// A `Result`-returning test must not assert, so failures travel through the
/// test's `Result` and still name what was expected.
fn ensure(holds: bool, message: impl Into<String>) -> Result<(), String> {
    if holds { Ok(()) } else { Err(message.into()) }
}

/// Write `source` to `name` inside `directory` and return the file path.
fn write_source(
    directory: &tempfile::TempDir,
    name: &str,
    source: &str,
) -> std::io::Result<PathBuf> {
    Dir::open_ambient_dir(directory.path(), ambient_authority())?.write(name, source)?;
    Ok(directory.path().join(name))
}

/// Run the binary over `paths` with a cleared environment.
fn run_binary(directory: &tempfile::TempDir, paths: &[&Path]) -> std::io::Result<Output> {
    Command::new(env!("CARGO_BIN_EXE_ddlint"))
        .current_dir(directory.path())
        .env_clear()
        .args(paths)
        .output()
}

/// A function body and a closure body both parse without diagnostics.
#[rstest]
#[case("function.dl", "function f(): u32 { return 1; }\n")]
#[case("closure.dl", "Output(head_x) :- Source(|ignored| return head_x).\n")]
fn permitted_return_positions_exit_zero(
    #[case] name: &str,
    #[case] source: &str,
) -> Result<(), Box<dyn Error>> {
    let directory = tempfile::TempDir::new()?;
    let path = write_source(&directory, name, source)?;
    let output = run_binary(&directory, &[&path])?;
    let stderr = String::from_utf8_lossy(&output.stderr);

    ensure(
        output.status.code() == Some(0),
        format!("a permitted `return` must exit zero, stderr: {stderr}"),
    )?;
    ensure(
        output.stderr.is_empty(),
        format!("a clean parse must not write to standard error, got: {stderr}"),
    )?;
    Ok(())
}

/// A bare rule-body `return` is reported with its path, span, and message.
#[test]
fn a_bare_rule_body_return_is_reported() -> Result<(), Box<dyn Error>> {
    let directory = tempfile::TempDir::new()?;
    let path = write_source(&directory, "bare.dl", "R(x) :- return x.")?;
    let output = run_binary(&directory, &[&path])?;
    let stderr = String::from_utf8_lossy(&output.stderr);

    ensure(
        output.status.code() == Some(1),
        format!("a misplaced `return` must exit non-zero, stderr: {stderr}"),
    )?;
    ensure(
        stderr.contains(&format!("{}:8..14: error: ", path.display())),
        format!("the diagnostic must name the path and keyword span, got: {stderr}"),
    )?;
    ensure(
        stderr.contains(RETURN_IN_RULE_BODY_MESSAGE),
        format!("the diagnostic must explain the rule-body restriction, got: {stderr}"),
    )?;
    Ok(())
}

/// An unreadable path is reported with the tool's own error prefix.
#[test]
fn a_missing_file_is_reported() -> Result<(), Box<dyn Error>> {
    let directory = tempfile::TempDir::new()?;
    let missing = directory.path().join("missing.dl");
    let output = run_binary(&directory, &[&missing])?;
    let stderr = String::from_utf8_lossy(&output.stderr);

    ensure(
        output.status.code() == Some(1),
        format!("an unreadable file must exit non-zero, stderr: {stderr}"),
    )?;
    ensure(
        stderr.starts_with(&format!("ddlint: {}: ", missing.display())),
        format!("a read failure must start with the tool prefix and path, got: {stderr}"),
    )?;
    Ok(())
}
