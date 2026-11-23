//! Process-level tests for `return` placement through the `ddlint` binary.
//!
//! The binary parses every path it is given, so these tests assert the
//! user-visible rejection and both permitted positions end to end.

use std::{
    fs,
    path::{Path, PathBuf},
    process::{Command, Output},
};

use rstest::rstest;

/// The user-visible diagnostic for a `return` outside a function or closure.
const RETURN_IN_RULE_BODY_MESSAGE: &str =
    "'return' is not permitted in a rule body; use it only in a function or closure body";

/// Write `source` to `name` inside `directory` and return the file path.
fn write_source(directory: &tempfile::TempDir, name: &str, source: &str) -> PathBuf {
    let path = directory.path().join(name);
    fs::write(&path, source).expect("the fixture source should be written");
    path
}

/// Run the binary over `paths` with a cleared environment.
fn run_binary(directory: &tempfile::TempDir, paths: &[&Path]) -> Output {
    Command::new(env!("CARGO_BIN_EXE_ddlint"))
        .current_dir(directory.path())
        .env_clear()
        .args(paths)
        .output()
        .expect("the ddlint binary should run")
}

/// A function body and a closure body both parse without diagnostics.
#[rstest]
#[case("function.dl", "function f(): u32 { return 1; }\n")]
#[case("closure.dl", "Output(head_x) :- Source(|ignored| return head_x).\n")]
fn permitted_return_positions_exit_zero(#[case] name: &str, #[case] source: &str) {
    let directory = tempfile::TempDir::new().expect("a temporary directory should be created");
    let path = write_source(&directory, name, source);
    let output = run_binary(&directory, &[&path]);
    let stderr = String::from_utf8_lossy(&output.stderr);

    assert_eq!(
        output.status.code(),
        Some(0),
        "a permitted `return` must exit zero, stderr: {stderr}",
    );
    assert!(
        output.stderr.is_empty(),
        "a clean parse must not write to standard error, got: {stderr}",
    );
}

/// A bare rule-body `return` is reported with its path, span, and message.
#[test]
fn a_bare_rule_body_return_is_reported() {
    let directory = tempfile::TempDir::new().expect("a temporary directory should be created");
    let path = write_source(&directory, "bare.dl", "R(x) :- return x.");
    let output = run_binary(&directory, &[&path]);
    let stderr = String::from_utf8_lossy(&output.stderr);

    assert_eq!(
        output.status.code(),
        Some(1),
        "a misplaced `return` must exit non-zero, stderr: {stderr}",
    );
    assert!(
        stderr.contains(&format!("{}:8..14: error: ", path.display())),
        "the diagnostic must name the path and keyword span, got: {stderr}",
    );
    assert!(
        stderr.contains(RETURN_IN_RULE_BODY_MESSAGE),
        "the diagnostic must explain the rule-body restriction, got: {stderr}",
    );
}

/// An unreadable path is reported with the tool's own error prefix.
#[test]
fn a_missing_file_is_reported() {
    let directory = tempfile::TempDir::new().expect("a temporary directory should be created");
    let missing = directory.path().join("missing.dl");
    let output = run_binary(&directory, &[&missing]);
    let stderr = String::from_utf8_lossy(&output.stderr);

    assert_eq!(
        output.status.code(),
        Some(1),
        "an unreadable file must exit non-zero, stderr: {stderr}",
    );
    assert!(
        stderr.starts_with(&format!("ddlint: {}: ", missing.display())),
        "a read failure must start with the tool prefix and path, got: {stderr}",
    );
}
