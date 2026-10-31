//! Process-level compatibility test for the existing placeholder command.

use std::process::Command;

use rstest::rstest;

/// With no configuration, the placeholder retains its exact output and status.
#[rstest]
fn placeholder_output_and_exit_status_are_unchanged() -> Result<(), Box<dyn std::error::Error>> {
    let directory = tempfile::TempDir::new()?;
    let output = Command::new(env!("CARGO_BIN_EXE_ddlint"))
        .current_dir(directory.path())
        .env_clear()
        .output()?;

    assert_eq!(output.status.code(), Some(1));
    assert_eq!(
        output.stdout,
        b"ddlint: CLI not yet implemented. Run with --help for planned options.\n"
    );
    assert!(
        output.stderr.is_empty(),
        "the placeholder wrote to stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    Ok(())
}
