//! Process-level test that a failed standard-output write is reported on
//! standard error.

#![cfg(unix)]

use std::{
    os::{fd::OwnedFd, unix::net::UnixStream},
    process::{Command, Stdio},
};

/// Writing to a socket with no peer fails deterministically, so the placeholder
/// reports the failure on standard error and still exits non-zero.
#[test]
fn a_failed_stdout_write_is_reported_on_stderr() -> Result<(), String> {
    let (writer, reader) = UnixStream::pair().expect("a Unix socket pair should be created");
    drop(reader);

    let directory = tempfile::TempDir::new().expect("a temporary directory should be created");
    let output = Command::new(env!("CARGO_BIN_EXE_ddlint"))
        .current_dir(directory.path())
        .env_clear()
        .stdin(Stdio::null())
        .stdout(Stdio::from(OwnedFd::from(writer)))
        .stderr(Stdio::piped())
        .output()
        .expect("the placeholder binary should run");

    if output.status.success() {
        return Err("the placeholder should exit non-zero".to_owned());
    }
    if !output.stdout.is_empty() {
        return Err(format!(
            "the placeholder wrote to standard output: {}",
            String::from_utf8_lossy(&output.stdout)
        ));
    }
    let stderr = String::from_utf8_lossy(&output.stderr);
    if !stderr.starts_with("ddlint: ") {
        return Err(format!("unexpected standard error: {stderr}"));
    }
    Ok(())
}
