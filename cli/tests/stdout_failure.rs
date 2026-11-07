//! Process-level test that a failed standard-output write is reported on
//! standard error.

#![cfg(unix)]

use std::{
    io::Write,
    os::{fd::OwnedFd, unix::net::UnixStream},
    process::{Command, Stdio},
};

/// Returns the error produced by writing to a socket whose peer has closed.
///
/// Reproducing the failure here derives the expected diagnostic from the
/// platform instead of hard-coding an error message, while keeping the
/// assertion independent of the process under test. This is a helper rather
/// than a test, so probe setup failures are returned for the caller to
/// propagate with `?` instead of panicking.
fn closed_peer_write_error() -> Result<String, String> {
    let (mut writer, reader) = UnixStream::pair()
        .map_err(|error| format!("a probe Unix socket pair should be created: {error}"))?;
    drop(reader);
    match writer.write_all(b"probe") {
        Ok(()) => Err("writing to a socket with no peer should fail".to_owned()),
        Err(error) => Ok(error.to_string()),
    }
}

/// Writing to a socket with no peer fails deterministically, so the placeholder
/// reports the failure on standard error and still exits non-zero.
#[test]
fn a_failed_stdout_write_is_reported_on_stderr() -> Result<(), String> {
    let expected_error = closed_peer_write_error()?;
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
    let stderr = String::from_utf8_lossy(&output.stderr);
    if !stderr.starts_with("ddlint: ") {
        return Err(format!("unexpected standard error: {stderr}"));
    }
    // Standard output is redirected to the socket, so the captured pipe stays
    // empty and proves nothing. The diagnostic itself is the evidence that the
    // write was attempted and its failure surfaced.
    if !stderr.contains(&expected_error) {
        return Err(format!(
            "standard error did not report the failed write ({expected_error}): {stderr}"
        ));
    }
    Ok(())
}
