//! CLI entry point for the `ddlint` tool.
//!
//! The current implementation does not perform any analysis. It writes a
//! placeholder message through a testable output boundary and exits non-zero.

use std::io::{self, Write};

/// Writes the placeholder message to the supplied writer.
///
/// Keeping the write behind a `Write` boundary lets a test observe a failed
/// standard-output write without spawning a process.
fn run(writer: &mut impl Write) -> io::Result<()> {
    writeln!(
        writer,
        "ddlint: CLI not yet implemented. Run with --help for planned options."
    )
}

/// Run the CLI entry point, reporting output failures to stderr and always
/// exiting with a non-zero status while the tool is unimplemented.
fn main() -> io::Result<()> {
    if let Err(error) = run(&mut io::stdout()) {
        // The placeholder has no other reporting channel, so send the failure
        // to stderr and keep the non-zero status callers already observe.
        writeln!(io::stderr(), "ddlint: {error}")?;
        std::process::exit(1);
    }
    std::process::exit(1);
}

#[cfg(test)]
mod tests {
    //! Tests for the CLI output boundary.

    use std::io::{self, Write};

    use super::run;

    /// A writer that rejects every write, standing in for a closed standard
    /// output.
    struct FailingWriter;

    impl Write for FailingWriter {
        fn write(&mut self, _bytes: &[u8]) -> io::Result<usize> {
            Err(io::Error::from(io::ErrorKind::BrokenPipe))
        }

        fn flush(&mut self) -> io::Result<()> { Err(io::Error::from(io::ErrorKind::BrokenPipe)) }
    }

    #[test]
    fn run_surfaces_a_write_failure() {
        let mut writer = FailingWriter;
        assert!(
            run(&mut writer).is_err(),
            "a failing writer should surface its error",
        );
    }
}
