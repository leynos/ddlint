//! CLI entry point for the `ddlint` tool.
//!
//! The current implementation parses each named file and reports parse
//! diagnostics. Linting, configuration loading, and directory walking are not
//! implemented yet; with no arguments the placeholder message keeps its
//! previous output and exit status.

use std::{
    ffi::{OsStr, OsString},
    io::{self, Read as _, Write},
    path::Path,
};

use cap_std::{ambient_authority, fs::File};
use ddlint::{parse_error_message, parse_error_span};

/// Message written when the command is invoked without file arguments.
const PLACEHOLDER_MESSAGE: &str =
    "ddlint: CLI not yet implemented. Run with --help for planned options.";

/// Renderer for a parse-only run.
///
/// Reporting goes through writer handles so a closed stream is observable as
/// an `io::Error` rather than a panic, and so a test can drive the same code
/// without spawning a process.
struct Reporter<'a, O, E> {
    stdout: &'a mut O,
    stderr: &'a mut E,
}

impl<O, E> Reporter<'_, O, E>
where
    O: Write,
    E: Write,
{
    /// Write the no-argument placeholder message.
    fn placeholder(&mut self) -> io::Result<()> { writeln!(self.stdout, "{PLACEHOLDER_MESSAGE}") }

    /// Report a file that could not be read.
    fn read_failure(&mut self, path: &OsStr, error: &io::Error) -> io::Result<()> {
        writeln!(self.stderr, "ddlint: {}: {error}", path.to_string_lossy())
    }

    /// Report one final parse diagnostic for `path`, naming its byte span.
    ///
    /// `span` is the half-open byte range the diagnostic covers, taken from
    /// the parser error by the caller so this renderer stays independent of
    /// the parser's error type.
    fn parse_diagnostic(
        &mut self,
        path: &OsStr,
        span: &std::ops::Range<usize>,
        message: &str,
    ) -> io::Result<()> {
        writeln!(
            self.stderr,
            "{}:{}..{}: error: {message}",
            path.to_string_lossy(),
            span.start,
            span.end,
        )
    }
}

/// Exit status of a completed run.
#[derive(Debug, Clone, Copy, Eq, PartialEq)]
enum Status {
    /// Every named file parsed without diagnostics.
    Clean,
    /// At least one file failed to read or parse, or the tool is a placeholder.
    Failed,
}

impl Status {
    /// Process exit code for this status.
    const fn code(self) -> i32 {
        match self {
            Self::Clean => 0,
            Self::Failed => 1,
        }
    }
}

/// Parse every named file, reporting failures, and return the run status.
///
/// With no arguments the placeholder message keeps the original behaviour: it
/// is written to standard output and the run fails.
fn run(args: &[OsString], stdout: &mut impl Write, stderr: &mut impl Write) -> io::Result<Status> {
    let mut reporter = Reporter { stdout, stderr };
    if args.is_empty() {
        reporter.placeholder()?;
        return Ok(Status::Failed);
    }

    let mut is_clean = true;
    for path in args {
        is_clean &= check_file(path, &mut reporter)?;
    }
    Ok(if is_clean {
        Status::Clean
    } else {
        Status::Failed
    })
}

/// Read `path` as UTF-8 text through an ambient file capability.
///
/// The operator supplies ambient paths on the command line, so the only
/// ambient operation is opening exactly the named file; every later read goes
/// through the resulting capability. Opening the file itself rather than a
/// directory lets an operator-supplied symlink resolve to a target outside
/// the link's directory, which a directory capability would refuse.
fn read_source(path: &Path) -> io::Result<String> {
    let mut source = String::new();
    File::open_ambient(path, ambient_authority())?.read_to_string(&mut source)?;
    Ok(source)
}

/// Parse one file, reporting read failures and parse diagnostics.
///
/// Returns `true` when the file was read and produced no diagnostics, so the
/// caller can keep scanning the remaining paths instead of stopping at the
/// first bad file.
fn check_file<O, E>(path: &OsStr, reporter: &mut Reporter<'_, O, E>) -> io::Result<bool>
where
    O: Write,
    E: Write,
{
    let source = match read_source(Path::new(path)) {
        Ok(source) => source,
        Err(error) => {
            reporter.read_failure(path, &error)?;
            return Ok(false);
        }
    };

    let parsed = ddlint::parse(&source);
    for error in parsed.errors() {
        let message = parse_error_message(error);
        reporter.parse_diagnostic(path, &parse_error_span(error), &message)?;
    }
    Ok(parsed.errors().is_empty())
}

/// Run the CLI entry point, reporting writer failures to standard error.
///
/// A failed diagnostic write is not a diagnostic itself, so it goes through
/// the same `ddlint: <error>` channel as an unreadable file and exits
/// non-zero without pretending the remaining files were checked.
fn main() -> io::Result<()> {
    let args: Vec<OsString> = std::env::args_os().skip(1).collect();
    match run(&args, &mut io::stdout(), &mut io::stderr()) {
        Ok(status) => std::process::exit(status.code()),
        Err(error) => {
            writeln!(io::stderr(), "ddlint: {error}")?;
            std::process::exit(1);
        }
    }
}

#[cfg(test)]
mod tests {
    //! Tests for the CLI output boundaries.

    use std::{
        ffi::OsString,
        io::{self, Write},
    };

    use cap_std::{ambient_authority, fs::Dir};

    use super::{Status, run};

    /// A writer that rejects every write, standing in for a closed stream.
    struct FailingWriter;

    impl Write for FailingWriter {
        fn write(&mut self, _bytes: &[u8]) -> io::Result<usize> {
            Err(io::Error::from(io::ErrorKind::BrokenPipe))
        }

        fn flush(&mut self) -> io::Result<()> { Err(io::Error::from(io::ErrorKind::BrokenPipe)) }
    }

    /// Collects everything written to it.
    #[derive(Default)]
    struct Captured(String);

    impl Write for Captured {
        fn write(&mut self, bytes: &[u8]) -> io::Result<usize> {
            self.0.push_str(&String::from_utf8_lossy(bytes));
            Ok(bytes.len())
        }

        fn flush(&mut self) -> io::Result<()> { Ok(()) }
    }

    #[test]
    fn run_surfaces_a_placeholder_write_failure() {
        let mut stdout = FailingWriter;
        let mut stderr = Captured::default();
        assert!(
            run(&[], &mut stdout, &mut stderr).is_err(),
            "a failing writer should surface its error",
        );
    }

    #[test]
    fn run_surfaces_a_diagnostic_write_failure() {
        let directory = tempfile::TempDir::new().expect("a temporary directory should be created");
        let fixture = Dir::open_ambient_dir(directory.path(), ambient_authority())
            .expect("the fixture directory should open");
        fixture
            .write("bad.dl", "R(x) :- return x.")
            .expect("the fixture should be written");
        let path = directory.path().join("bad.dl");

        let mut stdout = Captured::default();
        let mut stderr = FailingWriter;
        assert!(
            run(&[OsString::from(path)], &mut stdout, &mut stderr).is_err(),
            "a failing diagnostic writer should surface its error",
        );
    }

    #[test]
    fn the_placeholder_keeps_its_message_and_failed_status() {
        let mut stdout = Captured::default();
        let mut stderr = Captured::default();
        let status = run(&[], &mut stdout, &mut stderr).expect("writing to memory cannot fail");

        assert_eq!(
            status,
            Status::Failed,
            "the placeholder must keep its non-zero status",
        );
        assert_eq!(
            stdout.0, "ddlint: CLI not yet implemented. Run with --help for planned options.\n",
            "the placeholder message must not change",
        );
        assert!(
            stderr.0.is_empty(),
            "the placeholder must not write to stderr, got {:?}",
            stderr.0,
        );
    }

    #[test]
    fn a_missing_file_is_reported_with_the_path_prefix() {
        let mut stdout = Captured::default();
        let mut stderr = Captured::default();
        let missing = OsString::from("/nonexistent/ddlint-fixture.dl");
        let status =
            run(&[missing], &mut stdout, &mut stderr).expect("writing to memory cannot fail");

        assert_eq!(
            status,
            Status::Failed,
            "an unreadable file must fail the run",
        );
        assert!(
            stderr
                .0
                .starts_with("ddlint: /nonexistent/ddlint-fixture.dl: "),
            "a read failure must name the path, got {:?}",
            stderr.0,
        );
    }
}
