//! CLI entry point for the `ddlint` tool.
//!
//! The current implementation does not perform any analysis. It simply
//! serves as a placeholder executable that will eventually invoke the
//! parser and linter logic.

use std::io::{self, Write};

fn main() -> io::Result<()> {
    // Provide a minimal UX until the full CLI is implemented.
    writeln!(
        io::stdout(),
        "ddlint: CLI not yet implemented. Run with --help for planned options."
    )?;
    std::process::exit(1);
}
