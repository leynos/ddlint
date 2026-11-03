//! Controlled tools and assertions for the Whitaker gate contract.
//!
//! The contract drives the real Makefile with generated `/bin/sh` executables
//! that print a marker and fail, so a recipe that swallows a tool's exit status
//! is observable. This module owns those executables, the markers they print
//! and the assertions that read the combined output. It is split from the
//! contract root to keep each module within the module size budget.

use std::{
    process::{Command, Output, Stdio},
    sync::atomic::{AtomicU64, Ordering},
};

use camino::{Utf8Path, Utf8PathBuf};
use cap_std::{
    ambient_authority,
    fs_utf8::{Dir, Permissions, PermissionsExt},
};

/// A literal marker a controlled tool prints to prove it really ran.
#[derive(Clone, Copy)]
pub(super) struct Marker(&'static str);

impl Marker {
    /// Returns whether `output` contains this marker.
    fn appears_in(&self, output: &str) -> bool { output.contains(self.0) }
}

/// A `make` invocation with its arguments, for running and for diagnostics.
pub(super) struct Invocation(Vec<String>);

impl Invocation {
    /// Builds an invocation from `arguments`.
    pub(super) fn new(arguments: impl IntoIterator<Item = impl Into<String>>) -> Self {
        Self(arguments.into_iter().map(Into::into).collect())
    }

    /// Runs `make` and pairs its output with the invocation label, so
    /// diagnostics can still quote the command.
    fn run(&self) -> Result<(String, Output), String> {
        let output = Command::new("make")
            .args(&self.0)
            .current_dir(env!("CARGO_MANIFEST_DIR"))
            .stdin(Stdio::null())
            .output()
            .map_err(|error| format!("running make {:?}: {error}", self.0))?;
        Ok((self.0.join(" "), output))
    }

    /// Returns the standard output of a successful invocation, or a diagnostic
    /// quoting the failure.
    pub(super) fn printed(&self) -> Result<String, String> {
        let (invocation, output) = self.run()?;
        if output.status.success() {
            Ok(String::from_utf8_lossy(&output.stdout).into_owned())
        } else {
            Err(format!(
                "`make {invocation}` failed: {}",
                String::from_utf8_lossy(&output.stderr)
            ))
        }
    }
}

/// The marker the controlled Whitaker prints when the recipe really runs it.
pub(super) const WHITAKER_MARKER: Marker = Marker("controlled-whitaker-ran");

/// The marker the controlled Cargo prints when the Clippy recipe really runs
/// it.
pub(super) const CLIPPY_MARKER: Marker = Marker("controlled-clippy-ran");

/// The marker the controlled spelling tool prints when its gate really runs
/// it.
pub(super) const SPELLING_MARKER: Marker = Marker("controlled-spelling-ran");

/// The marker a controlled Cargo prints when a subcommand other than `clippy`
/// runs, which proves the recipe continued past the failing Clippy gate.
pub(super) const CARGO_PAST_CLIPPY_MARKER: Marker = Marker("controlled-cargo-ran-past-clippy");

/// The markers that must never appear once the Clippy gate has failed.
pub(super) const CLIPPY_FORBIDDEN: [Marker; 3] =
    [CARGO_PAST_CLIPPY_MARKER, WHITAKER_MARKER, SPELLING_MARKER];

/// How a generated controlled tool responds to being run.
#[derive(Clone, Copy)]
pub(super) enum Behaviour {
    /// Print the marker and exit non-zero on every invocation.
    AlwaysFails,
    /// Print the marker and exit non-zero only for the `clippy` subcommand;
    /// print the pass marker and exit zero for every other subcommand.
    ///
    /// A tool that failed unconditionally would let a recipe that ignores
    /// `cargo clippy` and fails later on `cargo doc` satisfy the contract, so
    /// the pass marker proves the later subcommand never ran.
    FailsForClippyOnly(Marker),
}

/// The naming, evidence and behaviour for a generated controlled tool.
#[derive(Clone, Copy)]
pub(super) struct ToolSpec {
    /// File name the generated executable is written under.
    pub(super) name: &'static str,
    /// Marker the executable prints before failing.
    pub(super) marker: Marker,
    /// How the executable responds to each invocation.
    pub(super) behaviour: Behaviour,
}

/// Disambiguates temporary tool directories created by concurrent cases.
static NEXT_CONTROLLED_TOOL: AtomicU64 = AtomicU64::new(0);

/// Returns the `/bin/sh` script a tool described by `spec` runs.
///
/// The Clippy-only variant dispatches on the first argument, so `cargo doc`
/// and any later Cargo subcommand succeed and leave the pass marker behind.
fn script_for(spec: ToolSpec) -> String {
    let marker = spec.marker.0;
    match spec.behaviour {
        Behaviour::AlwaysFails => {
            format!(
                concat!("#!/bin/sh\n", "printf '%s\\n' '{}'\n", "exit 1\n",),
                marker
            )
        }
        Behaviour::FailsForClippyOnly(pass) => format!(
            concat!(
                "#!/bin/sh\n",
                "if [ \"${{1:-}}\" = clippy ]; then\n",
                "  printf '%s\\n' '{}'\n",
                "  exit 1\n",
                "fi\n",
                "printf '%s\\n' '{}'\n",
                "exit 0\n",
            ),
            marker, pass.0
        ),
    }
}

/// A generated failing tool that leaves evidence it was executed.
pub(super) struct ControlledTool {
    /// Parent of the owned temporary directory, used for cleanup.
    parent: Dir,
    /// Relative name of the owned temporary directory.
    directory: Utf8PathBuf,
    /// Absolute executable path passed to `make` for the tool's variable.
    executable: Utf8PathBuf,
}

impl ControlledTool {
    /// Writes an executable described by `spec` that prints its marker to
    /// standard output and exits non-zero.
    pub(super) fn new(spec: ToolSpec) -> Result<Self, String> {
        let temp_root = Utf8PathBuf::from_path_buf(std::env::temp_dir())
            .map_err(|path| format!("temporary directory is not UTF-8: {}", path.display()))?;
        let parent = Dir::open_ambient_dir(&temp_root, ambient_authority())
            .map_err(|error| format!("opening temporary directory {temp_root}: {error}"))?;
        let directory = Utf8PathBuf::from(format!(
            "ddlint-controlled-tool-{}-{}",
            std::process::id(),
            NEXT_CONTROLLED_TOOL.fetch_add(1, Ordering::Relaxed)
        ));
        parent
            .create_dir(&directory)
            .map_err(|error| format!("creating temporary directory {directory}: {error}"))?;
        let root = parent
            .open_dir(&directory)
            .map_err(|error| format!("opening temporary directory {directory}: {error}"))?;
        let name = spec.name;
        let script = script_for(spec);
        root.write(name, script.as_bytes())
            .map_err(|error| format!("writing the controlled {name}: {error}"))?;
        root.set_permissions(name, Permissions::from_mode(0o755))
            .map_err(|error| format!("making the controlled {name} executable: {error}"))?;
        let executable = temp_root.join(&directory).join(name);
        Ok(Self {
            parent,
            directory,
            executable,
        })
    }

    /// Returns the executable path to pass to `make` for this tool.
    pub(super) fn path(&self) -> &Utf8Path { &self.executable }
}

impl Drop for ControlledTool {
    /// Removes the temporary tool directory once the case finishes.
    fn drop(&mut self) {
        // Cleanup is best effort: `Drop` cannot report a failure, and a stale
        // directory must not mask the assertion that already ran.
        drop(self.parent.remove_dir_all(&self.directory));
    }
}

/// Asserts that an invocation fails after running the controlled Whitaker,
/// quoting what it printed instead.
pub(super) fn assert_fails(invocation: &Invocation, marker: Marker) -> Result<(), String> {
    let (label, output) = invocation.run()?;
    let combined = format!(
        "{}{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    if output.status.success() {
        return Err(format!("`make {label}` succeeded: {combined}"));
    }
    if marker.appears_in(&combined) {
        Ok(())
    } else {
        let reason = format!(
            "`make {label}` failed without the controlled Whitaker marker {:?}",
            marker.0
        );
        Err(format!("{reason}; output: {combined}"))
    }
}

/// The marker order an invocation must respect: `expected` must appear, and
/// no `forbidden` marker may appear after it.
#[derive(Clone, Copy)]
pub(super) struct Stopping {
    /// Marker the invocation must print before failing.
    pub(super) expected: Marker,
    /// Markers the invocation must never reach.
    pub(super) forbidden: &'static [Marker],
}

/// Asserts that an invocation fails after running the tool that prints
/// `expected`, and that it never reaches any tool printing a `forbidden`
/// marker.
pub(super) fn assert_stops_after(
    invocation: &Invocation,
    stopping: Stopping,
) -> Result<(), String> {
    let (label, output) = invocation.run()?;
    let combined = format!(
        "{}{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    if output.status.success() {
        return Err(format!("`make {label}` succeeded: {combined}"));
    }
    if !stopping.expected.appears_in(&combined) {
        let reason = format!(
            "`make {label}` failed without the expected marker {:?}",
            stopping.expected.0
        );
        return Err(format!("{reason}; output: {combined}"));
    }
    if let Some(marker) = stopping
        .forbidden
        .iter()
        .find(|marker| marker.appears_in(&combined))
    {
        let reason = format!(
            "`make {label}` reached {:?} after the expected marker {:?}",
            marker.0, stopping.expected.0
        );
        return Err(format!("{reason}; output: {combined}"));
    }
    Ok(())
}
