//! Capability-rooted fake tools for the build provisioning contracts.
//!
//! This fixture module is private to `provisioning.rs`. Child processes use
//! offline fake installers and never receive a redirected Cargo cache.

use camino::{Utf8Path, Utf8PathBuf};
use cap_std::{
    ambient_authority,
    fs_utf8::{Dir, Permissions, PermissionsExt},
};
use std::{
    io,
    process::{Command, Output},
    sync::atomic::{AtomicU64, Ordering},
};

/// Disambiguates scratch directories created by concurrent contract cases.
static NEXT_SCRATCH_DIRECTORY: AtomicU64 = AtomicU64::new(0);

/// Logs and fulfils downloads with inert bytes kept inside the fixture root.
const CURL_SCRIPT: &str = r#"#!/bin/sh
printf 'curl:%s\n' "$*" >> "$TRACE_FILE"
destination=
while [ "$#" -gt 0 ]; do
  case "$1" in
    --output|-o)
      shift
      destination=${1-}
      ;;
    --output=*) destination=${1#--output=} ;;
    -o*) destination=${1#-o} ;;
  esac
  shift
done
if [ -n "$destination" ] && [ "$destination" != "-" ]; then
  case "$destination" in
    "$SANDBOX_ROOT"/*) printf 'offline fixture archive\n' > "$destination" ;;
    *) printf 'refusing download outside the test sandbox\n' >&2; exit 73 ;;
  esac
else
  printf 'offline fixture archive\n'
fi
"#;

/// Rejects fallback downloads so installer cases remain offline.
const WGET_SCRIPT: &str = r#"#!/bin/sh
printf 'wget-refused:%s\n' "$*" >> "$TRACE_FILE"
printf 'only the offline curl fixture is permitted\n' >&2
exit 73
"#;

/// Records checksum arguments/input and can inject a verification failure.
const CHECKSUM_SCRIPT: &str = r#"#!/bin/sh
printf 'sha256sum:%s\n' "$*" >> "$TRACE_FILE"
printf 'checksum-entry:%s\n' "$(/usr/bin/cat)" >> "$TRACE_FILE"
if [ "${FAKE_CHECKSUM_FAIL:-false}" = true ]; then
  exit 1
fi
exit 0
"#;

/// Extracts a minimal mold tree only beneath the private fixture root.
const TAR_SCRIPT: &str = r#"#!/bin/sh
printf 'tar:%s\n' "$*" >> "$TRACE_FILE"
destination=
while [ "$#" -gt 0 ]; do
  case "$1" in
    -C|--directory)
      shift
      destination=${1-}
      ;;
    -C*) destination=${1#-C} ;;
    --directory=*) destination=${1#--directory=} ;;
  esac
  shift
done
if [ -z "$destination" ]; then
  destination=$(pwd)
fi
case "$destination" in
  "$SANDBOX_ROOT"|"$SANDBOX_ROOT"/*) ;;
  *) printf 'refusing extraction outside the test sandbox\n' >&2; exit 73 ;;
esac
archive_root="$destination/mold-2.41.0-$FAKE_TARGET_ARCH-linux"
for binary_directory in "$archive_root/bin" "$destination/bin"; do
  mkdir -p "$binary_directory"
  printf '#!/bin/sh\nprintf "mold 2.41.0\\n"\n' > "$binary_directory/mold"
  chmod 755 "$binary_directory/mold"
done
exit 0
"#;

/// Models pinned toolchain and component discovery without installing them.
const RUSTUP_SCRIPT: &str = r#"#!/bin/sh
printf 'rustup:%s\n' "$*" >> "$TRACE_FILE"
case "$*" in
  *"toolchain install"*)
    case "$*" in
      *nightly-2025-09-16*) : > "$RUSTUP_TOOLCHAIN_MARKER" ;;
      *) printf 'refusing unpinned toolchain install\n' >&2; exit 74 ;;
    esac
    case "$*" in
      *rust-analyzer*) : > "$RUST_ANALYZER_MARKER" ;;
    esac
    ;;
  *nightly-2025-09-16*)
    if [ "${FAKE_TOOLCHAIN_PRESENT:-true}" != true ] && [ ! -f "$RUSTUP_TOOLCHAIN_MARKER" ]; then
      printf 'toolchain nightly-2025-09-16 is missing\n' >&2
      exit 1
    fi
    ;;
esac
case "$*" in
  *"toolchain list"*)
    if [ "${FAKE_TOOLCHAIN_PRESENT:-true}" = true ] || [ -f "$RUSTUP_TOOLCHAIN_MARKER" ]; then
      printf 'nightly-2025-09-16-%s-unknown-linux-gnu\n' "$FAKE_TARGET_ARCH"
    fi
    ;;
  *"component list"*)
    if [ -f "$RUST_ANALYZER_MARKER" ] || [ "${FAKE_COMPONENT_PRESENT:-true}" = true ]; then
      printf 'rust-analyzer-%s-unknown-linux-gnu\n' "$FAKE_TARGET_ARCH"
    fi
    ;;
  *"component add"*rust-analyzer*)
    : > "$RUST_ANALYZER_MARKER"
    ;;
esac
exit 0
"#;

/// Reports the pinned compiler and rejects an absent fake toolchain.
const RUSTC_SCRIPT: &str = r#"#!/bin/sh
if [ "${FAKE_TOOLCHAIN_PRESENT:-true}" != true ] && [ ! -f "$RUSTUP_TOOLCHAIN_MARKER" ]; then
  printf 'toolchain nightly-2025-09-16 is missing\n' >&2
  exit 1
fi
printf 'rustc 1.90.0-nightly (nightly-2025-09-16)\n'
"#;

/// Delegates read-only text utilities to their host implementations.
const TEXT_UTILITY_SCRIPT: &str = r#"#!/bin/sh
exec "/usr/bin/${0##*/}" "$@"
"#;

/// Supplies the current Linux architecture to the fake installer.
const UNAME_SCRIPT: &str = r#"#!/bin/sh
case "$*" in
  -m) printf '%s\n' "$FAKE_TARGET_ARCH" ;;
  -sm|-ms) printf 'Linux %s\n' "$FAKE_TARGET_ARCH" ;;
  *) printf 'Linux\n' ;;
esac
"#;

/// Owns capability-scoped temporary storage and its cleanup boundary.
struct ScratchDirectory {
    /// Capability handle used for scoped cleanup.
    parent: Dir,
    /// Name used to remove the scratch directory when the fixture drops.
    relative_path: Utf8PathBuf,
    /// Capability root for all staged files.
    root: Dir,
    /// Absolute location passed to child processes.
    absolute_path: Utf8PathBuf,
}

impl ScratchDirectory {
    /// Creates a unique directory beneath the system temporary directory.
    fn new() -> io::Result<Self> {
        let temp_root = Utf8PathBuf::from_path_buf(std::env::temp_dir()).map_err(|path| {
            io::Error::new(
                io::ErrorKind::InvalidInput,
                format!("temporary directory is not UTF-8: {}", path.display()),
            )
        })?;
        let parent = Dir::open_ambient_dir(&temp_root, ambient_authority())?;
        let name = format!(
            "ddlint-build-provisioning-{}-{}",
            std::process::id(),
            NEXT_SCRATCH_DIRECTORY.fetch_add(1, Ordering::Relaxed)
        );
        let relative_path = Utf8PathBuf::from(name);
        parent.create_dir(&relative_path)?;
        let root = parent.open_dir(&relative_path)?;
        let absolute_path = temp_root.join(&relative_path);
        Ok(Self {
            parent,
            relative_path,
            root,
            absolute_path,
        })
    }

    /// Writes a fixture file and optionally marks it executable.
    fn write(&self, relative_path: &str, contents: &str, executable: bool) -> io::Result<()> {
        let path = Utf8Path::new(relative_path);
        if let Some(parent) = path.parent() {
            self.root.create_dir_all(parent)?;
        }
        self.root.write(path, contents)?;
        if executable {
            self.root
                .set_permissions(path, Permissions::from_mode(0o755))?;
        }
        Ok(())
    }

    /// Resolves a fixture-relative path for child-process environment values.
    fn path(&self, relative_path: &str) -> Utf8PathBuf {
        self.absolute_path.join(relative_path)
    }

    /// Reads a fixture file through its capability root.
    fn read(&self, relative_path: &str) -> io::Result<String> {
        self.root.read_to_string(relative_path)
    }
}

impl Drop for ScratchDirectory {
    /// Removes the temporary tree after each provisioning case.
    fn drop(&mut self) {
        let _ = self.parent.remove_dir_all(&self.relative_path);
    }
}

/// Names the build-tool states exercised by provisioning contracts.
#[derive(Clone, Copy)]
pub(super) enum ToolScenario {
    /// Represents the existing pinned local build tools.
    Development,
    /// Omits mold so preflight routes must stop before Cargo.
    MissingMold,
    /// Exposes an incompatible mold version to the checker.
    WrongMold,
    /// Omits the pinned nightly toolchain from the fake rustup.
    MissingNightly,
    /// Requires replacing old mold and adding rust-analyzer.
    UpgradeMoldAndAddAnalyzer,
    /// Reuses CI mold and adds only rust-analyzer.
    ReuseCiMoldAndAddAnalyzer,
    /// Requires installing the pinned nightly with rust-analyzer.
    InstallMissingNightly,
    /// Makes archive verification fail before extraction.
    BadArchiveChecksum,
}

impl ToolScenario {
    /// Returns the fake mold version available in this scenario.
    const fn mold_version(self) -> Option<&'static str> {
        match self {
            Self::MissingMold => None,
            Self::WrongMold => Some("2.40.0"),
            Self::UpgradeMoldAndAddAnalyzer | Self::BadArchiveChecksum => Some("1.0.0"),
            _ => Some("2.41.0"),
        }
    }
}

/// Selects whether fake child tools may fall through to host utilities.
#[derive(Clone, Copy)]
pub(super) enum PathPolicy {
    /// Resolves commands only from the sandbox, causing absent tools to fail.
    SandboxOnly,
    /// Adds the host utility directories for ordinary test recipes.
    IncludeSystemTools,
}

/// Holds inert child tools and private paths for a provisioning test case.
pub(super) struct Sandbox {
    /// Owns staged fake commands and private HOME/TMP paths.
    directory: ScratchDirectory,
    /// Receives the ordered fake-tool invocation trace.
    trace_path: Utf8PathBuf,
    /// Tracks whether fake rustup installed rust-analyzer.
    analyzer_marker: Utf8PathBuf,
    /// Tracks whether fake rustup installed the pinned nightly.
    toolchain_marker: Utf8PathBuf,
    /// Selects the fake tool versions and availability.
    scenario: ToolScenario,
}

impl Sandbox {
    /// Creates fake commands and environment state for a named case.
    pub(super) fn new(scenario: ToolScenario) -> io::Result<Self> {
        let directory = ScratchDirectory::new()?;
        directory.root.create_dir("bin")?;
        directory
            .root
            .create_dir_all("home/.local/share/ddlint/build-tools")?;
        directory.root.create_dir("tmp")?;
        directory.root.create_dir("rustup")?;
        let trace_path = directory.path("trace.log");
        let analyzer_marker = directory.path("rust-analyzer-installed");
        let toolchain_marker = directory.path("rustup-toolchain-installed");
        for (name, contents) in [
            ("curl", CURL_SCRIPT),
            ("wget", WGET_SCRIPT),
            ("sha256sum", CHECKSUM_SCRIPT),
            ("tar", TAR_SCRIPT),
            ("rustup", RUSTUP_SCRIPT),
            ("rustc", RUSTC_SCRIPT),
            ("grep", TEXT_UTILITY_SCRIPT),
            ("uname", UNAME_SCRIPT),
        ] {
            directory.write(&format!("bin/{name}"), contents, true)?;
        }
        for name in ["cat", "sed", "awk"] {
            directory.write(&format!("bin/{name}"), TEXT_UTILITY_SCRIPT, true)?;
        }
        if let Some(version) = scenario.mold_version() {
            let mold_script = format!("#!/bin/sh\nprintf 'mold {version}\\n'\n");
            directory.write("bin/mold", &mold_script, true)?;
        }
        Ok(Self {
            directory,
            trace_path,
            analyzer_marker,
            toolchain_marker,
            scenario,
        })
    }

    /// Returns the fake-command directory used by the child PATH.
    fn bin_path(&self) -> Utf8PathBuf {
        self.directory.path("bin")
    }

    /// Includes standard system directories after the fake-command directory.
    fn path_with_system_tools(&self) -> String {
        format!("{}:/usr/bin:/bin", self.bin_path())
    }

    /// Runs Make with injected harmless build commands and scenario tools.
    pub(super) fn run_make(
        &self,
        target: Option<&str>,
        options: &[&str],
        path_policy: PathPolicy,
    ) -> io::Result<Output> {
        let path = match path_policy {
            PathPolicy::SandboxOnly => self.bin_path().to_string(),
            PathPolicy::IncludeSystemTools => self.path_with_system_tools(),
        };
        let checksum_failure = matches!(self.scenario, ToolScenario::BadArchiveChecksum);
        let nightly_present = !matches!(
            self.scenario,
            ToolScenario::MissingNightly | ToolScenario::InstallMissingNightly
        );
        let analyzer_present = !matches!(
            self.scenario,
            ToolScenario::UpgradeMoldAndAddAnalyzer
                | ToolScenario::ReuseCiMoldAndAddAnalyzer
                | ToolScenario::InstallMissingNightly
                | ToolScenario::BadArchiveChecksum
        );
        let mut command = Command::new("/usr/bin/make");
        command
            .arg("--no-print-directory")
            .args(options)
            .arg("CARGO=/usr/bin/echo cargo-marker")
            .arg("UV=/usr/bin/echo")
            .arg("WHITAKER=/usr/bin/echo")
            .arg("TYPOS_CONFIG_BUILDER=/usr/bin/echo")
            .arg("INTERROGATE=/usr/bin/echo interrogate-marker")
            .arg("APP=ddlint");
        if let Some(target) = target {
            command.arg(target);
        }
        command
            .current_dir(env!("CARGO_MANIFEST_DIR"))
            .env_remove("MAKEFLAGS")
            .env("PATH", path)
            .env("HOME", self.directory.path("home"))
            .env("TMPDIR", self.directory.path("tmp"))
            .env("BUILD_HOST_OS", "Linux")
            .env("RUSTUP_HOME", self.directory.path("rustup"))
            .env("RUSTUP_TOOLCHAIN", "nightly-2025-09-16")
            .env("FAKE_TARGET_ARCH", std::env::consts::ARCH)
            .env("TRACE_FILE", &self.trace_path)
            .env("SANDBOX_ROOT", &self.directory.absolute_path)
            .env("RUST_ANALYZER_MARKER", &self.analyzer_marker)
            .env("RUSTUP_TOOLCHAIN_MARKER", &self.toolchain_marker)
            .env("FAKE_CHECKSUM_FAIL", checksum_failure.to_string())
            .env("FAKE_TOOLCHAIN_PRESENT", nightly_present.to_string())
            .env("FAKE_COMPONENT_PRESENT", analyzer_present.to_string())
            .output()
    }

    /// Reads the recorded fake-tool command sequence.
    pub(super) fn trace(&self) -> io::Result<String> {
        self.directory.read("trace.log")
    }

    /// Reads the mold file placed under the private HOME prefix by fake tar.
    pub(super) fn installed_mold(&self) -> io::Result<String> {
        self.directory
            .read("home/.local/share/ddlint/build-tools/mold-2.41.0/bin/mold")
    }
}
