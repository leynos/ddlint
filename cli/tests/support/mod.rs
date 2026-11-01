//! Shared test fixtures for explicit configuration sources.

use std::error::Error;
use std::ffi::OsString;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use cap_std::ambient_authority;
use cap_std::fs::Dir;
use ortho_config::{MapEnv, SharedEnvSource, SharedScanEnvSource};
use tempfile::TempDir;

use ddlint_cli::config::{LintCliArgs, ValidatedLintSettings, load_lint_settings_with_sources};

/// A temporary directory and the selected TOML path within it.
pub(super) struct ConfigFile {
    _directory: TempDir,
    path: PathBuf,
}

impl ConfigFile {
    /// Create an explicit TOML source in a fresh temporary directory.
    pub(super) fn new(contents: &str) -> Result<Self, Box<dyn Error>> {
        let directory = TempDir::new()?;
        let file_system = Dir::open_ambient_dir(directory.path(), ambient_authority())?;
        file_system.write("ddlint.toml", contents.as_bytes())?;
        let path = directory.path().join("ddlint.toml");
        Ok(Self {
            _directory: directory,
            path,
        })
    }

    /// Return the selected file path.
    pub(super) fn path(&self) -> &Path {
        &self.path
    }
}

/// Load settings from injected environment and optional explicit TOML input.
pub(super) fn load(
    file: Option<&Path>,
    environment: MapEnv,
    cli: &LintCliArgs,
) -> Result<ValidatedLintSettings, Box<dyn Error>> {
    let mut arguments = vec![OsString::from("ddlint")];
    if let Some(path) = file {
        arguments.push(OsString::from("--config"));
        arguments.push(path.as_os_str().to_os_string());
    }

    let shared_environment = Arc::new(environment);
    let discovery: SharedEnvSource = shared_environment.clone();
    let merge: SharedScanEnvSource = shared_environment;
    load_lint_settings_with_sources(arguments, cli, discovery, merge).map_err(Into::into)
}

/// Return the default test CLI options.
pub(super) fn no_cli_overrides() -> LintCliArgs {
    LintCliArgs::default()
}

/// Create a closed injected source with no environment variables.
pub(super) fn empty_environment() -> MapEnv {
    MapEnv::new()
}
