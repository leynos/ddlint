//! Automatic discovery, selector precedence, and `extends` inheritance tests.
//!
//! These exercise the application-owned wiring to `OrthoConfig` rather than
//! repeating every upstream discovery permutation: the environment selector,
//! command-line selection above it, project-directory discovery, and relative
//! `extends` resolution with child-over-parent values.

mod support;

use std::error::Error;
use std::path::{Path, PathBuf};
use std::process::Command;

use cap_std::ambient_authority;
use cap_std::fs::Dir;
use ddlint::linter::RuleLevel;
use ddlint_cli::config::OutputFormat;
use ortho_config::MapEnv;
use rstest::rstest;
use support::{ConfigFile, empty_environment, load, no_cli_overrides};

/// Tells the isolated child process which behaviour to assert.
const PROJECT_DISCOVERY_CHILD: &str = "DDLINT_PROJECT_DISCOVERY_TEST_CHILD";

/// Distinguishes the environment-selector child from the project-discovery one.
const ENVIRONMENT_SELECTION_CHILD: &str = "DDLINT_ENVIRONMENT_SELECTION_TEST_CHILD";

/// Pin every automatic discovery root so only the isolated directory is scanned.
fn pinned_environment(root: &Path) -> MapEnv {
    MapEnv::new()
        .with_var("HOME", root.as_os_str())
        .with_var("USERPROFILE", root.as_os_str())
        .with_var("XDG_CONFIG_HOME", root.as_os_str())
        .with_var("XDG_CONFIG_DIRS", root.as_os_str())
        .with_var("APPDATA", root.as_os_str())
}

/// Write a configuration file into `directory`, returning its full path.
fn write_config(directory: &Path, name: &str, contents: &str) -> Result<PathBuf, Box<dyn Error>> {
    let file_system = Dir::open_ambient_dir(directory, ambient_authority())?;
    file_system.write(name, contents.as_bytes())?;
    Ok(directory.join(name))
}

/// Run one test in a child process rooted at `directory` with a clean environment.
///
/// Discovery reads the working directory and the process environment, so a
/// child process is the only way to isolate the project rung without mutating
/// shared process state.
fn run_isolated_child(
    directory: &Path,
    marker: &str,
    test_name: &str,
) -> Result<(), Box<dyn Error>> {
    let output = Command::new(std::env::current_exe()?)
        .args(["--exact", test_name, "--nocapture"])
        .current_dir(directory)
        .env_clear()
        .env(marker, "1")
        .output()?;

    assert!(
        output.status.success(),
        "isolated child `{test_name}` failed: stdout={}, stderr={}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    Ok(())
}

/// `DDLINT_CONFIG_PATH` selects a file that supplies settings.
#[rstest]
fn environment_selected_file_is_loaded() -> Result<(), Box<dyn Error>> {
    let file =
        ConfigFile::new("[cmds.lint]\nformat = 'json'\nignore_patterns = ['from-env-file']\n")?;
    let environment = MapEnv::new().with_var("DDLINT_CONFIG_PATH", file.path().as_os_str());

    let settings = load(None, environment, &no_cli_overrides())?;

    assert_eq!(settings.format, OutputFormat::Json);
    assert_eq!(settings.ignore_patterns, vec!["from-env-file".to_owned()]);
    Ok(())
}

/// An environment-selected file is required rather than silently skipped.
#[rstest]
fn missing_environment_selected_file_is_an_error() -> Result<(), Box<dyn Error>> {
    let directory = tempfile::TempDir::new()?;
    let missing = directory.path().join("missing.toml");
    let environment = MapEnv::new().with_var("DDLINT_CONFIG_PATH", missing.as_os_str());

    let error = load(None, environment, &no_cli_overrides())
        .expect_err("an environment-selected missing file must fail");

    assert!(error.to_string().contains("missing.toml"), "{error:#?}");
    Ok(())
}

/// `--config` outranks `DDLINT_CONFIG_PATH` when both name a file.
#[rstest]
fn command_line_selection_overrides_the_environment() -> Result<(), Box<dyn Error>> {
    let cli_file = ConfigFile::new("[cmds.lint]\nformat = 'compact'\n")?;
    let environment_file = ConfigFile::new("[cmds.lint]\nformat = 'json'\n")?;
    let environment =
        MapEnv::new().with_var("DDLINT_CONFIG_PATH", environment_file.path().as_os_str());

    let settings = load(Some(cli_file.path()), environment, &no_cli_overrides())?;

    assert_eq!(settings.format, OutputFormat::Compact);
    Ok(())
}

/// `ddlint.toml` in the working directory is discovered without a selector.
#[rstest]
fn project_file_in_the_working_directory_is_discovered() -> Result<(), Box<dyn Error>> {
    const TEST_NAME: &str = "project_file_in_the_working_directory_is_discovered";

    if std::env::var_os(PROJECT_DISCOVERY_CHILD).is_some() {
        let isolated_root = std::env::current_dir()?;
        let settings = load(
            None,
            pinned_environment(isolated_root.as_path()),
            &no_cli_overrides(),
        )?;

        assert_eq!(settings.format, OutputFormat::Json);
        assert_eq!(settings.ignore_patterns, vec!["generated/".to_owned()]);
    } else {
        let directory = tempfile::TempDir::new()?;
        write_config(
            directory.path(),
            "ddlint.toml",
            "[cmds.lint]\nformat = 'json'\nignore_patterns = ['generated/']\n",
        )?;
        run_isolated_child(directory.path(), PROJECT_DISCOVERY_CHILD, TEST_NAME)?;
    }
    Ok(())
}

/// An environment-selected file suppresses discovery of the project file.
#[rstest]
fn environment_selection_suppresses_project_discovery() -> Result<(), Box<dyn Error>> {
    const TEST_NAME: &str = "environment_selection_suppresses_project_discovery";

    if std::env::var_os(ENVIRONMENT_SELECTION_CHILD).is_some() {
        let isolated_root = std::env::current_dir()?;
        let selected = isolated_root.join("selected.toml");
        let environment = pinned_environment(isolated_root.as_path())
            .with_var("DDLINT_CONFIG_PATH", selected.as_os_str());
        let settings = load(None, environment, &no_cli_overrides())?;

        assert_eq!(settings.format, OutputFormat::Json);
    } else {
        let directory = tempfile::TempDir::new()?;
        write_config(
            directory.path(),
            "ddlint.toml",
            "[cmds.lint]\nformat = 'compact'\n",
        )?;
        write_config(
            directory.path(),
            "selected.toml",
            "[cmds.lint]\nformat = 'json'\n",
        )?;
        run_isolated_child(directory.path(), ENVIRONMENT_SELECTION_CHILD, TEST_NAME)?;
    }
    Ok(())
}

/// A relative `extends` parent contributes values beneath the child.
#[rstest]
fn relative_extends_layers_the_parent_under_the_child() -> Result<(), Box<dyn Error>> {
    let directory = tempfile::TempDir::new()?;
    write_config(
        directory.path(),
        "parent.toml",
        "[cmds.lint]\nformat = 'json'\nignore_patterns = ['parent-only']\n\
         [cmds.lint.rules]\nunused-variable = 'error'\n",
    )?;
    let child = write_config(
        directory.path(),
        "child.toml",
        "extends = 'parent.toml'\n[cmds.lint]\nignore_patterns = ['child-only']\n\
         [cmds.lint.rules]\nunused-relation = 'hint'\n",
    )?;

    let settings = load(Some(&child), empty_environment(), &no_cli_overrides())?;

    // The child does not restate `format`, so the parent value survives.
    assert_eq!(settings.format, OutputFormat::Json);
    assert_eq!(
        settings.rule_levels.get("unused-variable"),
        Some(&RuleLevel::Error)
    );
    assert_eq!(
        settings.rule_levels.get("unused-relation"),
        Some(&RuleLevel::Hint)
    );
    assert_eq!(settings.ignore_patterns, vec!["child-only".to_owned()]);
    Ok(())
}
