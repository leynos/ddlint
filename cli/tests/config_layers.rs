//! Precedence and nested command namespace tests.

mod support;

use std::process::Command;

use ddlint::linter::RuleLevel;
use ddlint_cli::config::{LintCliArgs, OutputFormat};
use ortho_config::MapEnv;
use rstest::rstest;
use support::{ConfigFile, empty_environment, load, no_cli_overrides};

/// The default lint settings are used when every optional source is absent.
#[rstest]
fn absent_sources_use_documented_defaults() -> Result<(), Box<dyn std::error::Error>> {
    const CHILD_MARKER: &str = "DDLINT_ISOLATED_DISCOVERY_TEST_CHILD";

    if std::env::var_os(CHILD_MARKER).is_some() {
        let isolated_root = std::env::current_dir()?;
        // Pin every platform discovery rung so even `/etc/xdg` cannot supply
        // host configuration to this defaults assertion.
        let environment = MapEnv::new()
            .with_var("HOME", isolated_root.as_os_str())
            .with_var("USERPROFILE", isolated_root.as_os_str())
            .with_var("XDG_CONFIG_HOME", isolated_root.as_os_str())
            .with_var("XDG_CONFIG_DIRS", isolated_root.as_os_str())
            .with_var("APPDATA", isolated_root.as_os_str());
        let settings = load(None, environment, &no_cli_overrides())?;

        assert_eq!(settings.format, OutputFormat::Rich);
        assert_eq!(
            settings.ignore_patterns,
            vec![
                ".git/".to_owned(),
                "build/".to_owned(),
                "target/".to_owned()
            ]
        );
        assert!(settings.rule_levels.is_empty());
    } else {
        let directory = tempfile::TempDir::new()?;
        let output = Command::new(std::env::current_exe()?)
            .args([
                "--exact",
                "absent_sources_use_documented_defaults",
                "--nocapture",
            ])
            .current_dir(directory.path())
            .env_clear()
            .env(CHILD_MARKER, "1")
            .output()?;

        assert!(
            output.status.success(),
            "isolated discovery child failed: stdout={}, stderr={}",
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        );
    }
    Ok(())
}

/// File and environment values use the selected lint command namespace.
#[rstest]
fn file_and_environment_layers_resolve_under_cmds_lint() -> Result<(), Box<dyn std::error::Error>> {
    let file = ConfigFile::new(
        "[cmds.lint]\nformat = 'json'\nignore_patterns = ['from-file']\n\
         [cmds.lint.rules]\nunused-variable = 'error'\n",
    )?;
    let environment = MapEnv::new()
        .with_var("DDLINT_CMDS__LINT__FORMAT", "compact")
        .with_var("DDLINT_CMDS__LINT__IGNORE_PATTERNS", "[\"from-env\"]")
        .with_var("DDLINT_CMDS__LINT__RULES__unused-variable", "warn");

    let settings = load(Some(file.path()), environment, &no_cli_overrides())?;

    assert_eq!(settings.format, OutputFormat::Compact);
    assert_eq!(settings.ignore_patterns, vec!["from-env".to_owned()]);
    assert_eq!(
        settings.rule_levels.get("unused-variable"),
        Some(&RuleLevel::Warn)
    );
    Ok(())
}

/// Explicit CLI values override file and environment values without parser defaults.
#[rstest]
fn explicit_cli_values_override_lower_layers() -> Result<(), Box<dyn std::error::Error>> {
    let file = ConfigFile::new("[cmds.lint]\nformat = 'json'\nignore_patterns = ['from-file']\n")?;
    let environment = MapEnv::new()
        .with_var("DDLINT_CMDS__LINT__FORMAT", "compact")
        .with_var("DDLINT_CMDS__LINT__IGNORE_PATTERNS", "[\"from-env\"]");
    let cli = LintCliArgs {
        format: Some(OutputFormat::Rich),
        no_ignore: true,
    };

    let settings = load(Some(file.path()), environment, &cli)?;

    assert_eq!(settings.format, OutputFormat::Rich);
    assert!(settings.ignore_patterns.is_empty());
    Ok(())
}

/// Rule levels supplied in a file are mapped to the reusable linter's domain type.
#[rstest]
fn file_rule_levels_map_to_domain_levels() -> Result<(), Box<dyn std::error::Error>> {
    let file = ConfigFile::new(
        "[cmds.lint.rules]\nunused-relation = 'hint'\nunused-variable = 'error'\n",
    )?;

    let settings = load(Some(file.path()), empty_environment(), &no_cli_overrides())?;

    assert_eq!(
        settings.rule_levels.get("unused-relation"),
        Some(&RuleLevel::Hint)
    );
    assert_eq!(
        settings.rule_levels.get("unused-variable"),
        Some(&RuleLevel::Error)
    );
    Ok(())
}
