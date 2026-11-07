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
fn absent_sources_use_documented_defaults() {
    const CHILD_MARKER: &str = "DDLINT_ISOLATED_DISCOVERY_TEST_CHILD";

    if std::env::var_os(CHILD_MARKER).is_some() {
        let isolated_root =
            std::env::current_dir().expect("the child working directory is readable");
        // Pin every platform discovery rung so even `/etc/xdg` cannot supply
        // host configuration to this defaults assertion.
        let environment = MapEnv::new()
            .with_var("HOME", isolated_root.as_os_str())
            .with_var("USERPROFILE", isolated_root.as_os_str())
            .with_var("XDG_CONFIG_HOME", isolated_root.as_os_str())
            .with_var("XDG_CONFIG_DIRS", isolated_root.as_os_str())
            .with_var("APPDATA", isolated_root.as_os_str());
        let settings =
            load(None, environment, &no_cli_overrides()).expect("the default settings should load");

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
        let directory = tempfile::TempDir::new().expect("a temporary directory should be created");
        let output = Command::new(std::env::current_exe().expect("the test binary is locatable"))
            .args([
                "--exact",
                "absent_sources_use_documented_defaults",
                "--nocapture",
            ])
            .current_dir(directory.path())
            .env_clear()
            .env(CHILD_MARKER, "1")
            .output()
            .expect("the isolated discovery child should run");

        assert!(
            output.status.success(),
            "isolated discovery child failed: stdout={}, stderr={}",
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        );
        // A renamed test would make `--exact` match nothing while the child
        // still exits successfully, so require the child to report one pass.
        let stdout = String::from_utf8_lossy(&output.stdout);
        assert!(
            stdout.contains("test result: ok. 1 passed;"),
            "isolated discovery child did not run exactly one test: stdout={stdout}"
        );
    }
}

/// File and environment values use the selected lint command namespace.
#[rstest]
fn file_and_environment_layers_resolve_under_cmds_lint() {
    let file = ConfigFile::new(
        "[cmds.lint]\nformat = 'json'\nignore_patterns = \
         ['from-file']\n[cmds.lint.rules]\nunused-variable = 'error'\n",
    )
    .expect("the namespace fixture file should be written");
    let environment = MapEnv::new()
        .with_var("DDLINT_CMDS__LINT__FORMAT", "compact")
        .with_var("DDLINT_CMDS__LINT__IGNORE_PATTERNS", "[\"from-env\"]")
        .with_var("DDLINT_CMDS__LINT__RULES__unused-variable", "warn");

    let settings = load(Some(file.path()), environment, &no_cli_overrides())
        .expect("the layered configuration should load");

    assert_eq!(settings.format, OutputFormat::Compact);
    assert_eq!(settings.ignore_patterns, vec!["from-env".to_owned()]);
    assert_eq!(
        settings.rule_levels.get("unused-variable"),
        Some(&RuleLevel::Warn)
    );
}

/// A file that restates every field replaces both defaults, including collections.
#[rstest]
fn file_only_values_override_defaults() {
    let file = ConfigFile::new(
        "[cmds.lint]\nformat = 'json'\nignore_patterns = ['generated/', 'vendor/']\n",
    )
    .expect("the defaults fixture file should be written");

    let settings = load(Some(file.path()), empty_environment(), &no_cli_overrides())
        .expect("the file-only configuration should load");

    assert_eq!(settings.format, OutputFormat::Json);
    assert_eq!(
        settings.ignore_patterns,
        vec!["generated/".to_owned(), "vendor/".to_owned()]
    );
}

/// Explicit CLI values override file and environment values without parser defaults.
#[rstest]
fn explicit_cli_values_override_lower_layers() {
    let file = ConfigFile::new("[cmds.lint]\nformat = 'json'\nignore_patterns = ['from-file']\n")
        .expect("the precedence fixture file should be written");
    let environment = MapEnv::new()
        .with_var("DDLINT_CMDS__LINT__FORMAT", "compact")
        .with_var("DDLINT_CMDS__LINT__IGNORE_PATTERNS", "[\"from-env\"]");
    let cli = LintCliArgs {
        format: Some(OutputFormat::Rich),
        no_ignore: true,
    };

    let settings =
        load(Some(file.path()), environment, &cli).expect("the command-line override should load");

    assert_eq!(settings.format, OutputFormat::Rich);
    assert!(
        settings.ignore_patterns.is_empty(),
        "`--no-ignore` must clear every inherited pattern, found {:?}",
        settings.ignore_patterns
    );
}

/// Rule levels supplied in a file are mapped to the reusable linter's domain type.
#[rstest]
fn file_rule_levels_map_to_domain_levels() {
    let file =
        ConfigFile::new("[cmds.lint.rules]\nunused-relation = 'hint'\nunused-variable = 'error'\n")
            .expect("the rule-levels fixture file should be written");

    let settings = load(Some(file.path()), empty_environment(), &no_cli_overrides())
        .expect("the rule levels should load");

    assert_eq!(
        settings.rule_levels.get("unused-relation"),
        Some(&RuleLevel::Hint)
    );
    assert_eq!(
        settings.rule_levels.get("unused-variable"),
        Some(&RuleLevel::Error)
    );
}
