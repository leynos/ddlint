//! Invalid and missing configuration source tests.

mod support;

use ortho_config::MapEnv;
use rstest::rstest;
use support::{ConfigFile, empty_environment, load, no_cli_overrides};

/// Malformed TOML is reported instead of falling back to defaults.
#[rstest]
fn malformed_explicit_file_is_an_error() -> Result<(), Box<dyn std::error::Error>> {
    let file = ConfigFile::new("[cmds.lint\nformat = 'json'\n")?;
    let error = load(Some(file.path()), empty_environment(), &no_cli_overrides())
        .expect_err("malformed explicit TOML must fail");

    assert!(error.to_string().contains("ddlint.toml"));
    Ok(())
}

/// An explicitly selected missing file is required and reported.
#[rstest]
fn missing_explicit_file_is_an_error() -> Result<(), Box<dyn std::error::Error>> {
    let directory = tempfile::TempDir::new()?;
    let missing_path = directory.path().join("missing.toml");
    let error = load(
        Some(&missing_path),
        empty_environment(),
        &no_cli_overrides(),
    )
    .expect_err("an explicitly selected missing file must fail");
    assert!(error.to_string().contains("missing.toml"));
    Ok(())
}

/// A selected directory cannot be read as a configuration file.
#[rstest]
fn unreadable_explicit_source_is_an_error() -> Result<(), Box<dyn std::error::Error>> {
    let directory = tempfile::TempDir::new()?;
    let error = load(
        Some(directory.path()),
        empty_environment(),
        &no_cli_overrides(),
    )
    .expect_err("a directory selected as a file must fail");

    assert!(error.to_string().contains("Configuration file"));
    Ok(())
}

/// Deserialization rejects invalid formats, severities, and unknown fields.
#[rstest]
#[case("[cmds.lint]\nformat = 'xml'\n")]
#[case("[cmds.lint]\nunknown = true\n")]
#[case("unexpected = true\n")]
#[case("[cmds.lint.rules]\nunused-variable = 'fatal'\n")]
fn invalid_values_and_unknown_keys_fail(
    #[case] contents: &str,
) -> Result<(), Box<dyn std::error::Error>> {
    let file = ConfigFile::new(contents)?;
    let result = load(Some(file.path()), empty_environment(), &no_cli_overrides());

    assert!(result.is_err());
    Ok(())
}

/// Rule names this build accepts in `[cmds.lint.rules]`.
///
/// The list is deliberately independent of the production table so that drift
/// is detected in both directions: a rule added to the validator without being
/// recorded here, or a rule advertised by the diagnostic that the validator
/// then rejects.
const EXPECTED_RULES: [&str; 2] = ["unused-relation", "unused-variable"];

/// Render the supported-rule clause the unknown-rule diagnostic must contain.
///
/// Joining with " and " mirrors the diagnostic's own connector, so this also
/// fails if its wording changes rather than merely the rule membership.
fn expected_supported_rules_clause() -> String {
    let rules = EXPECTED_RULES
        .iter()
        .map(|rule| format!("`{rule}`"))
        .collect::<Vec<_>>()
        .join(" and ");

    format!("supported rules are {rules}")
}

/// Unknown rule names are rejected after successful deserialization.
///
/// The diagnostic must name the offending rule and advertise the exact
/// supported-rule list, so drift in either the list or its rendering fails.
#[rstest]
fn unknown_rule_name_is_an_error() {
    let file = ConfigFile::new("[cmds.lint.rules]\nnew-rule = 'error'\n")
        .expect("the configuration fixture must be written");
    let error = load(Some(file.path()), empty_environment(), &no_cli_overrides())
        .expect_err("unknown rule names must fail validation");
    let message = error.to_string();
    let supported_clause = expected_supported_rules_clause();

    assert!(message.contains("new-rule"), "{message}");
    assert!(message.contains(&supported_clause), "{message}");
}

/// The validator accepts every advertised rule and rejects an unlisted one.
///
/// Both directions read [`EXPECTED_RULES`], so acceptance coverage grows with
/// the list while the rejection case still proves that names outside it remain
/// errors.
#[rstest]
fn supported_rules_are_enforced_both_ways() -> Result<(), Box<dyn std::error::Error>> {
    for rule in EXPECTED_RULES {
        let contents = format!("[cmds.lint.rules]\n{rule} = 'error'\n");
        let file = ConfigFile::new(&contents)?;
        let settings = load(Some(file.path()), empty_environment(), &no_cli_overrides())?;

        if !settings.rule_levels.contains_key(rule) {
            return Err(format!("{rule} must be accepted").into());
        }
    }

    let unsupported = "new-rule";
    let contents = format!("[cmds.lint.rules]\n{unsupported} = 'error'\n");
    let file = ConfigFile::new(&contents)?;

    if load(Some(file.path()), empty_environment(), &no_cli_overrides()).is_ok() {
        return Err(format!("{unsupported} must be rejected").into());
    }

    Ok(())
}

/// An environment value with an invalid enum spelling is not ignored.
#[rstest]
fn invalid_environment_value_is_an_error() -> Result<(), Box<dyn std::error::Error>> {
    let file = ConfigFile::new("")?;
    let environment = MapEnv::new().with_var("DDLINT_CMDS__LINT__FORMAT", "xml");
    let error = load(Some(file.path()), environment, &no_cli_overrides())
        .expect_err("invalid environment values must fail validation");

    assert!(error.to_string().contains("xml"), "{error:#?}");
    Ok(())
}
