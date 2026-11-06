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

/// Unknown rule names are rejected after successful deserialization.
///
/// The diagnostic must name the offending rule and advertise the exact
/// supported-rule list, so drift in either the list or its rendering fails.
#[rstest]
fn unknown_rule_name_is_an_error() -> Result<(), Box<dyn std::error::Error>> {
    let file = ConfigFile::new("[cmds.lint.rules]\nnew-rule = 'error'\n")?;
    let error = load(Some(file.path()), empty_environment(), &no_cli_overrides())
        .expect_err("unknown rule names must fail validation");
    let message = error.to_string();

    assert!(message.contains("new-rule"), "{message}");
    assert!(
        message.contains("supported rules are `unused-relation` and `unused-variable`"),
        "{message}"
    );
    Ok(())
}

/// Every rule advertised by the unknown-rule diagnostic is accepted on load.
#[rstest]
#[case("unused-relation")]
#[case("unused-variable")]
fn supported_rule_names_are_accepted(#[case] rule: &str) -> Result<(), Box<dyn std::error::Error>> {
    let contents = format!("[cmds.lint.rules]\n{rule} = 'error'\n");
    let file = ConfigFile::new(&contents)?;
    let settings = load(Some(file.path()), empty_environment(), &no_cli_overrides())?;

    assert!(
        settings.rule_levels.contains_key(rule),
        "{rule} must be accepted"
    );
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
