//! Configuration types and source resolution for the planned `ddlint` CLI.

use std::collections::BTreeMap;
use std::ffi::OsString;
use std::sync::Arc;

use clap::{Args, ValueEnum};
use ddlint::linter::RuleLevel;
use ortho_config::{OrthoConfig, OrthoError, SharedEnvSource, SharedScanEnvSource};
use serde::de::Error as DeError;
use serde::{Deserialize, Deserializer, Serialize};
use thiserror::Error;

/// Output style selected by the future lint command.
#[derive(Clone, Copy, Debug, Default, Deserialize, Eq, PartialEq, Serialize, ValueEnum)]
#[serde(rename_all = "lowercase")]
pub enum OutputFormat {
    /// Human-oriented diagnostics with source context.
    #[default]
    Rich,
    /// Machine-readable JSON diagnostics.
    Json,
    /// Compact one-line diagnostics.
    Compact,
}

/// Command-line-only lint options kept separate from loaded configuration.
#[derive(Clone, Debug, Default, Args)]
pub struct LintCliArgs {
    /// Select a renderer explicitly, overriding files and environment values.
    #[arg(long, value_enum)]
    pub format: Option<OutputFormat>,

    /// Disable all configured and default ignore patterns.
    #[arg(long)]
    pub no_ignore: bool,
}

/// Validated settings passed from the application boundary to lint execution.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ValidatedLintSettings {
    /// Selected diagnostic output format.
    pub format: OutputFormat,
    /// Glob patterns ignored by the planned lint command.
    pub ignore_patterns: Vec<String>,
    /// Validated per-rule severity overrides.
    pub rule_levels: BTreeMap<String, RuleLevel>,
}

/// Failures while loading or validating application configuration.
#[derive(Debug, Error)]
pub enum ConfigError {
    /// `OrthoConfig` could not parse arguments, read sources, or deserialize values.
    #[error("failed to load ddlint configuration: {0}")]
    Load(#[from] Arc<OrthoError>),

    /// A severity override names a rule not shipped by this version of ddlint.
    #[error(
        "unknown lint rule `{name}` in `[cmds.lint.rules]`; supported rules are `unused-relation` and `unused-variable`"
    )]
    UnknownRule {
        /// Unrecognised rule identifier from the configuration source.
        name: String,
    },

    /// A configuration key is not part of the supported CLI configuration.
    #[error("unknown configuration field `{name}`")]
    UnknownField {
        /// Unrecognised top-level key from the configuration source.
        name: String,
    },
}

/// Full document shape loaded by `OrthoConfig` at the CLI application boundary.
#[derive(Debug, Default, Deserialize, OrthoConfig, Serialize)]
#[serde(default)]
#[ortho_config(
    prefix = "DDLINT_",
    discovery(
        app_name = "ddlint",
        config_file_name = "ddlint.toml",
        dotfile_name = ".ddlint.toml",
        project_file_name = "ddlint.toml",
        config_cli_long = "config",
        config_cli_short = 'c',
        config_cli_visible = true
    )
)]
struct AppConfig {
    #[ortho_config(skip_cli)]
    cmds: CommandsConfig,
    // OrthoConfig adds `config_path` to its CLI merge layer. Capture all root
    // keys here so that this generated value can be ignored explicitly while
    // other unknown keys remain errors.
    #[serde(flatten)]
    #[ortho_config(skip_cli)]
    extra: BTreeMap<String, serde_json::Value>,
}

/// Nested namespace for commands that own configuration settings.
#[derive(Debug, Default, Deserialize, Serialize)]
#[serde(default, deny_unknown_fields)]
struct CommandsConfig {
    lint: LintConfig,
}

/// Configuration file and environment values for the planned lint command.
#[derive(Debug, Deserialize, Serialize)]
#[serde(default, deny_unknown_fields)]
struct LintConfig {
    format: OutputFormat,
    #[serde(deserialize_with = "deserialize_ignore_patterns")]
    ignore_patterns: Vec<String>,
    rules: BTreeMap<String, ConfigRuleLevel>,
}

/// Read TOML arrays and JSON-array strings used for collection environment values.
fn deserialize_ignore_patterns<'de, D>(deserializer: D) -> Result<Vec<String>, D::Error>
where
    D: Deserializer<'de>,
{
    #[derive(Deserialize)]
    #[serde(untagged)]
    enum PatternSource {
        List(Vec<String>),
        Json(String),
    }

    match PatternSource::deserialize(deserializer)? {
        PatternSource::List(patterns) => Ok(patterns),
        PatternSource::Json(value) => serde_json::from_str(&value)
            .map_err(|error| D::Error::custom(format!("expected a JSON string array: {error}"))),
    }
}

impl Default for LintConfig {
    fn default() -> Self {
        Self {
            format: OutputFormat::Rich,
            ignore_patterns: vec![
                ".git/".to_owned(),
                "build/".to_owned(),
                "target/".to_owned(),
            ],
            rules: BTreeMap::new(),
        }
    }
}

/// Severity spelling accepted from TOML and environment sources.
#[derive(Clone, Copy, Debug, Deserialize, Serialize)]
#[serde(rename_all = "lowercase")]
enum ConfigRuleLevel {
    Allow,
    Hint,
    Warn,
    Error,
}

impl From<ConfigRuleLevel> for RuleLevel {
    fn from(level: ConfigRuleLevel) -> Self {
        match level {
            ConfigRuleLevel::Allow => Self::Allow,
            ConfigRuleLevel::Hint => Self::Hint,
            ConfigRuleLevel::Warn => Self::Warn,
            ConfigRuleLevel::Error => Self::Error,
        }
    }
}

/// Resolve lint settings using `OrthoConfig`'s injected discovery and merge sources.
///
/// `arguments` contains only the generated configuration selector, such as
/// `ddlint --config /path/to/ddlint.toml`. Explicit lint flags are supplied
/// separately so their optional values remain above file and environment
/// layers without parser defaults.
///
/// # Examples
///
/// ```rust
/// use std::sync::Arc;
/// use ortho_config::{MapEnv, SharedEnvSource, SharedScanEnvSource};
/// use ddlint_cli::config::{LintCliArgs, OutputFormat, load_lint_settings_with_sources};
///
/// let environment = Arc::new(MapEnv::new());
/// let discovery: SharedEnvSource = environment.clone();
/// let merge: SharedScanEnvSource = environment;
/// let settings = load_lint_settings_with_sources(
///     ["ddlint"],
///     &LintCliArgs::default(),
///     discovery,
///     merge,
/// )?;
/// assert_eq!(settings.format, OutputFormat::Rich);
/// # Ok::<(), Box<dyn std::error::Error>>(())
/// ```
///
/// # Errors
///
/// Returns [`ConfigError::Load`] for source or deserialization failures and
/// [`ConfigError::UnknownField`] for unexpected top-level keys. Returns
/// [`ConfigError::UnknownRule`] when a configuration names an unavailable rule.
pub fn load_lint_settings_with_sources<I, T>(
    arguments: I,
    cli: &LintCliArgs,
    discovery: SharedEnvSource,
    merge: SharedScanEnvSource,
) -> Result<ValidatedLintSettings, ConfigError>
where
    I: IntoIterator<Item = T>,
    T: Into<OsString> + Clone,
{
    let arguments = arguments.into_iter().map(Into::into).collect::<Vec<_>>();
    let has_config_selector =
        has_cli_config_selector(&arguments) || discovery.get("DDLINT_CONFIG_PATH").is_some();
    let config = AppConfig::load_from_iter_with_sources(arguments, discovery, merge)?;
    if let Some(name) = config
        .extra
        .keys()
        .find(|name| name.as_str() != "config_path" || !has_config_selector)
    {
        return Err(ConfigError::UnknownField { name: name.clone() });
    }
    let mut lint = config.cmds.lint;
    if let Some(format) = cli.format {
        lint.format = format;
    }
    if cli.no_ignore {
        lint.ignore_patterns.clear();
    }
    validate_lint_config(lint)
}

/// Recognise the generated configuration path selector before deserialisation.
fn has_cli_config_selector(arguments: &[OsString]) -> bool {
    arguments.iter().skip(1).any(|argument| {
        argument == "--config"
            || argument == "-c"
            || argument.to_string_lossy().starts_with("--config=")
            || argument
                .to_string_lossy()
                .strip_prefix("-c")
                .is_some_and(|value| !value.is_empty())
    })
}

/// Convert loaded values to domain settings and reject rule names not in this build.
fn validate_lint_config(config: LintConfig) -> Result<ValidatedLintSettings, ConfigError> {
    let mut rule_levels = BTreeMap::new();
    for (name, level) in config.rules {
        let domain_name = match name.as_str() {
            "unused-relation" | "unused-variable" => name,
            _ => return Err(ConfigError::UnknownRule { name }),
        };
        rule_levels.insert(domain_name, RuleLevel::from(level));
    }

    Ok(ValidatedLintSettings {
        format: config.format,
        ignore_patterns: config.ignore_patterns,
        rule_levels,
    })
}
