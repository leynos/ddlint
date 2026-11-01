//! Property tests for source precedence and collection replacement.
//!
//! Each generated case assigns arbitrary values to a file layer, an
//! environment layer, and the explicit command-line layer, then checks that
//! the resolved settings equal the documented rule: for every field the
//! highest source that supplies a value wins, absent layers fall through, and
//! lists replace rather than merge.

mod support;

use std::collections::BTreeMap;
use std::fmt::Write as _;

use ddlint::linter::RuleLevel;
use ddlint_cli::config::{LintCliArgs, OutputFormat};
use ortho_config::MapEnv;
use proptest::prelude::*;
use support::{ConfigFile, empty_environment, load, no_cli_overrides};

/// Rule names this build accepts severity overrides for.
const RULE_NAMES: &[&str] = &["unused-relation", "unused-variable"];

/// Ignore patterns used to build generated file and environment layers.
const PATTERN_POOL: &[&str] = &[
    ".git/",
    "build/",
    "target/",
    "generated/",
    "vendor/",
    "src/",
];

/// Severity levels a generated layer may assign to a rule.
const RULE_LEVELS: &[RuleLevel] = &[
    RuleLevel::Allow,
    RuleLevel::Hint,
    RuleLevel::Warn,
    RuleLevel::Error,
];

/// Values one configuration source supplies for the lint command.
#[derive(Clone, Debug, Default)]
struct Layer {
    format: Option<OutputFormat>,
    ignore_patterns: Option<Vec<String>>,
    rules: Vec<(&'static str, RuleLevel)>,
}

/// Generate one accepted output format.
fn format_strategy() -> impl Strategy<Value = OutputFormat> {
    prop_oneof![
        Just(OutputFormat::Rich),
        Just(OutputFormat::Json),
        Just(OutputFormat::Compact),
    ]
}

/// Generate one accepted rule severity.
fn level_strategy() -> impl Strategy<Value = RuleLevel> {
    prop::sample::select(RULE_LEVELS)
}

/// Generate severity overrides for a random subset of the supported rules.
fn rule_strategy() -> impl Strategy<Value = Vec<(&'static str, RuleLevel)>> {
    prop::sample::subsequence(RULE_NAMES.to_vec(), 0..=RULE_NAMES.len())
        .prop_flat_map(|names| {
            let levels = prop::collection::vec(level_strategy(), names.len());
            (Just(names), levels)
        })
        .prop_map(|(names, levels)| names.into_iter().zip(levels).collect())
}

/// Generate the field values one source supplies, including absent fields.
fn layer_strategy() -> impl Strategy<Value = Layer> {
    (
        prop::option::of(format_strategy()),
        prop::option::of(prop::collection::vec(
            prop::sample::select(PATTERN_POOL),
            0..4,
        )),
        rule_strategy(),
    )
        .prop_map(|(format, ignore_patterns, rules)| Layer {
            format,
            ignore_patterns: ignore_patterns
                .map(|patterns| patterns.into_iter().map(str::to_owned).collect()),
            rules,
        })
}

/// Return the TOML and environment spelling of an output format.
fn format_spelling(format: OutputFormat) -> &'static str {
    match format {
        OutputFormat::Rich => "rich",
        OutputFormat::Json => "json",
        OutputFormat::Compact => "compact",
    }
}

/// Return the configuration spelling of a rule severity.
fn level_spelling(level: RuleLevel) -> &'static str {
    match level {
        RuleLevel::Allow => "allow",
        RuleLevel::Hint => "hint",
        RuleLevel::Warn => "warn",
        RuleLevel::Error => "error",
    }
}

/// Render a layer as the TOML document an explicit configuration file holds.
fn render_file(layer: &Layer) -> String {
    let mut document = String::from("[cmds.lint]\n");
    if let Some(format) = layer.format {
        let _ = writeln!(document, "format = '{}'", format_spelling(format));
    }
    if let Some(patterns) = &layer.ignore_patterns {
        let quoted = patterns
            .iter()
            .map(|pattern| format!("'{pattern}'"))
            .collect::<Vec<_>>()
            .join(", ");
        let _ = writeln!(document, "ignore_patterns = [{quoted}]");
    }
    if !layer.rules.is_empty() {
        document.push_str("[cmds.lint.rules]\n");
        for (name, level) in &layer.rules {
            let _ = writeln!(document, "{name} = '{}'", level_spelling(*level));
        }
    }
    document
}

/// Render patterns as the JSON string array an environment value carries.
fn render_json_array(patterns: &[String]) -> String {
    let quoted = patterns
        .iter()
        .map(|pattern| format!("\"{pattern}\""))
        .collect::<Vec<_>>()
        .join(",");
    format!("[{quoted}]")
}

/// Render a layer as the `DDLINT_*` variables an environment source holds.
fn render_environment(layer: &Layer) -> MapEnv {
    let mut environment = empty_environment();
    if let Some(format) = layer.format {
        environment = environment.with_var("DDLINT_CMDS__LINT__FORMAT", format_spelling(format));
    }
    if let Some(patterns) = &layer.ignore_patterns {
        environment = environment.with_var(
            "DDLINT_CMDS__LINT__IGNORE_PATTERNS",
            render_json_array(patterns),
        );
    }
    for (name, level) in &layer.rules {
        environment = environment.with_var(
            format!("DDLINT_CMDS__LINT__RULES__{name}"),
            level_spelling(*level),
        );
    }
    environment
}

/// Return the documented default ignore patterns.
fn default_ignore_patterns() -> Vec<String> {
    [".git/", "build/", "target/"].map(str::to_owned).to_vec()
}

/// Resolve the settings that the documented precedence rules require.
fn expected_settings(
    file: &Layer,
    environment: &Layer,
    cli: &LintCliArgs,
) -> (OutputFormat, Vec<String>, BTreeMap<String, RuleLevel>) {
    let format = cli
        .format
        .or(environment.format)
        .or(file.format)
        .unwrap_or_default();
    let ignore_patterns = if cli.no_ignore {
        Vec::new()
    } else {
        environment
            .ignore_patterns
            .clone()
            .or_else(|| file.ignore_patterns.clone())
            .unwrap_or_else(default_ignore_patterns)
    };
    let mut rule_levels = file
        .rules
        .iter()
        .map(|(name, level)| ((*name).to_owned(), *level))
        .collect::<BTreeMap<_, _>>();
    rule_levels.extend(
        environment
            .rules
            .iter()
            .map(|(name, level)| ((*name).to_owned(), *level)),
    );
    (format, ignore_patterns, rule_levels)
}

proptest! {
    #![proptest_config(ProptestConfig::with_cases(64))]

    /// For every field the highest present source wins; lists replace.
    #[test]
    fn highest_present_source_wins_and_lists_replace(
        file in layer_strategy(),
        environment in layer_strategy(),
        cli_format in prop::option::of(format_strategy()),
        no_ignore in any::<bool>(),
    ) {
        let config_file = ConfigFile::new(&render_file(&file))
            .expect("generated configuration file is created");
        let mut cli = no_cli_overrides();
        cli.format = cli_format;
        cli.no_ignore = no_ignore;
        let settings = load(
            Some(config_file.path()),
            render_environment(&environment),
            &cli,
        )
        .expect("generated configuration loads");
        let (format, ignore_patterns, rule_levels) =
            expected_settings(&file, &environment, &cli);

        prop_assert_eq!(settings.format, format);
        prop_assert_eq!(settings.ignore_patterns, ignore_patterns);
        prop_assert_eq!(settings.rule_levels, rule_levels);
    }
}
