//! Unit tests for core lint rule traits and supporting types.

use rowan::TextSize;

use super::{CstRule, LintDiagnostic, Rule, RuleConfig, RuleConfigValue, RuleCtx, RuleLevel};
use crate::{SyntaxKind, parse};

struct ExampleRule;

impl Rule for ExampleRule {
    fn name(&self) -> &'static str {
        "example-rule"
    }

    fn group(&self) -> &'static str {
        "correctness"
    }

    fn docs(&self) -> &'static str {
        "Example rule documentation."
    }
}

impl CstRule for ExampleRule {
    fn target_kinds(&self) -> &'static [SyntaxKind] {
        &[SyntaxKind::N_RULE]
    }
}

struct ErrorLevelRule;

impl Rule for ErrorLevelRule {
    fn name(&self) -> &'static str {
        "error-level-rule"
    }

    fn group(&self) -> &'static str {
        "correctness"
    }

    fn docs(&self) -> &'static str {
        "Rule with explicit error level."
    }

    fn default_level(&self) -> RuleLevel {
        RuleLevel::Error
    }
}

impl CstRule for ErrorLevelRule {
    fn target_kinds(&self) -> &'static [SyntaxKind] {
        &[SyntaxKind::N_RULE]
    }
}

fn assert_send_sync<T: Send + Sync>() {}

fn make_rule_config() -> RuleConfig {
    RuleConfig::from([
        ("enabled".to_owned(), RuleConfigValue::Bool(true)),
        ("max_depth".to_owned(), RuleConfigValue::Integer(2)),
        (
            "style".to_owned(),
            RuleConfigValue::String("strict".to_owned()),
        ),
    ])
}

#[test]
fn trait_object_exposes_name_group_and_docs() {
    let rule: &dyn CstRule = &ExampleRule;
    assert_eq!(rule.name(), "example-rule");
    assert_eq!(rule.group(), "correctness");
    assert_eq!(rule.docs(), "Example rule documentation.");
}

#[test]
fn trait_object_exposes_default_level_and_target_kinds() {
    let rule: &dyn CstRule = &ExampleRule;
    assert_eq!(rule.default_level(), RuleLevel::Warn);
    assert_eq!(rule.target_kinds(), &[SyntaxKind::N_RULE]);
}

#[test]
fn explicit_rule_level_override_is_visible_through_trait_object() {
    let rule: &dyn CstRule = &ErrorLevelRule;
    assert_eq!(rule.default_level(), RuleLevel::Error);
}

#[test]
fn rule_level_as_str_returns_language_spellings() {
    assert_eq!(RuleLevel::Allow.as_str(), "allow");
    assert_eq!(RuleLevel::Hint.as_str(), "hint");
    assert_eq!(RuleLevel::Warn.as_str(), "warn");
}

#[test]
fn rule_level_error_as_str_and_display_agree() {
    assert_eq!(RuleLevel::Error.as_str(), "error");
    assert_eq!(RuleLevel::Warn.to_string(), "warn");
}

#[test]
fn cst_rule_is_send_and_sync() {
    assert_send_sync::<ExampleRule>();
}

#[test]
fn lint_diagnostic_accessors_round_trip() {
    let span = rowan::TextRange::new(TextSize::from(1), TextSize::from(3));
    let diagnostic = LintDiagnostic::new("example-rule", "message", span);

    assert_eq!(diagnostic.rule_name(), "example-rule");
    assert_eq!(diagnostic.message(), "message");
    assert_eq!(diagnostic.span(), span);
}

#[test]
fn rule_config_value_correct_type_accessors_return_inner_value() {
    assert_eq!(RuleConfigValue::Bool(true).as_bool(), Some(true));
    assert_eq!(RuleConfigValue::Integer(3).as_integer(), Some(3));
    assert_eq!(
        RuleConfigValue::String("x".to_owned()).as_string(),
        Some("x")
    );
}

#[test]
fn rule_config_value_wrong_type_accessors_return_none() {
    assert_eq!(RuleConfigValue::Bool(true).as_integer(), None);
    assert_eq!(RuleConfigValue::Integer(3).as_string(), None);
}

#[test]
fn rule_ctx_exposes_source_text_and_ast_roots() {
    let source = "input relation R(x: u32);";
    let parsed = parse(source);
    let ctx = RuleCtx::from_parsed(source, &parsed, make_rule_config());

    assert_eq!(ctx.source_text(), source);
    assert_eq!(ctx.ast_root().relations().len(), 1);
    assert_eq!(ctx.cst_root().kind(), SyntaxKind::N_DATALOG_PROGRAM);
}

#[test]
fn rule_ctx_config_accessor_returns_full_config() {
    let source = "input relation R(x: u32);";
    let parsed = parse(source);
    let config = make_rule_config();
    let ctx = RuleCtx::from_parsed(source, &parsed, config.clone());

    assert_eq!(ctx.config(), &config);
}

#[test]
fn rule_ctx_config_value_and_bool_accessors_return_correct_values() {
    let source = "input relation R(x: u32);";
    let parsed = parse(source);
    let ctx = RuleCtx::from_parsed(source, &parsed, make_rule_config());

    assert_eq!(
        ctx.config_value("enabled"),
        Some(&RuleConfigValue::Bool(true))
    );
    assert_eq!(ctx.config_bool("enabled"), Some(true));
}

#[test]
fn rule_ctx_int_and_string_config_accessors_return_correct_values() {
    let source = "input relation R(x: u32);";
    let parsed = parse(source);
    let ctx = RuleCtx::from_parsed(source, &parsed, make_rule_config());

    assert_eq!(ctx.config_int("max_depth"), Some(2));
    assert_eq!(ctx.config_string("style"), Some("strict"));
}

#[test]
fn rule_ctx_config_accessors_return_none_for_type_mismatch_and_missing_keys() {
    let source = "input relation R(x: u32);";
    let parsed = parse(source);
    let ctx = RuleCtx::from_parsed(source, &parsed, make_rule_config());

    assert_eq!(ctx.config_bool("style"), None);
    assert_eq!(ctx.config_value("missing"), None);
}
