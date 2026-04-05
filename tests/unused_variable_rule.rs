//! Behavioural tests for the shipped `unused-variable` lint rule.

use rstest::rstest;

#[path = "support/unused_variable.rs"]
mod support;
use support::run_unused_variable_rule;

fn run_rule(source: &str) -> Vec<ddlint::linter::LintDiagnostic> {
    run_unused_variable_rule(source)
}

#[rstest]
#[case(
    "Output(head_x) :- Source(_).",
    vec!["variable `head_x` is defined but never used in this rule"],
)]
#[case("Output(head_x) :- Source(head_x).", Vec::new())]
#[case(
    "Output(result) :- Source(result), var assigned_x = Seed(result), Use(result).",
    vec!["variable `assigned_x` is defined but never used in this rule"],
)]
#[case(
    "Output(result) :- Source(result), var assigned_x = Seed(result), Use(assigned_x).",
    Vec::new()
)]
#[case(
    "Output(result) :- Source(result), for (item_y in Items(result)) Inner(result).",
    vec!["variable `item_y` is defined but never used in this rule"],
)]
#[case(
    "Output(result) :- Source(result), for (item_y in Items(result)) Inner(item_y).",
    Vec::new()
)]
#[case("Output(_) :- Source(_).", Vec::new())]
#[case(
    "for (item_y in Source(_)) Output(0).",
    vec!["variable `item_y` is defined but never used in this rule"],
)]
#[case("for (item_y in Source(item_y)) Output(item_y).", Vec::new())]
#[case(
    "Output(x) :- Source(0), var x = Seed(0), Sink(x).",
    vec!["variable `x` is defined but never used in this rule"],
)]
fn unused_variable_rule_matches_expected_messages(
    #[case] source: &str,
    #[case] expected_messages: Vec<&str>,
) {
    let diagnostics = run_rule(source);
    let actual_messages: Vec<_> = diagnostics
        .iter()
        .map(ddlint::linter::LintDiagnostic::message)
        .collect();

    assert_eq!(actual_messages, expected_messages);
    assert!(
        diagnostics
            .iter()
            .all(|diagnostic| diagnostic.rule_name() == "unused-variable"),
        "all diagnostics should come from the shipped rule",
    );
}

#[rstest]
fn unused_variable_rule_reports_multiple_diagnostics_in_symbol_order() {
    let diagnostics = run_rule(
        "Output(head_x) :- Source(0), var assigned_x = Seed(0), for (item_y in Items(0)) Inner(0).",
    );
    let actual_messages: Vec<_> = diagnostics
        .iter()
        .map(ddlint::linter::LintDiagnostic::message)
        .collect();

    assert_eq!(
        actual_messages,
        vec![
            "variable `head_x` is defined but never used in this rule",
            "variable `assigned_x` is defined but never used in this rule",
            "variable `item_y` is defined but never used in this rule",
        ]
    );
    assert!(
        diagnostics
            .iter()
            .all(|diagnostic| diagnostic.rule_name() == "unused-variable"),
        "all diagnostics should come from the shipped rule",
    );
}
