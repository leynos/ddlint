//! Behavioural tests for the shipped `unused-relation` lint rule.

use rstest::rstest;

#[path = "support/unused_relation.rs"]
mod support;
use support::run_unused_relation_rule;

fn run_rule(source: &str) -> Vec<ddlint::linter::LintDiagnostic> {
    run_unused_relation_rule(source)
}

#[rstest]
#[case(
    concat!(
        "input relation Source(x: u32)\n",
        "relation Sink(x: u32)\n",
        "Sink(x) :- Source(x).\n",
    ),
    vec!["relation `Sink` is declared but never read from"],
)]
#[case(
    concat!(
        "input relation Source(x: u32)\n",
        "relation Used(x: u32)\n",
        "relation Sink(x: u32)\n",
        "Used(x) :- Source(x).\n",
        "Sink(x) :- Used(x).\n",
    ),
    vec!["relation `Sink` is declared but never read from"],
)]
#[case(
    concat!(
        "input relation Source(x: u32)\n",
        "relation HeadOnly(x: u32)\n",
        "HeadOnly(x) :- Source(x).\n",
    ),
    vec!["relation `HeadOnly` is declared but never read from"],
)]
#[case(
    concat!(
        "relation Declared(x: u32)\n",
        "relation Sink(x: u32)\n",
        "Sink(x) :- Missing(x).\n",
    ),
    vec![
        "relation `Declared` is declared but never read from",
        "relation `Sink` is declared but never read from",
    ],
)]
#[case(
    concat!(
        "relation Zebra(x: u32)\n",
        "relation Alpha(x: u32)\n",
        "relation Middle(x: u32)\n",
    ),
    vec![
        "relation `Zebra` is declared but never read from",
        "relation `Alpha` is declared but never read from",
        "relation `Middle` is declared but never read from",
    ],
)]
#[case(
    concat!(
        "input relation Source(x: u32)\n",
        "relation ForRead(x: u32)\n",
        "relation Sink(x: u32)\n",
        "ForRead(x) :- Source(x).\n",
        "Sink(x) :- for (y in ForRead(x)) Inner(y).\n",
    ),
    vec!["relation `Sink` is declared but never read from"],
)]
#[case(
    concat!(
        "input relation Source(x: u32)\n",
        "relation GuardRead(x: u32)\n",
        "relation Sink(x: u32)\n",
        "GuardRead(x) :- Source(x).\n",
        "Sink(x) :- for (y in Source(x) if GuardRead(y)) Inner(y).\n",
    ),
    vec!["relation `Sink` is declared but never read from"],
)]
fn unused_relation_rule_matches_expected_messages(
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
            .all(|diagnostic| diagnostic.rule_name() == "unused-relation"),
        "all diagnostics should come from the shipped rule",
    );
}
