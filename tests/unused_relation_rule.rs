//! Behavioural tests for the shipped `unused-relation` lint rule.

use ddlint::linter::rules::correctness::UnusedRelationRule;
use ddlint::linter::{CstRuleStore, RuleConfig, Runner};
use ddlint::parse;
use rstest::rstest;

fn run_rule(source: &str) -> Vec<ddlint::linter::LintDiagnostic> {
    let parsed = parse(source);
    assert!(
        parsed.errors().is_empty(),
        "unused-relation behavioural source should parse cleanly: {:?}",
        parsed.errors()
    );

    let mut store = CstRuleStore::new();
    store.register(Box::new(UnusedRelationRule));
    Runner::new(&store, source, &parsed, RuleConfig::new()).run()
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
