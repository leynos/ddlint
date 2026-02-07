//! Apply statement parsing tests.

use super::helpers::parse_apply;
use super::helpers::{parse_err, parse_ok};
use rstest::{fixture, rstest};

#[fixture]
fn apply_single() -> &'static str {
    "apply normalise(User) -> (Normalised)"
}

#[fixture]
fn apply_multi() -> &'static str {
    "apply pipeline(User, enrich) -> (Out1, Out2)"
}

#[fixture]
fn apply_empty_lists() -> &'static str {
    "apply empty() -> ()"
}

#[rstest]
#[case(apply_single(), "normalise", vec!["User"], vec!["Normalised"])]
#[case(
    apply_multi(),
    "pipeline",
    vec!["User", "enrich"],
    vec!["Out1", "Out2"]
)]
#[case(apply_empty_lists(), "empty", vec![], vec![])]
fn parses_apply_statements(
    #[case] src: &str,
    #[case] name: &str,
    #[case] inputs: Vec<&str>,
    #[case] outputs: Vec<&str>,
) {
    let apply = parse_apply(src);
    assert_eq!(apply.transformer_name().as_deref(), Some(name));
    let inputs_expected: Vec<String> = inputs.into_iter().map(str::to_string).collect();
    let outputs_expected: Vec<String> = outputs.into_iter().map(str::to_string).collect();
    assert_eq!(apply.inputs(), inputs_expected);
    assert_eq!(apply.outputs(), outputs_expected);
}

#[rstest]
#[case("apply t(x) (y)")]
#[case("apply t(x) -> y")]
#[case("apply t x -> (y)")]
fn apply_errors(#[case] src: &str) {
    let parsed = parse_err(src);
    assert!(parsed.root().applys().is_empty());
}

#[rstest]
fn apply_round_trip(apply_single: &str) {
    let parsed = parse_ok(apply_single);
    assert_eq!(parsed.root().applys().len(), 1);
}
