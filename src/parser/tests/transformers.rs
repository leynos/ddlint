//! Transformer declaration parsing tests.
//!
//! Validates extern transformer syntax and edge cases.

use super::common::parse_transformer;
use crate::test_util::{ErrorPattern, assert_no_parse_errors, assert_parse_error};
use rstest::{fixture, rstest};

#[fixture]
fn transformer_single_io() -> &'static str {
    "extern transformer normalize(input: UnnormalizedData): NormalizedData"
}

#[fixture]
fn transformer_multi_io() -> &'static str {
    "extern transformer correlate(users: User, sessions: UserSession): UserActivity, SessionAlerts"
}

#[fixture]
fn transformer_invalid() -> &'static str {
    "extern transformer incomplete_transformer(input: SomeData):"
}

#[fixture]
fn transformer_no_inputs() -> &'static str {
    "extern transformer no_inputs(): OutputType"
}

#[fixture]
fn transformer_no_outputs() -> &'static str {
    "extern transformer no_outputs(input: InputType):"
}

#[fixture]
fn transformer_extra_ws() -> &'static str {
    " extern   transformer   spaced  (  foo  :  Bar  ,  baz : Qux )  :  Out1 , Out2 "
}

#[fixture]
fn transformer_dup_inputs() -> &'static str {
    "extern transformer dup_inputs(foo: Bar, foo: Baz): Out"
}

#[fixture]
fn transformer_reserved_names() -> &'static str {
    "extern transformer reserved(transformer: Type, extern: Type): out"
}

#[rstest]
#[case(transformer_single_io(), "normalize", vec![("input", "UnnormalizedData")], vec!["NormalizedData".into()])]
#[case(
    transformer_multi_io(),
    "correlate",
    vec![("users", "User"), ("sessions", "UserSession")],
    vec!["UserActivity".into(), "SessionAlerts".into()],
)]
fn parses_transformers(
    #[case] src: &str,
    #[case] name: &str,
    #[case] inputs: Vec<(&str, &str)>,
    #[case] outputs: Vec<String>,
) {
    let t = parse_transformer(src);
    assert_eq!(t.name().as_deref(), Some(name));
    let in_expected: Vec<(String, String)> = inputs
        .into_iter()
        .map(|(n, ty)| (n.into(), ty.into()))
        .collect();
    assert_eq!(t.inputs(), in_expected);
    assert_eq!(t.outputs(), outputs);
}

#[rstest]
#[case::no_outputs(transformer_no_outputs(), ErrorPattern::from("Unexpected"), 0, 48)]
#[case::invalid_decl(transformer_invalid(), ErrorPattern::from("Unexpected"), 0, 59)]
fn transformer_error_cases(
    #[case] src: &str,
    #[case] pattern: ErrorPattern,
    #[case] start: usize,
    #[case] end: usize,
) {
    let parsed = crate::parse(src);
    let errors = parsed.errors();
    assert_parse_error(errors, pattern, start, end);
    assert!(parsed.root().transformers().is_empty());
}

#[rstest]
fn transformer_no_inputs_parsed(transformer_no_inputs: &str) {
    let t = parse_transformer(transformer_no_inputs);
    assert_eq!(t.inputs(), Vec::<(String, String)>::new());
    assert_eq!(t.outputs(), vec![String::from("OutputType")]);
}

#[rstest]
fn transformer_extra_whitespace_parsed(transformer_extra_ws: &str) {
    let t = parse_transformer(transformer_extra_ws);
    assert_eq!(t.name().as_deref(), Some("spaced"));
    assert_eq!(
        t.inputs(),
        vec![("foo".into(), "Bar".into()), ("baz".into(), "Qux".into())]
    );
    assert_eq!(t.outputs(), vec!["Out1".to_string(), "Out2".to_string()]);
}

#[rstest]
#[expect(clippy::expect_used, reason = "Using expect for clearer test failures")]
fn transformer_duplicate_input_names(transformer_dup_inputs: &str) {
    let parsed = crate::parse(transformer_dup_inputs);
    assert_no_parse_errors(parsed.errors());
    let transformers = parsed.root().transformers();
    let t = transformers.first().expect("transformer missing");
    let inputs = t.inputs();
    let foo_count = inputs.iter().filter(|(n, _)| n == "foo").count();
    assert_eq!(foo_count, 2);
}

#[rstest]
fn transformer_reserved_keyword_names(transformer_reserved_names: &str) {
    let t = parse_transformer(transformer_reserved_names);
    let names: Vec<_> = t.inputs().into_iter().map(|(n, _)| n).collect();
    assert_eq!(names, vec!["transformer", "extern"]);
}
