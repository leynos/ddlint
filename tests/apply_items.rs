//! Behavioural tests for `apply` statements.

use chumsky::error::SimpleReason;
use ddlint::SyntaxKind;
use ddlint::parse;
use rstest::rstest;

#[test]
fn parses_apply_items_in_program() {
    let src = concat!(
        "extern transformer normalise(input: User): Normalised\n",
        "apply normalise(User) -> (Normalised)\n",
    );
    let parsed = parse(src);
    assert!(
        parsed.errors().is_empty(),
        "unexpected parse errors: {:?}",
        parsed.errors()
    );
    let applys = parsed.root().applys();
    assert_eq!(applys.len(), 1);
    let Some(apply) = applys.first() else {
        panic!("apply missing");
    };
    assert_eq!(apply.transformer_name().as_deref(), Some("normalise"));
    assert_eq!(apply.inputs(), vec!["User".to_string()]);
    assert_eq!(apply.outputs(), vec!["Normalised".to_string()]);
}

fn assert_missing_colon_error(
    errors: &[chumsky::error::Simple<SyntaxKind>],
    expected_found: Option<SyntaxKind>,
) {
    let [error] = errors else {
        panic!("expected one parse error, got {errors:?}");
    };
    assert!(matches!(error.reason(), SimpleReason::Unexpected));
    assert!(
        error
            .expected()
            .filter_map(|expected| expected.as_ref())
            .any(|kind| *kind == SyntaxKind::T_COLON),
        "expected missing-colon error, got {error:?}",
    );
    assert_eq!(error.found().copied(), expected_found);
}

#[rstest]
#[case("extern transformer missing_colon(input: SomeData)", None, Vec::new())]
#[case(
    concat!(
        "extern transformer missing_colon(input: SomeData)\n",
        "extern transformer ok(input: SomeData): OtherData\n",
        "extern transformer another(input: OtherData): FinalData",
    ),
    Some(SyntaxKind::K_EXTERN),
    vec![Some(String::from("ok")), Some(String::from("another"))],
)]
fn transformer_missing_colon_behaviour(
    #[case] src: &str,
    #[case] expected_found: Option<SyntaxKind>,
    #[case] expected_names: Vec<Option<String>>,
) {
    let parsed = parse(src);
    assert_missing_colon_error(parsed.errors(), expected_found);

    let transformers = parsed.root().transformers();
    let names: Vec<_> = transformers
        .iter()
        .map(ddlint::ast::Transformer::name)
        .collect();
    assert_eq!(names, expected_names);
}
