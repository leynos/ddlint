//! Behavioural tests for `apply` statements.

use chumsky::error::SimpleReason;
use ddlint::SyntaxKind;
use ddlint::parse;

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

#[test]
fn transformer_declarations_require_a_colon_before_outputs() {
    let src = "extern transformer missing_colon(input: SomeData)";
    let parsed = parse(src);
    let errors = parsed.errors();

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
    assert_eq!(error.found(), None);
    assert!(parsed.root().transformers().is_empty());
}

#[test]
fn malformed_transformer_does_not_prevent_parsing_subsequent_declarations() {
    let src = concat!(
        "extern transformer missing_colon(input: SomeData)\n",
        "extern transformer ok(input: SomeData): OtherData\n",
        "extern transformer another(input: OtherData): FinalData",
    );
    let parsed = parse(src);
    let errors = parsed.errors();

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
    assert_eq!(error.found(), Some(&SyntaxKind::K_EXTERN));

    let transformers = parsed.root().transformers();
    assert_eq!(transformers.len(), 2);
    let names: Vec<_> = transformers
        .iter()
        .map(ddlint::ast::Transformer::name)
        .collect();
    assert_eq!(names, vec![Some("ok".into()), Some("another".into())]);
}
