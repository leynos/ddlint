//! Behavioural tests for `apply` statements.

use ddlint::parse;
use ddlint::test_util::assert_parse_error;

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

    assert_parse_error(parsed.errors(), "Unexpected", 0, src.len());
    assert!(parsed.root().transformers().is_empty());
}
