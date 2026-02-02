//! Behavioural tests for `apply` statements.

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
    assert_eq!(parsed.root().applys().len(), 1);
}
