//! Rule head adornments, locations, and reference lowering.

use ddlint::parse;

#[test]
fn parses_multi_head_rules_with_adornments_and_ref_new_lowering() {
    let src = "A(a)@site(x) -<10>, B'(b), &RefOrder{ id: id, amt: amt } :- Src(a, b, id, amt).";
    let parsed = parse(src);
    assert!(
        parsed.errors().is_empty(),
        "unexpected parse errors: {:?}",
        parsed.errors()
    );
    let rule = parsed
        .root()
        .rules()
        .first()
        .cloned()
        .expect("rule missing");

    let heads = match rule.heads() {
        Ok(heads) => heads,
        Err(errs) => panic!("heads should parse: {errs:?}"),
    };
    assert_eq!(heads.len(), 3);

    let Some(first) = heads.first() else {
        panic!("missing first head");
    };
    assert_eq!(
        first.atom.to_sexpr(),
        "(delay 10 (call A a))",
        "first head should include delay"
    );
    let Some(location) = first.location.as_ref() else {
        panic!("location missing");
    };
    assert_eq!(location.to_sexpr(), "(call site x)");

    let Some(second) = heads.get(1) else {
        panic!("missing second head");
    };
    assert_eq!(second.atom.to_sexpr(), "(diff (call B b))");

    let Some(third) = heads.get(2) else {
        panic!("missing third head");
    };
    assert_eq!(
        third.atom.to_sexpr(),
        "(call ref_new (struct RefOrder (id id) (amt amt)))"
    );
}
