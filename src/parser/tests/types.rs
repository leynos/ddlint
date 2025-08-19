//! Type definition parsing tests.
//!
//! Exercises typedef and extern type handling.

use super::common::pretty_print;
use crate::parse;
use crate::parser::ast::{AstNode, TypeDef};
use rstest::rstest;

#[rstest]
fn standard_typedef() {
    let src = "typedef Uuid = string\n";
    let parsed = parse(src);
    assert!(parsed.errors().is_empty());
    let defs = parsed.root().type_defs();
    assert_eq!(defs.len(), 1);
    let def = defs.first().unwrap_or_else(|| panic!("typedef not found"));
    assert_eq!(def.name(), Some("Uuid".into()));
    assert!(!def.is_extern());
}

#[rstest]
fn complex_typedef() {
    let src = "typedef UserRecord = (name: string, age: u64, active: bool)\n";
    let parsed = parse(src);
    assert!(parsed.errors().is_empty());
    let defs = parsed.root().type_defs();
    assert_eq!(defs.len(), 1);
    let def = defs.first().unwrap_or_else(|| panic!("typedef not found"));
    assert_eq!(def.name(), Some("UserRecord".into()));
    assert!(!def.is_extern());
}

#[rstest]
fn extern_type() {
    let src = "extern type FfiHandle\n";
    let parsed = parse(src);
    assert!(parsed.errors().is_empty());
    let defs = parsed.root().type_defs();
    assert_eq!(defs.len(), 1);
    let def = defs.first().unwrap_or_else(|| panic!("typedef not found"));
    assert_eq!(def.name(), Some("FfiHandle".into()));
    assert!(def.is_extern());
}

#[rstest]
fn extern_without_type_is_ignored() {
    let src = "extern foo\nextern type Bar";
    let parsed = parse(src);
    let defs = parsed.root().type_defs();
    assert_eq!(defs.len(), 1);
    assert_eq!(
        defs.first().and_then(TypeDef::name),
        Some("Bar".to_string())
    );
}

#[rstest]
#[case("typedef Uuid = string\n", "Uuid", false, "typedef Uuid = string\n")]
#[case("typedef Foo=bar\n", "Foo", false, "typedef Foo=bar\n")]
#[case(
    "typedef Record = (name: string, active: bool)\n",
    "Record",
    false,
    "typedef Record = (name: string, active: bool)\n"
)]
#[case("extern type Handle\n", "Handle", true, "extern type Handle\n")]
#[case("extern type  Extra  \n", "Extra", true, "extern type  Extra  \n")]
fn typedef_variations(
    #[case] src: &str,
    #[case] expected: &str,
    #[case] is_extern: bool,
    #[case] expected_text: &str,
) {
    let parsed = parse(src);
    assert!(parsed.errors().is_empty());
    let defs = parsed.root().type_defs();
    assert_eq!(defs.len(), 1);
    let def = defs
        .first()
        .unwrap_or_else(|| panic!("typedef should exist for valid source"));
    assert_eq!(def.name(), Some(expected.to_string()));
    assert_eq!(def.is_extern(), is_extern);
    let text = pretty_print(def.syntax());
    assert_eq!(text, expected_text);
}

#[rstest]
fn typedef_nesting_and_whitespace() {
    let src = "typedef Foo=string\ntypedef   Bar = u64  \n";
    let parsed = parse(src);
    assert!(parsed.errors().is_empty());
    assert_eq!(pretty_print(parsed.root().syntax()), src);
    let defs = parsed.root().type_defs();
    assert_eq!(defs.len(), 2);
    let first = defs
        .first()
        .unwrap_or_else(|| panic!("first typedef missing"));
    let second = defs
        .get(1)
        .unwrap_or_else(|| panic!("second typedef missing"));
    assert_eq!(pretty_print(first.syntax()), "typedef Foo=string\n");
    assert_eq!(pretty_print(second.syntax()), "typedef   Bar = u64  \n");
    let first_end = first.syntax().text_range().end();
    let second_start = second.syntax().text_range().start();
    assert!(first_end <= second_start);
}

#[rstest]
fn typedef_missing_name_returns_none() {
    let src = "typedef = string\n";
    let parsed = parse(src);
    assert!(parsed.errors().is_empty());
    let defs = parsed.root().type_defs();
    assert_eq!(defs.len(), 1);
    let def = defs
        .first()
        .unwrap_or_else(|| panic!("typedef span exists"));
    assert_eq!(def.name(), None);
}
