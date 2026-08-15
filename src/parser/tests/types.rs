//! Type definition parsing tests.
//!
//! Exercises type and extern type handling.

use super::helpers::pretty_print;
use crate::parse;
use crate::parser::ast::{AstNode, TypeDef};
use crate::test_util::assert_no_parse_errors;
use rstest::rstest;

#[rstest]
fn standard_type_definition() {
    let src = "type Uuid = string\n";
    let parsed = parse(src);
    assert_no_parse_errors(parsed.errors());
    let defs = parsed.root().type_defs();
    assert_eq!(defs.len(), 1);
    #[expect(clippy::expect_used, reason = "tests require a type")]
    let def = defs.first().expect("type not found");
    assert_eq!(def.name().as_deref(), Some("Uuid"));
    assert!(!def.is_extern());
}

#[rstest]
fn complex_type_definition() {
    let src = "type UserRecord = (name: string, age: u64, active: bool)\n";
    let parsed = parse(src);
    assert_no_parse_errors(parsed.errors());
    let defs = parsed.root().type_defs();
    assert_eq!(defs.len(), 1);
    #[expect(clippy::expect_used, reason = "tests require a type")]
    let def = defs.first().expect("type not found");
    assert_eq!(def.name().as_deref(), Some("UserRecord"));
    assert!(!def.is_extern());
}

#[rstest]
fn extern_type() {
    let src = "extern type FfiHandle\n";
    let parsed = parse(src);
    assert_no_parse_errors(parsed.errors());
    let defs = parsed.root().type_defs();
    assert_eq!(defs.len(), 1);
    #[expect(clippy::expect_used, reason = "tests require a type")]
    let def = defs.first().expect("type not found");
    assert_eq!(def.name().as_deref(), Some("FfiHandle"));
    assert!(def.is_extern());
}

#[rstest]
fn extern_without_type_is_ignored() {
    let src = "extern foo\nextern type Bar";
    let parsed = parse(src);
    let defs = parsed.root().type_defs();
    assert_eq!(defs.len(), 1);
    assert_eq!(defs.first().and_then(TypeDef::name).as_deref(), Some("Bar"));
}

#[rstest]
#[case("type Uuid = string\n", "Uuid", false, "type Uuid = string\n")]
#[case("type Foo=bar\n", "Foo", false, "type Foo=bar\n")]
#[case(
    "type Record = (name: string, active: bool)\n",
    "Record",
    false,
    "type Record = (name: string, active: bool)\n"
)]
#[case("extern type Handle\n", "Handle", true, "extern type Handle\n")]
#[case("extern type  Extra  \n", "Extra", true, "extern type  Extra  \n")]
fn type_definition_variations(
    #[case] src: &str,
    #[case] expected: &str,
    #[case] is_extern: bool,
    #[case] expected_text: &str,
) {
    let parsed = parse(src);
    assert_no_parse_errors(parsed.errors());
    let defs = parsed.root().type_defs();
    assert_eq!(defs.len(), 1);
    #[expect(clippy::expect_used, reason = "tests require a type")]
    let def = defs.first().expect("type should exist for valid source");
    assert_eq!(def.name().as_deref(), Some(expected));
    assert_eq!(def.is_extern(), is_extern);
    let text = pretty_print(def.syntax());
    assert_eq!(text, expected_text);
}

#[rstest]
fn type_definition_nesting_and_whitespace() {
    let src = "type Foo=string\ntype   Bar = u64  \n";
    let parsed = parse(src);
    assert_no_parse_errors(parsed.errors());
    assert_eq!(pretty_print(parsed.root().syntax()), src);
    let defs = parsed.root().type_defs();
    assert_eq!(defs.len(), 2);
    #[expect(
        clippy::expect_used,
        reason = "tests require both type definitions to exist"
    )]
    let first = defs.first().expect("first type missing");
    #[expect(
        clippy::expect_used,
        reason = "tests require both type definitions to exist"
    )]
    let second = defs.get(1).expect("second type missing");
    assert_eq!(pretty_print(first.syntax()), "type Foo=string\n");
    assert_eq!(pretty_print(second.syntax()), "type   Bar = u64  \n");
    let first_end = first.syntax().text_range().end();
    let second_start = second.syntax().text_range().start();
    assert!(first_end <= second_start);
}

#[rstest]
fn type_definition_missing_name_returns_none() {
    let src = "type = string\n";
    let parsed = parse(src);
    assert_no_parse_errors(parsed.errors());
    let defs = parsed.root().type_defs();
    assert_eq!(defs.len(), 1);
    #[expect(clippy::expect_used, reason = "tests require a type span")]
    let def = defs.first().expect("type span exists");
    assert_eq!(def.name(), None);
}
