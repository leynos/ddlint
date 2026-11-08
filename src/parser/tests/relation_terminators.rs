//! Differential tests for relation statement boundaries.
//!
//! A `;` terminates a relation declaration, so a declaration that follows it
//! must be scanned whether it starts on the same line or the next. Binder
//! lists must also be comma-separated identifiers; any other content is a
//! malformed primary key (D-REL-007).

use chumsky::error::SimpleReason;
use rstest::rstest;

use crate::{parse, test_util::assert_no_parse_errors};

/// Return the names of the relations scanned from `src`.
fn relation_names(src: &str) -> Vec<String> {
    parse(src)
        .root()
        .relations()
        .into_iter()
        .filter_map(|relation| relation.name())
        .collect()
}

/// Return the custom diagnostic messages reported for `src`.
fn custom_error_messages(src: &str) -> Vec<String> {
    parse(src)
        .errors()
        .iter()
        .filter_map(|error| match error.reason() {
            SimpleReason::Custom(message) => Some(message.clone()),
            _ => None,
        })
        .collect()
}

#[rstest]
#[case(
    "input relation R(x: u32) primary key (x);",
    "input relation R(x: u32);"
)]
#[case(
    "input relation R(x: R) primary key (x) x.id;",
    "input relation R(x: R);"
)]
fn keyed_semicolon_matches_keyless(#[case] keyed: &str, #[case] keyless: &str) {
    let keyed_parsed = parse(keyed);
    let keyless_parsed = parse(keyless);
    assert_no_parse_errors(keyed_parsed.errors());
    assert_no_parse_errors(keyless_parsed.errors());
    assert_eq!(relation_names(keyed), relation_names(keyless));
    assert_eq!(relation_names(keyed), vec!["R".to_owned()]);
}

#[test]
fn statement_terminator_does_not_hide_next_declaration() {
    let keyed = "input relation R(x: u32) primary key (x); output relation S(x: u32)";
    let keyless = "input relation R(x: u32); output relation S(x: u32)";

    let keyed_parsed = parse(keyed);
    let keyless_parsed = parse(keyless);
    assert_no_parse_errors(keyed_parsed.errors());
    assert_no_parse_errors(keyless_parsed.errors());
    assert_eq!(relation_names(keyed), relation_names(keyless));
    assert_eq!(relation_names(keyed), vec!["R".to_owned(), "S".to_owned()]);
}

#[rstest]
#[case("input relation R(x: u32) primary key (x.y)")]
#[case("input relation R(x: u32) primary key (1)")]
#[case("input relation R(x: u32) primary key (,)")]
#[case("input relation R(x: u32) primary key (x,)")]
#[case("input relation R(x: u32) primary key (x y)")]
fn non_identifier_binders_are_rejected(#[case] src: &str) {
    let parsed = parse(src);
    let messages = custom_error_messages(src);
    let malformed = messages
        .iter()
        .filter(|message| message.contains("D-REL-007"))
        .count();

    assert_eq!(malformed, 1, "expected one D-REL-007, got {messages:?}");
    assert!(
        parsed.root().relations().is_empty(),
        "a malformed key must not yield a relation"
    );
}
