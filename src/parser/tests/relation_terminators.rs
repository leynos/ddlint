//! Differential tests for relation statement boundaries.
//!
//! A `;` terminates a relation declaration, so a declaration that follows it
//! must be scanned whether it starts on the same line or the next. Binder
//! lists must also be comma-separated identifiers; any other content is a
//! malformed primary key (D-REL-007).

use chumsky::error::SimpleReason;
use rstest::rstest;

use super::helpers::{first_relation_end, structure_after};
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

/// Pins that a trailing `;` ends the declaration whether the key is a binder
/// list or a spec expression, and that both forms scan like their keyless twin.
/// A `;` after a key clause terminates the relation like a keyless `;`.
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

/// Guards against the scanner swallowing a same-line successor: the keyed
/// source must yield both `R` and `S`, just as its keyless twin does.
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

/// A keyed declaration must own the same trailing trivia as its keyless twin.
///
/// `INV-TERM` adjusts spans for the removed clause: the relation span must
/// shrink by exactly the clause length, and the CST after the relation must be
/// identical. A keyed relation that stops at its binder list leaves inline
/// trivia outside the relation while the keyless twin absorbs it, which this
/// comparison detects.
#[rstest]
#[case(
    "input relation R(x: u32) primary key (x)  ;",
    "input relation R(x: u32)  ;"
)]
#[case(
    "input relation R(x: u32) primary key (x) /* note */ ;\noutput relation S(x: u32)",
    "input relation R(x: u32) /* note */ ;\noutput relation S(x: u32)"
)]
#[case(
    "input relation R(x: u32) primary key (a, b)\n  ;\ninput relation T(y: u32)",
    "input relation R(x: u32)\n  ;\ninput relation T(y: u32)"
)]
fn keyed_relation_owns_the_same_trivia_as_its_keyless_twin(
    #[case] keyed: &str,
    #[case] keyless: &str,
) {
    let keyed_parsed = parse(keyed);
    let keyless_parsed = parse(keyless);
    assert_no_parse_errors(keyed_parsed.errors());
    assert_no_parse_errors(keyless_parsed.errors());
    assert_eq!(relation_names(keyed), relation_names(keyless));

    let removed =
        u32::try_from(keyed.len() - keyless.len()).expect("test sources fit the u32 offset range");
    let keyed_end = first_relation_end(&keyed_parsed).expect("keyed source has a relation");
    let keyless_end = first_relation_end(&keyless_parsed).expect("keyless source has a relation");
    assert_eq!(keyed_end, keyless_end + removed);
    assert_eq!(
        structure_after(&keyed_parsed, keyed_end),
        structure_after(&keyless_parsed, keyless_end)
    );
}

/// Checks that binder content other than comma-separated identifiers (paths,
/// literals, empty entries, space separation) reports exactly one D-REL-007
/// diagnostic and exposes no relation.
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
