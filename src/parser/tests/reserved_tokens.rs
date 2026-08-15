//! Reserved-token policy tests.
//!
//! These tests pin the parser-facing diagnostics for legacy `DDlog` tokens whose
//! lexer kinds remain available only so recovery can report precise spans.

use chumsky::error::{Simple, SimpleReason};
use rstest::rstest;

use crate::SyntaxKind;
use crate::parser::expression::parse_expression;
use crate::parser::reserved_tokens::{
    RESERVED_BARE_HASH_ERROR, RESERVED_BIGINT_ERROR, RESERVED_BIT_ERROR, RESERVED_DOUBLE_ERROR,
    RESERVED_FLOAT_ERROR, RESERVED_SIGNED_ERROR, RESERVED_SPACESHIP_ERROR, RESERVED_TYPEDEF_ERROR,
};
use crate::test_util::{assert_custom_parse_error_contains, assert_no_parse_errors};

use super::helpers::{count_nodes_by_kind, parse_err, parse_ok};

#[test]
fn typedef_keyword_is_rejected_without_type_def_ast() {
    let parsed = parse_err("typedef Foo = u32\n");

    assert!(parsed.root().type_defs().is_empty());
    assert_custom_parse_error_contains(parsed.errors(), RESERVED_TYPEDEF_ERROR);
}

#[test]
fn modern_type_keyword_still_parses_type_definitions() {
    let parsed = parse_ok("type Foo = u32\n");

    let defs = parsed.root().type_defs();
    assert_eq!(defs.len(), 1);
    assert_eq!(
        defs.first().and_then(crate::ast::TypeDef::name).as_deref(),
        Some("Foo")
    );
}

#[rstest]
#[case::bigint("type Foo = bigint;\n", RESERVED_BIGINT_ERROR)]
#[case::bit("type Foo = bit<32>;\n", RESERVED_BIT_ERROR)]
#[case::double("type Foo = double;\n", RESERVED_DOUBLE_ERROR)]
#[case::float("type Foo = float;\n", RESERVED_FLOAT_ERROR)]
#[case::signed("type Foo = signed<32>;\n", RESERVED_SIGNED_ERROR)]
fn legacy_type_names_are_rejected_in_type_position(#[case] src: &str, #[case] expected: &str) {
    let parsed = parse_err(src);

    assert_custom_parse_error_contains(parsed.errors(), expected);
}

#[rstest]
#[case::bigint("bigint", RESERVED_BIGINT_ERROR)]
#[case::bit("bit", RESERVED_BIT_ERROR)]
#[case::double("double", RESERVED_DOUBLE_ERROR)]
#[case::float("float", RESERVED_FLOAT_ERROR)]
#[case::signed("signed", RESERVED_SIGNED_ERROR)]
fn legacy_type_names_are_rejected_outwith_type_position(
    #[case] legacy_type: &str,
    #[case] expected: &str,
) {
    let source = format!("Output(x) :- Source(x), var {legacy_type} = x.\n");
    let parsed = parse_err(source);

    assert_custom_parse_error_contains(parsed.errors(), expected);
}

#[test]
fn spaceship_operator_is_rejected_in_expression_context() {
    let Err(errors) = parse_expression("1 <=> 2") else {
        panic!("spaceship expression should be rejected");
    };

    assert_custom_parse_error_contains(&errors, RESERVED_SPACESHIP_ERROR);
}

#[rstest]
#[case("# foo\n")]
#[case("# [attribute]\ntype Foo = u32\n")]
#[case("#/*comment*/[attribute]\ntype Foo = u32\n")]
fn bare_hash_is_rejected(#[case] source: &str) {
    let bare = parse_err(source);
    assert_custom_parse_error_contains(bare.errors(), RESERVED_BARE_HASH_ERROR);
}

#[test]
fn attribute_hash_is_preserved() {
    let attributed = parse_ok("#[attribute]\ntype Foo = u32\n");
    assert_eq!(
        count_nodes_by_kind(attributed.root().syntax(), SyntaxKind::N_ATTRIBUTE),
        1
    );
    assert_eq!(attributed.root().type_defs().len(), 1);
}

#[test]
fn emitted_reserved_errors_are_not_duplicated() {
    let parsed = parse_err("Output(x) :- Source(x), bigint.\n");

    assert_eq!(
        count_custom_parse_errors(parsed.errors(), RESERVED_BIGINT_ERROR),
        1,
    );
}

#[test]
fn spaceship_in_assignment_pattern_is_rejected() {
    let parsed = parse_err("Output(x) :- x <=> y = z.\n");

    assert_eq!(
        count_custom_parse_errors(parsed.errors(), RESERVED_SPACESHIP_ERROR),
        1,
    );
}

#[test]
fn many_reserved_operators_report_once_each() {
    const EXPRESSION_COUNT: usize = 128;
    let body = (0..EXPRESSION_COUNT)
        .map(|index| format!("value_{index} <=> value_{index}"))
        .collect::<Vec<_>>()
        .join(", ");
    let source = format!("Output(x) :- {body}.\n");
    let parsed = parse_err(source);

    assert_eq!(
        count_custom_parse_errors(parsed.errors(), RESERVED_SPACESHIP_ERROR),
        EXPRESSION_COUNT,
    );
}

fn count_custom_parse_errors(errors: &[Simple<SyntaxKind>], expected: &str) -> usize {
    errors
        .iter()
        .filter(
            |error| matches!(error.reason(), SimpleReason::Custom(message) if message == expected),
        )
        .count()
}

#[test]
fn import_alias_keyword_is_preserved() {
    let parsed = parse_ok("import foo::bar as baz\n");
    let imports = parsed.root().imports();

    assert_eq!(imports.len(), 1);
    assert_eq!(
        imports.first().map(crate::ast::Import::path).as_deref(),
        Some("foo::bar")
    );
    assert_eq!(
        imports
            .first()
            .and_then(crate::ast::Import::alias)
            .as_deref(),
        Some("baz")
    );
    assert_no_parse_errors(parsed.errors());
}
