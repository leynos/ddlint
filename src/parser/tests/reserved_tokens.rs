//! Reserved-token policy tests.
//!
//! These tests pin the parser-facing diagnostics for legacy `DDlog` tokens whose
//! lexer kinds remain available only so recovery can report precise spans.

use chumsky::error::{Simple, SimpleReason};
use rstest::rstest;
use std::ops::Range;

use crate::SyntaxKind;
use crate::parser::expression::parse_expression;
use crate::test_util::assert_no_parse_errors;

mod fixtures;

use super::helpers::{SourceText, count_nodes_by_kind, parse_err, parse_ok};
use fixtures::{DeclarationPosition, ExpectedReservedToken, ReservedTokenScenario};

#[test]
fn typedef_keyword_is_rejected_without_type_def_ast() {
    let source = "typedef Foo = u32\n";
    let parsed = parse_err(source);

    assert!(parsed.root().type_defs().is_empty());
    assert_reserved_token_error(
        parsed.errors(),
        ExpectedReservedToken::Typedef,
        token_span(source, ExpectedReservedToken::Typedef),
    );
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
#[case::bigint(ReservedTokenScenario::new("type Foo = bigint;\n", ExpectedReservedToken::Bigint))]
#[case::bit(ReservedTokenScenario::new("type Foo = bit<32>;\n", ExpectedReservedToken::Bit))]
#[case::double(ReservedTokenScenario::new("type Foo = double;\n", ExpectedReservedToken::Double))]
#[case::float(ReservedTokenScenario::new("type Foo = float;\n", ExpectedReservedToken::Float))]
#[case::signed(ReservedTokenScenario::new(
    "type Foo = signed<32>;\n",
    ExpectedReservedToken::Signed
))]
fn legacy_type_names_are_rejected_in_type_position(#[case] scenario: ReservedTokenScenario) {
    let span = token_span(scenario.source.as_ref(), scenario.expected);
    let parsed = parse_err(scenario.source);

    assert_reserved_token_error(parsed.errors(), scenario.expected, span);
}

#[rstest]
#[case(ExpectedReservedToken::Bigint)]
#[case(ExpectedReservedToken::Bit)]
#[case(ExpectedReservedToken::Double)]
#[case(ExpectedReservedToken::Float)]
#[case(ExpectedReservedToken::Signed)]
fn legacy_types_in_declarations(
    #[case] expected: ExpectedReservedToken,
    #[values(
        DeclarationPosition::FunctionParameter,
        DeclarationPosition::RelationField
    )]
    position: DeclarationPosition,
) {
    let source = position.source(expected);
    let span = token_span(source.as_ref(), expected);
    let parsed = parse_err(source);
    assert_reserved_token_error(parsed.errors(), expected, span);
}

#[rstest]
#[case(ReservedTokenScenario::new("typedef Old = u32;", ExpectedReservedToken::Typedef))]
#[case(ReservedTokenScenario::new("type Bad = bigint;", ExpectedReservedToken::Bigint))]
#[case(ReservedTokenScenario::new("# bad", ExpectedReservedToken::BareHash))]
#[case(ReservedTokenScenario::new("Output(x) :- x <=> 1.", ExpectedReservedToken::Spaceship))]
fn rejected_syntax_preserves_later_declarations(#[case] scenario: ReservedTokenScenario) {
    let rejected = scenario.source.as_ref();
    let source = format!("{rejected}\ntype Later = u32;\ninput relation Kept(value: u32);\n");
    let parsed = parse_err(source);
    assert!(!reserved_token_spans(parsed.errors(), scenario.expected).is_empty());
    assert!(
        parsed
            .root()
            .type_defs()
            .iter()
            .any(|item| item.name().as_deref() == Some("Later"))
    );
    assert!(
        parsed
            .root()
            .relations()
            .iter()
            .any(|item| item.name().as_deref() == Some("Kept"))
    );
}

#[rstest]
#[case::bigint(ExpectedReservedToken::Bigint)]
#[case::bit(ExpectedReservedToken::Bit)]
#[case::double(ExpectedReservedToken::Double)]
#[case::float(ExpectedReservedToken::Float)]
#[case::signed(ExpectedReservedToken::Signed)]
fn legacy_type_names_are_rejected_outwith_type_position(#[case] expected: ExpectedReservedToken) {
    let legacy_type = expected.lexeme();
    let source = format!("Output(x) :- Source(x), var {legacy_type} = x.\n");
    let expected_span = token_span(&source, expected);
    let parsed = parse_err(source);

    assert_reserved_token_error(parsed.errors(), expected, expected_span);
}

#[test]
fn spaceship_operator_is_rejected_in_expression_context() {
    let Err(errors) = parse_expression("1 <=> 2") else {
        panic!("spaceship expression should be rejected");
    };

    assert_reserved_token_error(&errors, ExpectedReservedToken::Spaceship, 2..5);
}

#[rstest]
#[case(SourceText::from("# foo\n"))]
#[case(SourceText::from("# [attribute]\ntype Foo = u32\n"))]
#[case(SourceText::from("#/*comment*/[attribute]\ntype Foo = u32\n"))]
fn bare_hash_is_rejected(#[case] source: SourceText) {
    let bare = parse_err(source);
    assert_reserved_token_error(bare.errors(), ExpectedReservedToken::BareHash, 0..1);
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

#[rstest]
#[case(ReservedTokenScenario::new(
    "Output(x) :- Source(x), bigint.\n",
    ExpectedReservedToken::Bigint
))]
#[case(ReservedTokenScenario::new(
    "Output(x) :- x <=> y = z.\n",
    ExpectedReservedToken::Spaceship
))]
#[case(ReservedTokenScenario::new(
    "for (x in Items(x)) Process(x <=> 1).",
    ExpectedReservedToken::Spaceship
))]
fn reserved_errors_are_not_duplicated(#[case] scenario: ReservedTokenScenario) {
    let span = token_span(scenario.source.as_ref(), scenario.expected);
    let parsed = parse_err(scenario.source);
    assert_reserved_token_error(parsed.errors(), scenario.expected, span);
}

#[rstest]
#[case(ReservedTokenScenario::new("1 <=> 2 <=> 3", ExpectedReservedToken::Spaceship))]
#[case(ReservedTokenScenario::new("1 <=> bigint + float", ExpectedReservedToken::Spaceship))]
#[case(ReservedTokenScenario::new("1 <=> bigint + float", ExpectedReservedToken::Bigint))]
#[case(ReservedTokenScenario::new("1 <=> bigint + float", ExpectedReservedToken::Float))]
#[case(ReservedTokenScenario::new("# value", ExpectedReservedToken::BareHash))]
#[case(ReservedTokenScenario::new("1 <=> # value", ExpectedReservedToken::BareHash))]
#[case(ReservedTokenScenario::new("# [value]", ExpectedReservedToken::BareHash))]
#[case(ReservedTokenScenario::new("#/*comment*/[value]", ExpectedReservedToken::BareHash))]
fn expression_failures_report_every_reserved_token(#[case] scenario: ReservedTokenScenario) {
    let source = scenario.source.as_ref();
    let Err(errors) = parse_expression(source) else {
        panic!("reserved expression must fail");
    };
    let expected = source
        .match_indices(scenario.expected.lexeme())
        .map(|(start, text)| start..start + text.len())
        .collect::<Vec<_>>();
    assert_eq!(reserved_token_spans(&errors, scenario.expected), expected);
}

#[test]
fn many_reserved_operators_report_once_each() {
    const EXPRESSION_COUNT: usize = 128;
    let body = (0..EXPRESSION_COUNT)
        .map(|index| format!("value_{index} <=> value_{index}"))
        .collect::<Vec<_>>()
        .join(", ");
    let source = format!("Output(x) :- {body}.\n");
    let expected_spans = source
        .match_indices("<=>")
        .map(|(start, token)| start..start + token.len())
        .collect::<Vec<_>>();
    let parsed = parse_err(source);

    assert_eq!(
        count_custom_parse_errors(parsed.errors(), ExpectedReservedToken::Spaceship),
        EXPRESSION_COUNT,
    );
    assert_eq!(
        reserved_token_spans(parsed.errors(), ExpectedReservedToken::Spaceship),
        expected_spans,
    );
}

fn count_custom_parse_errors(
    errors: &[Simple<SyntaxKind>],
    expected: ExpectedReservedToken,
) -> usize {
    reserved_token_spans(errors, expected).len()
}

fn assert_reserved_token_error(
    errors: &[Simple<SyntaxKind>],
    expected: ExpectedReservedToken,
    span: Range<usize>,
) {
    assert_eq!(reserved_token_spans(errors, expected), vec![span]);
}

fn reserved_token_spans(
    errors: &[Simple<SyntaxKind>],
    expected: ExpectedReservedToken,
) -> Vec<Range<usize>> {
    errors
        .iter()
        .filter(
            |error| matches!(error.reason(), SimpleReason::Custom(message) if message == expected.message()),
        )
        .map(|error| error.span().clone())
        .collect()
}

fn token_span(source: &str, expected: ExpectedReservedToken) -> Range<usize> {
    let token = expected.lexeme();
    let Some(start) = source.find(token) else {
        panic!("missing token {token:?} in source {source:?}");
    };
    start..start + token.len()
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
