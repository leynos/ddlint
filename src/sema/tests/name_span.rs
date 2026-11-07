//! Semantic-model tests for precise identifier spans.

use rstest::rstest;

use super::{DeclarationKind, parse_ok, symbols_named};
use crate::sema::SymbolOrigin;

fn span_text<'a>(source: &'a str, span: &crate::Span) -> &'a str {
    let Some(text) = source.get(span.start..span.end) else {
        panic!(
            "invalid UTF-8 boundary for span {}..{} in `{source}`",
            span.start, span.end
        )
    };
    text
}

fn find_symbol<'a>(
    model: &'a super::super::SemanticModel,
    name: &str,
    kind: DeclarationKind,
    origin: SymbolOrigin,
) -> &'a super::super::Symbol {
    let Some(symbol) = symbols_named(model, name, kind)
        .into_iter()
        .find(|symbol| symbol.origin() == origin)
    else {
        panic!("missing symbol `{name}` with origin {origin:?}");
    };
    symbol
}

/// One top-level declaration name-span case.
struct TopLevelDeclarationCase {
    source: &'static str,
    symbol_name: &'static str,
    kind: DeclarationKind,
    origin: SymbolOrigin,
    expected_text: &'static str,
}

/// Check that top-level declarations record identifier name spans.
#[rstest]
#[case(TopLevelDeclarationCase {
    source: "input relation Source(x: u32)",
    symbol_name: "Source",
    kind: DeclarationKind::Relation,
    origin: SymbolOrigin::RelationDeclaration,
    expected_text: "Source",
})]
#[case(TopLevelDeclarationCase {
    source: "function project(id: u32): u32 {}",
    symbol_name: "project",
    kind: DeclarationKind::Function,
    origin: SymbolOrigin::FunctionDeclaration,
    expected_text: "project",
})]
#[case(TopLevelDeclarationCase {
    source: "function project(project: u32): u32 { project }",
    symbol_name: "project",
    kind: DeclarationKind::Function,
    origin: SymbolOrigin::FunctionDeclaration,
    expected_text: "project",
})]
#[case(TopLevelDeclarationCase {
    source: "type UserId = u64",
    symbol_name: "UserId",
    kind: DeclarationKind::Type,
    origin: SymbolOrigin::TypeDeclaration,
    expected_text: "UserId",
})]
fn top_level_declarations_capture_identifier_name_spans(#[case] case: TopLevelDeclarationCase) {
    let TopLevelDeclarationCase {
        source,
        symbol_name,
        kind,
        origin,
        expected_text,
    } = case;
    let parsed = parse_ok(source);
    let semantic_model = super::super::build(&parsed);
    let symbol = find_symbol(&semantic_model, symbol_name, kind, origin);

    let name_span = symbol
        .name_span()
        .unwrap_or_else(|| panic!("missing name_span for `{symbol_name}`"));

    assert_eq!(span_text(source, name_span), expected_text);
    assert_ne!(name_span, symbol.span());
}

#[rstest]
fn semantic_rule_head_bindings_leave_name_span_unavailable() {
    let source = "for (item_y in Source(item_y)) Output(item_y).";
    let parsed = parse_ok(source);
    let semantic_model = super::super::build(&parsed);
    let symbol = find_symbol(
        &semantic_model,
        "item_y",
        DeclarationKind::RuleBinding,
        SymbolOrigin::SemanticRuleHead,
    );

    assert!(symbol.name_span().is_none());
    assert_eq!(span_text(source, symbol.span()), source);
}

/// One binding declaration name-span case.
struct BindingDeclarationCase {
    source: &'static str,
    symbol_name: &'static str,
    origin: SymbolOrigin,
    expected_name_text: &'static str,
    expected_name_span_start: Option<usize>,
    expected_span_text: &'static str,
}

/// Check that binding declarations record identifier name spans.
#[rstest]
#[case(BindingDeclarationCase {
    source: "Output(Output) :- Output(_).",
    symbol_name: "Output",
    origin: SymbolOrigin::RuleHead,
    expected_name_text: "Output",
    expected_name_span_start: Some(7_usize),
    expected_span_text: "Output(Output) :- Output(_).",
})]
#[case(BindingDeclarationCase {
    source: "Output(result) :- Source(result), var assigned_x = Seed(result).",
    symbol_name: "assigned_x",
    origin: SymbolOrigin::AssignmentPattern,
    expected_name_text: "assigned_x",
    expected_name_span_start: None,
    expected_span_text: "var assigned_x = Seed(result)",
})]
#[case(BindingDeclarationCase {
    source: "Output(result) :- Source(result), for (item_y in Items(result)) Inner(result).",
    symbol_name: "item_y",
    origin: SymbolOrigin::ForPattern,
    expected_name_text: "item_y",
    expected_name_span_start: None,
    expected_span_text: "for (item_y in Items(result)) Inner(result)",
})]
fn binding_declarations_capture_identifier_name_spans(#[case] case: BindingDeclarationCase) {
    let BindingDeclarationCase {
        source,
        symbol_name,
        origin,
        expected_name_text,
        expected_name_span_start,
        expected_span_text,
    } = case;
    let parsed = parse_ok(source);
    let semantic_model = super::super::build(&parsed);
    let symbol = find_symbol(
        &semantic_model,
        symbol_name,
        DeclarationKind::RuleBinding,
        origin,
    );

    let name_span = symbol
        .name_span()
        .unwrap_or_else(|| panic!("missing binding name_span for `{symbol_name}`"));

    assert_eq!(span_text(source, name_span), expected_name_text);
    if let Some(start) = expected_name_span_start {
        assert_eq!(name_span.start, start);
    }
    assert_eq!(span_text(source, symbol.span()), expected_span_text);
}

#[rstest]
fn nested_for_loop_bindings_lack_name_span() {
    let source = concat!(
        "Output(x) :- Source(x), for (outer in Items(x)) for (inner in Nested(outer)) ",
        "Inner(inner)."
    );
    let parsed = parse_ok(source);
    let semantic_model = super::super::build(&parsed);
    let symbol = find_symbol(
        &semantic_model,
        "inner",
        DeclarationKind::RuleBinding,
        SymbolOrigin::ForPattern,
    );

    assert!(symbol.name_span().is_none());
    assert_eq!(
        span_text(source, symbol.span()),
        "for (outer in Items(x)) for (inner in Nested(outer)) Inner(inner)"
    );
}
