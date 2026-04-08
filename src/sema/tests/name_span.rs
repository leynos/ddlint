//! Semantic-model tests for precise identifier spans.

use rstest::rstest;

use super::{DeclarationKind, parse_ok, symbols_named};
use crate::sema::SymbolOrigin;

fn span_text<'a>(source: &'a str, span: &crate::Span) -> &'a str {
    source.get(span.start..span.end).unwrap_or_else(|| {
        panic!(
            "invalid UTF-8 boundary for span {}..{} in `{source}`",
            span.start, span.end
        )
    })
}

fn find_symbol<'a>(
    model: &'a super::super::SemanticModel,
    name: &str,
    kind: DeclarationKind,
    origin: SymbolOrigin,
) -> &'a super::super::Symbol {
    symbols_named(model, name, kind)
        .into_iter()
        .find(|symbol| symbol.origin() == origin)
        .unwrap_or_else(|| panic!("missing symbol `{name}` with origin {origin:?}"))
}

#[rstest]
#[case(
    "input relation Source(x: u32)",
    "Source",
    DeclarationKind::Relation,
    SymbolOrigin::RelationDeclaration,
    "Source"
)]
#[case(
    "function project(id: u32): u32 {}",
    "project",
    DeclarationKind::Function,
    SymbolOrigin::FunctionDeclaration,
    "project"
)]
#[case(
    "function project(project: u32): u32 { project }",
    "project",
    DeclarationKind::Function,
    SymbolOrigin::FunctionDeclaration,
    "project"
)]
#[case(
    "typedef UserId = u64",
    "UserId",
    DeclarationKind::Type,
    SymbolOrigin::TypeDeclaration,
    "UserId"
)]
fn top_level_declarations_capture_identifier_name_spans(
    #[case] source: &str,
    #[case] symbol_name: &str,
    #[case] kind: DeclarationKind,
    #[case] origin: SymbolOrigin,
    #[case] expected_text: &str,
) {
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

#[rstest]
fn rule_head_bindings_capture_identifier_name_spans() {
    let source = "Output(Output) :- Output(_).";
    let parsed = parse_ok(source);
    let semantic_model = super::super::build(&parsed);
    let symbol = find_symbol(
        &semantic_model,
        "Output",
        DeclarationKind::RuleBinding,
        SymbolOrigin::RuleHead,
    );

    let name_span = symbol
        .name_span()
        .unwrap_or_else(|| panic!("missing head binding name_span"));

    assert_eq!(span_text(source, name_span), "Output");
    assert_eq!(name_span.start, 7);
    assert_eq!(span_text(source, symbol.span()), source);
}

#[rstest]
fn assignment_bindings_capture_identifier_name_spans() {
    let source = "Output(result) :- Source(result), var assigned_x = Seed(result).";
    let parsed = parse_ok(source);
    let semantic_model = super::super::build(&parsed);
    let symbol = find_symbol(
        &semantic_model,
        "assigned_x",
        DeclarationKind::RuleBinding,
        SymbolOrigin::AssignmentPattern,
    );

    let name_span = symbol
        .name_span()
        .unwrap_or_else(|| panic!("missing assignment binding name_span"));

    assert_eq!(span_text(source, name_span), "assigned_x");
    assert_eq!(
        span_text(source, symbol.span()),
        "var assigned_x = Seed(result)"
    );
}

#[rstest]
fn for_loop_bindings_capture_identifier_name_spans() {
    let source = "Output(result) :- Source(result), for (item_y in Items(result)) Inner(result).";
    let parsed = parse_ok(source);
    let semantic_model = super::super::build(&parsed);
    let symbol = find_symbol(
        &semantic_model,
        "item_y",
        DeclarationKind::RuleBinding,
        SymbolOrigin::ForPattern,
    );

    let name_span = symbol
        .name_span()
        .unwrap_or_else(|| panic!("missing for-loop binding name_span"));

    assert_eq!(span_text(source, name_span), "item_y");
    assert_eq!(
        span_text(source, symbol.span()),
        "for (item_y in Items(result)) Inner(result)"
    );
}
