//! Tests for rule-head binding span helpers.

use super::{collect_head_binding_spans, should_skip_binding_ident};
use crate::{Span, SyntaxKind, tokenize_without_trivia};

fn ident_indices(tokens: &[(SyntaxKind, Span)]) -> Vec<usize> {
    tokens
        .iter()
        .enumerate()
        .filter_map(|(index, (kind, _))| (*kind == SyntaxKind::T_IDENT).then_some(index))
        .collect()
}

#[test]
fn collect_head_binding_spans_skips_relation_callee_collision() {
    let source = "Output(Output)";

    let spans = collect_head_binding_spans(source, 0);

    assert_eq!(spans.as_slice(), &[("Output".to_string(), 7..13)]);
}

#[test]
fn should_skip_binding_ident_rejects_callee_but_keeps_argument_binding() {
    let tokens = tokenize_without_trivia("Output(Output)");
    let indices = ident_indices(&tokens);
    let [callee_index, binding_index] = indices.as_slice() else {
        panic!("expected two identifier tokens in `Output(Output)`");
    };

    assert_eq!(indices.len(), 2);
    assert!(should_skip_binding_ident(&tokens, *callee_index));
    assert!(!should_skip_binding_ident(&tokens, *binding_index));
}

#[test]
fn should_skip_binding_ident_rejects_dotted_and_constructor_positions() {
    let tokens = tokenize_without_trivia("Output(record.field, Item { value: value })");
    let indices = ident_indices(&tokens);
    let [
        callee_index,
        record_index,
        field_index,
        constructor_index,
        field_label_index,
        binding_index,
    ] = indices.as_slice()
    else {
        panic!("expected six identifier tokens in dotted/constructor sample");
    };

    assert_eq!(indices.len(), 6);
    assert!(should_skip_binding_ident(&tokens, *callee_index));
    assert!(!should_skip_binding_ident(&tokens, *record_index));
    assert!(should_skip_binding_ident(&tokens, *field_index));
    assert!(should_skip_binding_ident(&tokens, *constructor_index));
    assert!(should_skip_binding_ident(&tokens, *field_label_index));
    assert!(!should_skip_binding_ident(&tokens, *binding_index));
}
