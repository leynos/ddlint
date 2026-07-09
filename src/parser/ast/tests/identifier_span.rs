//! Tests for identifier-span discovery helpers.

use super::super::{AstNode, find_identifier_span, find_identifier_span_in_range};
use crate::{parse, test_util::span_text};

fn first_rule(source: &str) -> super::super::Rule {
    let Some(rule) = parse(source).root().rules().into_iter().next() else {
        panic!("expected a parsed rule in `{source}`");
    };
    rule
}

fn required_offset(source: &str, needle: &str) -> usize {
    let Some(offset) = source.find(needle) else {
        panic!("expected `{needle}` in `{source}`");
    };
    offset
}

#[test]
fn identifier_search_picks_match_within_repeated_name_subtree() {
    let source = "Output(foo) :- Source(foo), Other(foo).";
    let rule = first_rule(source);
    let other_start = required_offset(source, "Other(foo)");
    let other_end = other_start + "Other(foo)".len();

    let span = find_identifier_span_in_range(rule.syntax(), &(other_start..other_end), "foo")
        .unwrap_or_else(|| panic!("expected foo within Other(foo) in `{source}`"));

    assert_eq!(span_text(source, &span), "foo");
    assert_eq!(span.start, other_start + "Other(".len());
}

#[test]
fn identifier_search_returns_none_for_non_overlapping_range() {
    let source = "Output(foo) :- Source(foo), Other(foo).";
    let rule = first_rule(source);
    let output_start = required_offset(source, "Output");
    let output_end = output_start + "Output".len();

    assert_eq!(
        find_identifier_span_in_range(rule.syntax(), &(output_start..output_end), "foo"),
        None
    );
}

#[test]
fn identifier_search_matches_exact_nested_token_range() {
    let source = "Output(result) :- var assigned_x = Seed(result).";
    let rule = first_rule(source);
    let assign_start = required_offset(source, "assigned_x");
    let assign_end = assign_start + "assigned_x".len();
    let token_span = assign_start..assign_end;

    let span = find_identifier_span_in_range(rule.syntax(), &token_span, "assigned_x")
        .unwrap_or_else(|| panic!("expected exact nested token match in `{source}`"));

    assert_eq!(span, token_span);
}

#[test]
fn identifier_search_returns_none_when_name_is_absent() {
    let source = "Output(result) :- Source(result).";
    let rule = first_rule(source);

    assert_eq!(find_identifier_span(rule.syntax(), "missing_name"), None);
}

#[test]
fn identifier_search_prefers_first_match_in_source_order() {
    let source = "Output(foo) :- Nested(foo, foo).";
    let rule = first_rule(source);

    let span = find_identifier_span(rule.syntax(), "foo")
        .unwrap_or_else(|| panic!("expected to find foo in `{source}`"));

    assert_eq!(span_text(source, &span), "foo");
    assert_eq!(span.start, required_offset(source, "foo"));
}
