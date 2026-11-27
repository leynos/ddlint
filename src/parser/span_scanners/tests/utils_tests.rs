//! Tests for the shared span-scanner utilities.
//!
//! `parse_and_record` owns the record, advance, and recover policy that every
//! Chumsky-based scanner composes. These tests pin that policy directly so the
//! single-keyword collector built on top of it cannot change it silently.

use std::cell::Cell;

use chumsky::{
    error::{Simple, SimpleReason},
    prelude::*,
};
use rstest::rstest;

use super::super::utils::{State, collect_with_parser, parse_and_record};
use crate::{Span, SyntaxKind, test_util::tokenize};

const PRE_EXISTING_DIAGNOSTIC: &str = "pre-existing diagnostic";
const RECORDED_DIAGNOSTIC: &str = "recorded diagnostic";

/// Source whose first line holds two tokens followed by a second line.
///
/// Keeping a second line lets a cursor assertion distinguish advancing to the
/// parsed span end from skipping the whole line.
const TWO_LINE_SRC: &str = "alpha beta\ngamma delta\n";

fn whole_token_span() -> impl Parser<SyntaxKind, Span, Error = Simple<SyntaxKind>> {
    any().map_with_span(|_, span: Span| span)
}

fn whole_token_span_with_diagnostic() -> impl Parser<SyntaxKind, Span, Error = Simple<SyntaxKind>> {
    whole_token_span().validate(|span, _whole: Span, emit| {
        emit(Simple::custom(span.clone(), RECORDED_DIAGNOSTIC));
        span
    })
}

fn failing_parser() -> impl Parser<SyntaxKind, Span, Error = Simple<SyntaxKind>> {
    just(SyntaxKind::K_TYPE).map_with_span(|_, span: Span| span)
}

fn custom_message(error: &Simple<SyntaxKind>) -> Option<&str> {
    match error.reason() {
        SimpleReason::Custom(message) => Some(message.as_str()),
        SimpleReason::Unexpected | SimpleReason::Unclosed { .. } => None,
    }
}

fn state_with_diagnostic<'a>(tokens: &'a [(SyntaxKind, Span)], src: &'a str) -> State<'a> {
    let mut st: State<'a> = State::new(tokens, src, Vec::new());
    st.extra.push(Simple::custom(0..0, PRE_EXISTING_DIAGNOSTIC));
    st
}

#[test]
fn parse_and_record_records_span_and_advances_to_its_end() {
    let tokens = tokenize(TWO_LINE_SRC);
    let mut st: State<'_> = State::new(&tokens, TWO_LINE_SRC, Vec::new());

    let (result, errors) = parse_and_record(&mut st, 0, whole_token_span());

    assert_eq!(
        result,
        Some(0..5),
        "the first token span should be returned unchanged"
    );
    assert_eq!(
        st.spans,
        vec![0..5],
        "the parsed span should be recorded once"
    );
    assert_eq!(
        st.stream.cursor(),
        1,
        "the cursor should stop at the span end instead of skipping the line"
    );
    assert!(errors.is_empty(), "no diagnostics were emitted: {errors:?}");
    assert!(
        st.extra.is_empty(),
        "extra should stay empty: {:?}",
        st.extra
    );
}

#[test]
fn parse_and_record_appends_diagnostics_after_existing_ones() {
    let tokens = tokenize(TWO_LINE_SRC);
    let mut st = state_with_diagnostic(&tokens, TWO_LINE_SRC);

    let (result, errors) = parse_and_record(&mut st, 0, whole_token_span_with_diagnostic());

    assert_eq!(
        result,
        Some(0..5),
        "a parser that only validates still yields its span"
    );
    assert_eq!(
        st.spans,
        vec![0..5],
        "a validating parser should still record its span"
    );
    assert_eq!(
        st.stream.cursor(),
        1,
        "validation errors must not trigger line skipping"
    );
    assert_eq!(
        errors.len(),
        1,
        "one diagnostic should be reported: {errors:?}"
    );
    assert_eq!(
        st.extra.len(),
        2,
        "the pre-existing diagnostic should be kept: {:?}",
        st.extra
    );
    assert_eq!(
        st.extra.first().and_then(custom_message),
        Some(PRE_EXISTING_DIAGNOSTIC),
        "the pre-existing diagnostic should stay first"
    );
    assert_eq!(
        st.extra.get(1).and_then(custom_message),
        Some(RECORDED_DIAGNOSTIC),
        "the new diagnostic should be appended after the existing one"
    );
}

#[test]
fn parse_and_record_skips_the_line_on_failure() {
    let tokens = tokenize(TWO_LINE_SRC);
    let mut st = state_with_diagnostic(&tokens, TWO_LINE_SRC);

    let (result, errors) = parse_and_record(&mut st, 0, failing_parser());

    assert_eq!(result, None, "the failing parser should yield no span");
    assert!(
        st.spans.is_empty(),
        "a failed parse must not record a span: {:?}",
        st.spans
    );
    assert_eq!(
        st.stream.cursor(),
        4,
        "the cursor should move to the first token after the newline"
    );
    assert_eq!(
        st.stream.peek().map(|(kind, _)| *kind),
        Some(SyntaxKind::T_IDENT),
        "recovery should stop at the next line's first token"
    );
    assert!(
        !errors.is_empty(),
        "a failed parse must report at least one diagnostic"
    );
    assert_eq!(
        st.extra.len(),
        1 + errors.len(),
        "failure diagnostics should be appended to extra: {:?}",
        st.extra
    );
    assert_eq!(
        st.extra.first().and_then(custom_message),
        Some(PRE_EXISTING_DIAGNOSTIC),
        "appending failure diagnostics must not reorder existing ones"
    );
}

const VALIDATION_DIAGNOSTIC: &str = "keyword pair rejected by validation";

/// Minimal inline-trivia parser, so these tests do not depend on the
/// production grammars or their recovery details.
fn inline_trivia() -> impl Parser<SyntaxKind, (), Error = Simple<SyntaxKind>> + Clone {
    filter(|kind: &SyntaxKind| matches!(kind, SyntaxKind::T_WHITESPACE | SyntaxKind::T_COMMENT))
        .ignored()
}

/// Minimal single-keyword grammar: `type <ident>` with inline trivia.
///
/// Like the production grammars, the trailing trivia padding means a recorded
/// span reaches to the end of the statement's inline whitespace.
fn type_ident_pair() -> impl Parser<SyntaxKind, Span, Error = Simple<SyntaxKind>> {
    let trivia = inline_trivia().repeated();
    just(SyntaxKind::K_TYPE)
        .padded_by(trivia.clone())
        .ignore_then(just(SyntaxKind::T_IDENT).padded_by(trivia))
        .map_with_span(|_, span: Span| span)
}

/// Minimal grammar that emits one non-terminal diagnostic per match.
fn validating_type_ident_pair() -> impl Parser<SyntaxKind, Span, Error = Simple<SyntaxKind>> {
    type_ident_pair().validate(|span, _whole: Span, emit| {
        emit(Simple::custom(span.clone(), VALIDATION_DIAGNOSTIC));
        span
    })
}

/// Read the source text under each span, marking invalid spans distinctly.
fn span_texts<'a>(src: &'a str, spans: &[Span]) -> Vec<&'a str> {
    spans
        .iter()
        .map(|span| src.get(span.clone()).unwrap_or("<out of bounds>"))
        .collect()
}

#[rstest]
#[case("", vec![])]
#[case("type alpha", vec!["type alpha"])]
#[case(":: type alpha", vec!["type alpha"])]
#[case("type alpha type beta", vec!["type alpha ", "type beta"])]
fn collect_with_parser_records_matches_in_source_order(
    #[case] src: &str,
    #[case] expected_texts: Vec<&str>,
) {
    let tokens = tokenize(src);
    let (spans, errors) = collect_with_parser(&tokens, src, SyntaxKind::K_TYPE, type_ident_pair);
    assert_eq!(
        span_texts(src, &spans),
        expected_texts,
        "collected span texts mismatch for {src:?}: {spans:?}"
    );
    assert!(
        errors.is_empty(),
        "no diagnostics expected for {src:?}: {errors:?}"
    );
}

#[test]
fn collect_with_parser_skips_failed_line_and_continues() {
    let src = "type type\ntype alpha\n";
    let tokens = tokenize(src);
    let (spans, errors) = collect_with_parser(&tokens, src, SyntaxKind::K_TYPE, type_ident_pair);
    // The grammar's trailing padding absorbs the newline, so a successful span
    // reaches past it while the failed line contributes only a diagnostic.
    assert_eq!(
        span_texts(src, &spans),
        vec!["type alpha\n"],
        "the declaration after the failed line should still be collected"
    );
    assert!(
        !errors.is_empty(),
        "the failed declaration should report a diagnostic"
    );
}

#[test]
fn collect_with_parser_keeps_span_and_diagnostics_on_validating_success() {
    let src = "type alpha type beta";
    let tokens = tokenize(src);
    let (spans, errors) =
        collect_with_parser(&tokens, src, SyntaxKind::K_TYPE, validating_type_ident_pair);
    assert_eq!(
        span_texts(src, &spans),
        vec!["type alpha ", "type beta"],
        "validating parsers should still record every match"
    );
    assert_eq!(
        errors.len(),
        2,
        "each match should append its validation diagnostic: {errors:?}"
    );
    assert!(
        errors
            .iter()
            .all(|error| custom_message(error) == Some(VALIDATION_DIAGNOSTIC)),
        "every diagnostic should be the validation diagnostic: {errors:?}"
    );
}

#[test]
fn collect_with_parser_builds_a_fresh_parser_per_match() {
    let src = "type alpha type beta type gamma";
    let tokens = tokenize(src);
    let calls = Cell::new(0usize);
    let make_parser = || {
        calls.set(calls.get() + 1);
        type_ident_pair()
    };

    let (spans, errors) = collect_with_parser(&tokens, src, SyntaxKind::K_TYPE, make_parser);

    assert_eq!(
        calls.get(),
        3,
        "the factory should run once per matching keyword"
    );
    assert_eq!(spans.len(), 3, "every match should record a span");
    assert!(errors.is_empty(), "no diagnostics expected: {errors:?}");
}
