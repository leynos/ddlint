//! Rule-statement scanner tests.
//!
//! These cases pin how the rule-body grammar accepts control-flow statements,
//! including assignments whose right-hand side is `if` or `for`, and where it
//! reports a diagnostic when the statement shape is wrong.

use chumsky::{Parser, Stream, prelude::end};
use rstest::rstest;

use super::super::{collect_rule_spans, rules::rule_statement};
use crate::{SyntaxKind, parser::lexer_helpers::inline_ws, test_util::tokenize};

fn parse_rule_statement_input(src: &str) -> (Option<()>, Vec<chumsky::error::Simple<SyntaxKind>>) {
    let tokens = tokenize(src);
    let ws = inline_ws().repeated().ignored();
    // The production statement parser stops at a rule separator, so the tests
    // add `end()` themselves: without it a parser that accepts only a prefix
    // would pass, hiding a scanner that never reaches the statement's tail.
    let parser = rule_statement(ws).then_ignore(end());
    let stream = Stream::from_iter(0..src.len(), tokens.into_iter());
    parser.parse_recovery(stream)
}

#[rstest]
#[case("if (cond) if (nested) Process(nested) else Skip() else Handle()")]
#[case("for (a in A(a)) for (b in B(b)) ProcessPair(a, b)")]
#[case("for (item in if cond { Items(item) } else { Others(item) }) Process(item)")]
#[case("for (item in Items(item)) if (item > 10) Process(item)")]
#[case("for (item in Items(item) if item.active) Process(item)")]
#[case("if (outer) { for (item in Items(item)) if (should(item)) Process(item) } else { Skip() }")]
#[case("Call(x)")]
#[case("foo = bar(baz)")]
fn rule_statement_parses_control_flow(#[case] src: &str) {
    let (res, errs) = parse_rule_statement_input(src);
    assert!(res.is_some(), "expected successful parse for {src}");
    assert!(errs.is_empty(), "unexpected errors for {src:?}: {errs:?}");
}

/// An assignment right-hand side must not end the rule early.
///
/// The statement parser accepts a valid prefix, so only the full rule scan
/// shows whether the whole right-hand side stayed in the same rule.
#[rstest]
#[case("R(x) :- Atom(x), var y = { 1 } + 2.", &["Atom(x)", "var y = { 1 } + 2"])]
#[case("R(x) :- Atom(x), var y = if ready { 1 } else { 2 }.", &["Atom(x)", "var y = if ready { 1 } else { 2 }"])]
#[case("R(x) :- Atom(x), var y = if x > 1 { x } else { 0 }.", &["Atom(x)", "var y = if x > 1 { x } else { 0 }"])]
#[case("R(x) :- Atom(x), y = if ready(x) { Accept(x) } else { Skip() }.", &["Atom(x)", "y = if ready(x) { Accept(x) } else { Skip() }"])]
#[case("R(x) :- Atom(x), var y = for (i in Items(i)) i.", &["Atom(x)", "var y = for (i in Items(i)) i"])]
#[case(
    "R(x) :- Atom(x), var y = { 1 } + 2, Other(y).",
    &["Atom(x)", "var y = { 1 } + 2", "Other(y)"]
)]
#[case(
    "R(x) :- 1 + { y = obj.field; y }, Use(x).",
    &["1 + { y = obj.field; y }", "Use(x)"]
)]
fn assignment_with_block_rhs_scans_one_rule(#[case] src: &str, #[case] expected_terms: &[&str]) {
    let tokens = tokenize(src);
    let (rule_spans, expr_spans, errors) = collect_rule_spans(&tokens, src, &[]);
    assert_eq!(rule_spans.len(), 1, "expected one rule span for {src:?}");
    assert!(
        errors.is_empty(),
        "unexpected errors for {src:?}: {errors:?}"
    );
    let rule_text = rule_spans
        .first()
        .and_then(|span| src.get(span.clone()))
        .unwrap_or_default();
    assert_eq!(
        rule_text, src,
        "the rule span must cover the whole statement for {src:?}"
    );
    let terms: Vec<&str> = expr_spans
        .iter()
        .filter_map(|span| src.get(span.clone()))
        .map(str::trim)
        .collect();
    assert_eq!(
        terms.as_slice(),
        expected_terms,
        "each body term must keep its full span for {src:?}"
    );
}

#[rstest]
#[case("if { Process(cond) }")]
#[case("for (item Items(item)) Process(item)")]
#[case("for (item in Items(item) Process(item)")]
fn rule_statement_reports_errors(#[case] src: &str) {
    let (res, errs) = parse_rule_statement_input(src);
    if res.is_some() {
        assert!(
            !errs.is_empty(),
            "expected errors for {src}, but parser recovered without diagnostics",
        );
    } else {
        assert!(
            !errs.is_empty(),
            "expected errors for {src}, but parser reported none",
        );
    }
}
