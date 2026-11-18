//! Rule-statement scanner tests.
//!
//! These cases pin how the rule-body grammar accepts control-flow statements,
//! including assignments whose right-hand side is `if` or `for`, and where it
//! reports a diagnostic when the statement shape is wrong.

use chumsky::{Parser, Stream};
use rstest::rstest;

use super::super::rules::rule_statement;
use crate::{SyntaxKind, parser::lexer_helpers::inline_ws, test_util::tokenize};

fn parse_rule_statement_input(src: &str) -> (Option<()>, Vec<chumsky::error::Simple<SyntaxKind>>) {
    let tokens = tokenize(src);
    let ws = inline_ws().repeated().ignored();
    let parser = rule_statement(ws);
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
#[case("var y = if x > 1 { x } else { 0 }")]
#[case("y = if ready(x) { Accept(x) } else { Skip() }")]
fn rule_statement_parses_control_flow(#[case] src: &str) {
    let (res, errs) = parse_rule_statement_input(src);
    assert!(res.is_some(), "expected successful parse for {src}");
    assert!(errs.is_empty(), "unexpected errors for {src:?}: {errs:?}");
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
