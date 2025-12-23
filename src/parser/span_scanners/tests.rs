//! Tests for specialised span scanners.

use super::rules::rule_statement;
use super::{
    collect_function_spans, collect_import_spans, collect_index_spans, collect_relation_spans,
    collect_rule_spans, collect_transformer_spans, collect_typedef_spans,
};
use crate::parser::lexer_helpers::inline_ws;
use crate::parser::span_scanner::{merge_spans, parse_tokens};
use crate::test_util::tokenize;
use crate::{Span, SyntaxKind};
use chumsky::{Parser, Stream};
use rstest::rstest;

/// Configuration for `assert_rule_span_collection`, allowing fluent toggling of optional behaviour.
struct RuleSpanAssertConfig<'a> {
    expected_count: usize,
    count_message: &'a str,
    select_last: bool,
    trim_before_check: bool,
    expected_prefix: &'a str,
}

impl<'a> RuleSpanAssertConfig<'a> {
    fn new(expected_count: usize, count_message: &'a str, expected_prefix: &'a str) -> Self {
        Self {
            expected_count,
            count_message,
            select_last: false,
            trim_before_check: false,
            expected_prefix,
        }
    }

    fn select_last(mut self) -> Self {
        self.select_last = true;
        self
    }

    fn trim_before_check(mut self) -> Self {
        self.trim_before_check = true;
        self
    }
}

/// Assert rule span collection for a given source, with configurable span selection.
fn assert_rule_span_collection(src: &str, config: &RuleSpanAssertConfig<'_>) {
    let tokens = tokenize(src);
    let (rule_spans, _, errors) = collect_rule_spans(&tokens, src, &[]);
    assert!(errors.is_empty());
    assert_eq!(
        rule_spans.len(),
        config.expected_count,
        "{}",
        config.count_message
    );
    let span = if config.select_last {
        rule_spans
            .last()
            .cloned()
            .unwrap_or_else(|| panic!("expected at least one rule span"))
    } else {
        rule_spans
            .first()
            .cloned()
            .unwrap_or_else(|| panic!("expected at least one rule span"))
    };
    let rule_text = src
        .get(span.clone())
        .unwrap_or_else(|| panic!("rule span {span:?} out of bounds"));
    let checked = if config.trim_before_check {
        rule_text.trim_start()
    } else {
        rule_text
    };
    assert!(
        checked.starts_with(config.expected_prefix),
        "unexpected rule text: {rule_text}"
    );
}

fn parse_rule_statement_input(src: &str) -> (Option<()>, Vec<chumsky::error::Simple<SyntaxKind>>) {
    let tokens = tokenize(src);
    let ws = inline_ws().repeated().ignored();
    let parser = rule_statement(ws);
    let stream = Stream::from_iter(0..src.len(), tokens.into_iter());
    parser.parse_recovery(stream)
}

#[rstest]
#[case("import foo\n", vec![0..11], true)]
#[case("import\n", vec![], false)]
fn collect_import_spans_cases(
    #[case] src: &str,
    #[case] expected: Vec<Span>,
    #[case] errs_empty: bool,
) {
    let tokens = tokenize(src);
    let (spans, errs) = collect_import_spans(&tokens, src);
    assert_eq!(spans, expected);
    assert_eq!(errs.is_empty(), errs_empty);
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

#[expect(
    clippy::expect_used,
    reason = "tests unwrap spans to provide crisp failure messages"
)]
#[test]
fn collects_expression_spans_for_each_literal() {
    let src = "\n  R(x) :- Atom(x), if ready(x) { Accept(x) } else { Skip() }.";
    let tokens = tokenize(src);
    let (_rule_spans, expr_spans, errors) = collect_rule_spans(&tokens, src, &[]);
    assert!(errors.is_empty());
    assert_eq!(expr_spans.len(), 2);
    let first = expr_spans
        .first()
        .and_then(|sp| src.get(sp.clone()))
        .expect("first span text missing");
    assert_eq!(first, "Atom(x)");
    let second = expr_spans
        .get(1)
        .cloned()
        .and_then(|sp| src.get(sp))
        .expect("second span text missing");
    assert_eq!(second.trim(), "if ready(x) { Accept(x) } else { Skip() }");
}

#[test]
fn collects_rule_spans_for_by_ref_and_multi_head_rules() {
    let src = "&A(x), B'(y)@loc(y) -<1> :- C(x, y).";
    let tokens = tokenize(src);
    let (rule_spans, expr_spans, errors) = collect_rule_spans(&tokens, src, &[]);
    assert!(errors.is_empty(), "unexpected errors: {errors:?}");
    assert_eq!(rule_spans.len(), 1);
    assert_eq!(expr_spans.len(), 1);
}

#[test]
fn merge_spans_merges_overlapping_and_adjacent() {
    let spans = vec![10..12, 0..5, 5..7, 11..15];
    let merged = merge_spans(spans);
    assert_eq!(merged, vec![0..7, 10..15]);
}

#[test]
fn merge_spans_sorts_and_preserves_gaps() {
    let spans = vec![30..35, 5..10, 20..22];
    let merged = merge_spans(spans);
    assert_eq!(merged, vec![5..10, 20..22, 30..35]);
}

#[test]
fn collect_rule_spans_handles_adjacent_exclusions() {
    let src = "R1(x) :- A(x). R2(x) :- B(x).";
    let tokens = tokenize(src);

    let first_rule = "R1(x) :- A(x).";
    let start = src
        .find(first_rule)
        .unwrap_or_else(|| panic!("missing first rule in {src:?}"));
    let end = start + first_rule.len();
    // Two adjacent exclusion spans covering the first rule.
    let exclusions = vec![start..end - 1, end - 1..end];

    let (rule_spans, _, errors) = collect_rule_spans(&tokens, src, &exclusions);
    assert!(errors.is_empty());
    assert_eq!(rule_spans.len(), 1, "second rule should remain");
    let remaining = rule_spans
        .first()
        .and_then(|sp| src.get(sp.clone()))
        .unwrap_or_else(|| panic!("remaining rule span out of bounds"));
    assert!(
        remaining.starts_with("R2("),
        "unexpected remaining rule: {remaining}"
    );
}

#[test]
fn collect_rule_spans_skips_multiple_disjoint_exclusions() {
    let src = "\
import foo
extern transformer normalise(input: Data): Data
R1(x) :- A(x).
function greet(name: string): string { }
R2(x) :- B(x).
";
    let tokens = tokenize(src);

    let (import_spans, import_errors) = collect_import_spans(&tokens, src);
    assert!(import_errors.is_empty());
    let (typedef_spans, typedef_errors) = collect_typedef_spans(&tokens, src);
    assert!(typedef_errors.is_empty());
    let (relation_spans, relation_errors) = collect_relation_spans(&tokens, src);
    assert!(relation_errors.is_empty());
    let (index_spans, index_errors) = collect_index_spans(&tokens, src);
    assert!(index_errors.is_empty());
    let (function_spans, function_errors) = collect_function_spans(&tokens, src);
    assert!(function_errors.is_empty());
    let (transformer_spans, transformer_errors) = collect_transformer_spans(&tokens, src);
    assert!(transformer_errors.is_empty());

    let mut non_rule_spans = Vec::new();
    non_rule_spans.extend(import_spans);
    non_rule_spans.extend(typedef_spans);
    non_rule_spans.extend(relation_spans);
    non_rule_spans.extend(index_spans);
    non_rule_spans.extend(function_spans);
    non_rule_spans.extend(transformer_spans);
    let non_rule_spans = merge_spans(non_rule_spans);

    let (rule_spans, expr_spans, rule_errors) = collect_rule_spans(&tokens, src, &non_rule_spans);
    assert!(rule_errors.is_empty());
    assert_eq!(rule_spans.len(), 2, "expected two rules only");
    assert_eq!(expr_spans.len(), 2, "expected one expression per rule");

    let first_rule_span = rule_spans
        .first()
        .cloned()
        .unwrap_or_else(|| panic!("expected first rule span in {src:?}"));
    let first_rule = src
        .get(first_rule_span.clone())
        .unwrap_or_else(|| panic!("first rule span {first_rule_span:?} out of bounds"));
    assert!(
        first_rule.starts_with("R1("),
        "unexpected first rule text: {first_rule}"
    );
    let second_rule_span = rule_spans
        .get(1)
        .cloned()
        .unwrap_or_else(|| panic!("expected second rule span in {src:?}"));
    let second_rule = src
        .get(second_rule_span.clone())
        .unwrap_or_else(|| panic!("second rule span {second_rule_span:?} out of bounds"));
    assert!(
        second_rule.starts_with("R2("),
        "unexpected second rule text: {second_rule}"
    );
}

#[test]
fn collect_rule_spans_respects_exclusions() {
    let src = "import foo\nextern function f(): void {}\nR(x) :- Foo(x).\n";
    let tokens = tokenize(src);
    let (imports, errors) = collect_import_spans(&tokens, src);
    assert!(errors.is_empty());
    let (functions, fn_errors) = collect_function_spans(&tokens, src);
    assert!(fn_errors.is_empty());

    let imports = merge_spans([imports, functions].concat());

    let (rule_spans, expr_spans, rule_errors) = collect_rule_spans(&tokens, src, &imports);
    assert!(rule_errors.is_empty());
    assert_eq!(rule_spans.len(), 1);
    assert_eq!(expr_spans.len(), 1);

    let rule_span = rule_spans
        .first()
        .cloned()
        .unwrap_or_else(|| panic!("rule span missing for {src:?}"));
    let rule_text = src
        .get(rule_span.clone())
        .unwrap_or_else(|| panic!("rule span {rule_span:?} out of bounds"));
    assert!(
        rule_text.starts_with("R("),
        "unexpected rule text: {rule_text}"
    );

    let expr_span = expr_spans
        .first()
        .cloned()
        .unwrap_or_else(|| panic!("expression span missing for {src:?}"));
    let expr_text = src
        .get(expr_span.clone())
        .unwrap_or_else(|| panic!("expression span {expr_span:?} out of bounds"))
        .trim();
    assert_eq!(expr_text, "Foo(x)");
}

#[test]
fn rule_not_treated_as_line_start_after_inline_spaces() {
    let src = "   R(x) :- A(x).";
    let tokens = tokenize(src);
    let (rule_spans, _, errors) = collect_rule_spans(&tokens, src, &[]);
    assert!(errors.is_empty());
    assert!(
        rule_spans.is_empty(),
        "rule should not start after inline spaces with no boundary"
    );
}

#[test]
fn rule_treated_as_line_start_after_dot_same_line() {
    assert_rule_span_collection(
        "Fact().   R(x) :- A(x).",
        &RuleSpanAssertConfig::new(2, "both fact and trailing rule should parse", "R(")
            .select_last()
            .trim_before_check(),
    );
}

#[test]
fn rule_treated_as_line_start_after_newline_trivia() {
    assert_rule_span_collection(
        "/*c*/\n   // inline comment\nR(x) :- A(x).",
        &RuleSpanAssertConfig::new(1, "newline trivia should start rule", "R("),
    );
}

#[test]
fn parse_tokens_skips_non_rule_constructs_when_scanning_rules() {
    let src = concat!(
        "import foo::bar\n",
        "typedef T = string\n",
        "input relation Log(id: u32) primary key (id)\n",
        "index I_Log on Log(id)\n",
        "function f(): u32 { return 1; }\n",
        "extern transformer normalise(input: Log): Log\n",
        "Rule(x) :- Log(x).\n"
    );

    let tokens = tokenize(src);
    let (spans, errors) = parse_tokens(&tokens, src);

    assert!(errors.is_empty());

    assert_eq!(spans.rules().len(), 1);
    let rule_span = spans
        .rules()
        .first()
        .cloned()
        .unwrap_or_else(|| panic!("missing rule span for {src:?}"));
    let rule_text = src
        .get(rule_span.clone())
        .unwrap_or_else(|| panic!("rule span {rule_span:?} out of bounds"));
    assert_eq!(rule_text.trim_end(), "Rule(x) :- Log(x).");

    assert_eq!(
        spans.expressions().len(),
        1,
        "expected one expression span for rule body"
    );
    let expr_span = spans
        .expressions()
        .first()
        .cloned()
        .unwrap_or_else(|| panic!("expression span missing for {src:?}"));
    let expr_text = src
        .get(expr_span.clone())
        .unwrap_or_else(|| panic!("expression span {expr_span:?} out of bounds"))
        .trim();
    assert_eq!(expr_text, "Log(x)");
}
