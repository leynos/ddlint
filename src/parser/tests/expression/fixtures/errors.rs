//! Error-case fixtures for expression parser tests.

use crate::parser::tests::expression::fixtures::{
    CountedErrorCase, PostfixErrorCase, SpannedErrorCase,
};

pub(super) fn error_cases() -> Vec<CountedErrorCase> {
    let mut cases = operator_error_cases();
    cases.extend(postfix_counted_error_cases());
    cases.extend(control_flow_error_cases());
    cases.extend(match_error_cases());
    cases.extend(collection_error_cases());
    cases
}

pub(super) fn match_pattern_error_cases() -> Vec<SpannedErrorCase> {
    vec![
        SpannedErrorCase {
            src: "match (x) { ) -> x }",
            msg: "unmatched closing parenthesis in match pattern",
            start: 12,
            end: 13,
        },
        SpannedErrorCase {
            src: "match (x) { ( } -> x }",
            msg: "unmatched closing brace in match pattern",
            start: 14,
            end: 15,
        },
        SpannedErrorCase {
            src: "match (x) { ] -> x }",
            msg: "unmatched closing bracket in match pattern",
            start: 12,
            end: 13,
        },
    ]
}

pub(super) fn postfix_error_cases() -> Vec<PostfixErrorCase> {
    vec![
        PostfixErrorCase {
            src: "foo.",
            msg: "expected identifier or tuple index after '.'",
            start: 4,
            end: 4,
            unclosed: false,
        },
        PostfixErrorCase {
            src: "e[1]",
            msg: "expected comma",
            start: 3,
            end: 4,
            unclosed: false,
        },
        PostfixErrorCase {
            src: "e[1,0",
            msg: "expected right bracket",
            start: 5,
            end: 5,
            unclosed: true,
        },
        PostfixErrorCase {
            src: "[1, 2",
            msg: "expected right bracket",
            start: 5,
            end: 5,
            unclosed: true,
        },
        PostfixErrorCase {
            src: "{a: 1",
            msg: "expected right brace",
            start: 5,
            end: 5,
            unclosed: true,
        },
        PostfixErrorCase {
            src: "{a, b}",
            msg: "expected ':'",
            start: 2,
            end: 3,
            unclosed: false,
        },
        PostfixErrorCase {
            src: "pkg::()",
            msg: "expected identifier after '::'",
            start: 5,
            end: 6,
            unclosed: false,
        },
    ]
}

fn counted_error_cases(srcs: &[&'static str]) -> Vec<CountedErrorCase> {
    srcs.iter()
        .map(|&src| CountedErrorCase { src, min_errs: 1 })
        .collect()
}

fn operator_error_cases() -> Vec<CountedErrorCase> {
    counted_error_cases(&[
        "1 +", "(1 + 2", "1 ? 2", "x :", "x as", "x =", "x ;", "x =>", "",
    ])
}

fn postfix_counted_error_cases() -> Vec<CountedErrorCase> {
    counted_error_cases(&[
        "e[1 0]", "e[,0]", "e[1,]", "e[1,,0]", "t.", "t.-1", "t.+1", "t..0",
    ])
}

fn control_flow_error_cases() -> Vec<CountedErrorCase> {
    counted_error_cases(&["if cond else value", "if cond value else", "if"])
}

fn match_error_cases() -> Vec<CountedErrorCase> {
    counted_error_cases(&[
        "match (x) { _ => x }",
        "match (x) {}",
        "match x { _ -> x }",
        "match (x) { ) -> x }",
        "match (x) { } -> x }",
        "match (x) { ] -> x }",
    ])
}

fn collection_error_cases() -> Vec<CountedErrorCase> {
    counted_error_cases(&["[1, 2", "{a: 1", "{a, b}"])
}
