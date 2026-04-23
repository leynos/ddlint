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

fn operator_error_cases() -> Vec<CountedErrorCase> {
    vec![
        CountedErrorCase {
            src: "1 +",
            min_errs: 1,
        },
        CountedErrorCase {
            src: "(1 + 2",
            min_errs: 1,
        },
        CountedErrorCase {
            src: "1 ? 2",
            min_errs: 1,
        },
        CountedErrorCase {
            src: "x :",
            min_errs: 1,
        },
        CountedErrorCase {
            src: "x as",
            min_errs: 1,
        },
        CountedErrorCase {
            src: "x =",
            min_errs: 1,
        },
        CountedErrorCase {
            src: "x ;",
            min_errs: 1,
        },
        CountedErrorCase {
            src: "x =>",
            min_errs: 1,
        },
        CountedErrorCase {
            src: "",
            min_errs: 1,
        },
    ]
}

fn postfix_counted_error_cases() -> Vec<CountedErrorCase> {
    vec![
        CountedErrorCase {
            src: "e[1 0]",
            min_errs: 1,
        },
        CountedErrorCase {
            src: "e[,0]",
            min_errs: 1,
        },
        CountedErrorCase {
            src: "e[1,]",
            min_errs: 1,
        },
        CountedErrorCase {
            src: "e[1,,0]",
            min_errs: 1,
        },
        CountedErrorCase {
            src: "t.",
            min_errs: 1,
        },
        CountedErrorCase {
            src: "t.-1",
            min_errs: 1,
        },
        CountedErrorCase {
            src: "t.+1",
            min_errs: 1,
        },
        CountedErrorCase {
            src: "t..0",
            min_errs: 1,
        },
    ]
}

fn control_flow_error_cases() -> Vec<CountedErrorCase> {
    vec![
        CountedErrorCase {
            src: "if cond else value",
            min_errs: 1,
        },
        CountedErrorCase {
            src: "if cond value else",
            min_errs: 1,
        },
        CountedErrorCase {
            src: "if",
            min_errs: 1,
        },
    ]
}

fn match_error_cases() -> Vec<CountedErrorCase> {
    vec![
        CountedErrorCase {
            src: "match (x) { _ => x }",
            min_errs: 1,
        },
        CountedErrorCase {
            src: "match (x) {}",
            min_errs: 1,
        },
        CountedErrorCase {
            src: "match x { _ -> x }",
            min_errs: 1,
        },
        CountedErrorCase {
            src: "match (x) { ) -> x }",
            min_errs: 1,
        },
        CountedErrorCase {
            src: "match (x) { } -> x }",
            min_errs: 1,
        },
        CountedErrorCase {
            src: "match (x) { ] -> x }",
            min_errs: 1,
        },
    ]
}

fn collection_error_cases() -> Vec<CountedErrorCase> {
    vec![
        CountedErrorCase {
            src: "[1, 2",
            min_errs: 1,
        },
        CountedErrorCase {
            src: "{a: 1",
            min_errs: 1,
        },
        CountedErrorCase {
            src: "{a, b}",
            min_errs: 1,
        },
    ]
}
