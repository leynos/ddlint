//! Integration tests for function parameter error handling.
//!
//! These tests feed malformed function declarations into the parser and assert
//! that specific delimiter issues are reported with precise spans.

use ddlint::parser::parse;
use ddlint::test_util::assert_delimiter_error;
use rstest::rstest;

#[rstest]
#[case("function f(x: int {", "T_RPAREN", 0, 19)]
#[case("function f[x: int) {}", "T_LPAREN", 10, 11)]
fn function_parameter_errors(
    #[case] src: &str,
    #[case] msg: &str,
    #[case] start: usize,
    #[case] end: usize,
) {
    let parsed = parse(src);
    let errors = parsed.errors();
    assert_delimiter_error(errors, msg, start, end);
}
