//! Function declaration parsing tests.
//!
//! Exercises extern and normal functions with varied signatures.

use super::helpers::{parse_function, pretty_print};
use crate::{parser::ast::AstNode, test_util::assert_parse_error};
use rstest::{fixture, rstest};

#[fixture]
fn extern_function() -> &'static str {
    "extern function hash(data: string): u64\n"
}

#[fixture]
fn function_with_body() -> &'static str {
    "function to_uppercase(s: string): string {\n}\n"
}

#[fixture]
fn function_no_return() -> &'static str {
    "function log_message(msg: string) {\n}\n"
}

#[fixture]
fn function_no_params() -> &'static str {
    "function greet(): string {\n}\n"
}

#[fixture]
fn function_multi_params() -> &'static str {
    "function concat(a: string, b: string): string {\n}\n"
}

#[fixture]
fn function_complex_params() -> &'static str {
    "function complex(p: (u32,(u8,string))): bool {\n}\n"
}

#[fixture]
fn function_ws_comments() -> &'static str {
    "function  spaced  (  x : string )  :  u8 { /*empty*/ }\n"
}

#[fixture]
fn function_generic_params() -> &'static str {
    "function example(arg: Vec<(u32,string)>, map: Map<string,u64>): bool {\n}\n"
}

#[fixture]
fn function_nested_generics() -> &'static str {
    "function test(p: Vec<Map<string,Vec<u8>>>, arr: [Vec<u32>]): bool {}\n"
}

#[fixture]
fn function_shift_param() -> &'static str {
    "function shift(x: Vec<<u8>>): bool {}\n"
}

#[rstest]
#[case::extern_fn(extern_function(), "hash", true, vec![("data", "string")], Some("u64"))]
#[case::body(function_with_body(), "to_uppercase", false, vec![("s", "string")], Some("string"))]
#[case::no_return(function_no_return(), "log_message", false, vec![("msg", "string")], None)]
#[case::no_params(function_no_params(), "greet", false, vec![], Some("string"))]
#[case::multi_params(function_multi_params(), "concat", false, vec![("a", "string"), ("b", "string")], Some("string"))]
#[case::complex_params(function_complex_params(), "complex", false, vec![("p", "(u32,(u8,string))")], Some("bool"))]
#[case::ws_comments(function_ws_comments(), "spaced", false, vec![("x", "string")], Some("u8"))]
#[case::generic(function_generic_params(), "example", false, vec![("arg", "Vec<(u32,string)>") , ("map", "Map<string,u64>")], Some("bool"))]
#[case::nested_generics(function_nested_generics(), "test", false, vec![("p", "Vec<Map<string,Vec<u8>>>") , ("arr", "[Vec<u32>]")], Some("bool"))]
#[case::shift(function_shift_param(), "shift", false, vec![("x", "Vec<<u8>>")], Some("bool"))]
fn parses_functions(
    #[case] src: &str,
    #[case] name: &str,
    #[case] is_extern: bool,
    #[case] params: Vec<(&str, &str)>,
    #[case] ret: Option<&str>,
) {
    let func = parse_function(src);
    assert_eq!(func.name().as_deref(), Some(name));
    assert_eq!(func.is_extern(), is_extern);
    let expected: Vec<(String, String)> = params
        .into_iter()
        .map(|(n, t)| (n.into(), t.into()))
        .collect();
    assert_eq!(func.parameters(), expected);
    assert_eq!(func.return_type().as_deref(), ret);
    assert_eq!(pretty_print(func.syntax()), src);
}

#[fixture]
fn extern_function_missing_colon() -> &'static str {
    "extern function missing_colon u32\n"
}

#[fixture]
fn function_unterminated_body() -> &'static str {
    "function foo() {"
}

#[fixture]
fn function_unclosed_params() -> &'static str {
    "function bad(a: string { }\n"
}

#[rstest]
fn extern_function_missing_colon_is_error(extern_function_missing_colon: &str) {
    let parsed = crate::parse(extern_function_missing_colon);
    assert_parse_error(parsed.errors(), "left paren", 30, 33);
    assert!(parsed.root().functions().is_empty());
}

#[rstest]
#[case(function_unterminated_body(), "right brace", function_unterminated_body().len())]
#[case(function_unclosed_params(), "right paren", function_unclosed_params().len())]
fn function_error_cases(#[case] src: &str, #[case] token: &str, #[case] end: usize) {
    let parsed = crate::parse(src);
    assert_parse_error(parsed.errors(), token, 0, end);
    assert!(parsed.root().functions().is_empty());
}
