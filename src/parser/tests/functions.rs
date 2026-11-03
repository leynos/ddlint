//! Function declaration parsing tests.
//!
//! Exercises extern and normal functions with varied signatures.

use fixture_lints::allow_fixture_expansion_lints;
use rstest::{fixture, rstest};

use super::helpers::{parse_function, pretty_print};
use crate::{parser::ast::AstNode, test_util::assert_parse_error};

#[allow_fixture_expansion_lints]
#[fixture]
fn extern_function() -> &'static str { "extern function hash(data: string): u64\n" }

#[allow_fixture_expansion_lints]
#[fixture]
fn function_with_body() -> &'static str { "function to_uppercase(s: string): string {\n}\n" }

#[allow_fixture_expansion_lints]
#[fixture]
fn function_no_return() -> &'static str { "function log_message(msg: string) {\n}\n" }

#[allow_fixture_expansion_lints]
#[fixture]
fn function_no_params() -> &'static str { "function greet(): string {\n}\n" }

#[allow_fixture_expansion_lints]
#[fixture]
fn function_multi_params() -> &'static str {
    "function concat(a: string, b: string): string {\n}\n"
}

#[allow_fixture_expansion_lints]
#[fixture]
fn function_complex_params() -> &'static str {
    "function complex(p: (u32,(u8,string))): bool {\n}\n"
}

#[allow_fixture_expansion_lints]
#[fixture]
fn function_ws_comments() -> &'static str {
    "function  spaced  (  x : string )  :  u8 { /*empty*/ }\n"
}

#[allow_fixture_expansion_lints]
#[fixture]
fn function_generic_params() -> &'static str {
    "function example(arg: Vec<(u32,string)>, map: Map<string,u64>): bool {\n}\n"
}

#[allow_fixture_expansion_lints]
#[fixture]
fn function_nested_generics() -> &'static str {
    "function test(p: Vec<Map<string,Vec<u8>>>, arr: [Vec<u32>]): bool {}\n"
}

#[allow_fixture_expansion_lints]
#[fixture]
fn function_shift_param() -> &'static str { "function shift(x: Vec<<u8>>): bool {}\n" }

/// One function-parsing case.
struct FunctionParseCase {
    src: &'static str,
    name: &'static str,
    is_extern: bool,
    params: Vec<(&'static str, &'static str)>,
    ret: Option<&'static str>,
}

#[rstest]
#[case::extern_fn(FunctionParseCase {
    src: extern_function(),
    name: "hash",
    is_extern: true,
    params: vec![("data", "string")],
    ret: Some("u64"),
})]
#[case::body(FunctionParseCase {
    src: function_with_body(),
    name: "to_uppercase",
    is_extern: false,
    params: vec![("s", "string")],
    ret: Some("string"),
})]
#[case::no_return(FunctionParseCase {
    src: function_no_return(),
    name: "log_message",
    is_extern: false,
    params: vec![("msg", "string")],
    ret: None,
})]
#[case::no_params(FunctionParseCase {
    src: function_no_params(),
    name: "greet",
    is_extern: false,
    params: vec![],
    ret: Some("string"),
})]
#[case::multi_params(FunctionParseCase {
    src: function_multi_params(),
    name: "concat",
    is_extern: false,
    params: vec![("a", "string"), ("b", "string")],
    ret: Some("string"),
})]
#[case::complex_params(FunctionParseCase {
    src: function_complex_params(),
    name: "complex",
    is_extern: false,
    params: vec![("p", "(u32,(u8,string))")],
    ret: Some("bool"),
})]
#[case::ws_comments(FunctionParseCase {
    src: function_ws_comments(),
    name: "spaced",
    is_extern: false,
    params: vec![("x", "string")],
    ret: Some("u8"),
})]
#[case::generic(FunctionParseCase {
    src: function_generic_params(),
    name: "example",
    is_extern: false,
    params: vec![("arg", "Vec<(u32,string)>"), ("map", "Map<string,u64>")],
    ret: Some("bool"),
})]
#[case::nested_generics(FunctionParseCase {
    src: function_nested_generics(),
    name: "test",
    is_extern: false,
    params: vec![("p", "Vec<Map<string,Vec<u8>>>"), ("arr", "[Vec<u32>]")],
    ret: Some("bool"),
})]
#[case::shift(FunctionParseCase {
    src: function_shift_param(),
    name: "shift",
    is_extern: false,
    params: vec![("x", "Vec<<u8>>")],
    ret: Some("bool"),
})]
fn parses_functions(#[case] case: FunctionParseCase) {
    let FunctionParseCase {
        src,
        name,
        is_extern,
        params,
        ret,
    } = case;
    let func = parse_function(src).expect("expected a single function");
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

#[allow_fixture_expansion_lints]
#[fixture]
fn extern_function_missing_colon() -> &'static str { "extern function missing_colon u32\n" }

#[allow_fixture_expansion_lints]
#[fixture]
fn function_unterminated_body() -> &'static str { "function foo() {" }

#[allow_fixture_expansion_lints]
#[fixture]
fn function_unclosed_params() -> &'static str { "function bad(a: string { }\n" }

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
