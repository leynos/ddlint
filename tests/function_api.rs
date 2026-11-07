//! Integration tests for the function parsing API.
//!
//! These tests verify that the parser correctly parses function declarations
//! and that the `Function` AST node provides the expected API methods.

use ddlint::{parser::parse, test_util::assert_no_parse_errors};
use rstest::rstest;

/// One function-parsing API case.
struct FunctionCase {
    src: &'static str,
    name: &'static str,
    is_extern: bool,
    params: Vec<(String, String)>,
    ret: Option<String>,
}

#[rstest]
#[case(FunctionCase {
    src: "extern function hash(data: string): u64\n",
    name: "hash",
    is_extern: true,
    params: vec![("data".to_owned(), "string".to_owned())],
    ret: Some("u64".to_owned()),
})]
#[case(FunctionCase {
    src: "function greet(name: string): string { }\n",
    name: "greet",
    is_extern: false,
    params: vec![("name".to_owned(), "string".to_owned())],
    ret: Some("string".to_owned()),
})]
#[case(FunctionCase {
    src: "function log(msg: string) { }\n",
    name: "log",
    is_extern: false,
    params: vec![("msg".to_owned(), "string".to_owned())],
    ret: None,
})]
fn function_api(#[case] case: FunctionCase) {
    let FunctionCase {
        src,
        name,
        is_extern,
        params,
        ret,
    } = case;
    let parsed = parse(src);
    assert_no_parse_errors(parsed.errors());
    let funcs = parsed.root().functions();
    let func = funcs.first().expect("function missing");
    assert_eq!(func.name().as_deref(), Some(name));
    assert_eq!(func.is_extern(), is_extern);
    assert_eq!(func.parameters(), params);
    assert_eq!(func.return_type(), ret);
}
