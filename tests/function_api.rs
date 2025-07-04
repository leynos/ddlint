//! Integration tests for the function parsing API.
//!
//! These tests verify that the parser correctly parses function declarations
//! and that the `Function` AST node provides the expected API methods.

use ddlint::parser::parse;
use rstest::rstest;

#[rstest]
#[case(
    "extern function hash(data: string): u64\n",
    "hash",
    true,
    vec![("data".to_string(), "string".to_string())],
    Some("u64".to_string()),
)]
#[case(
    "function greet(name: string): string { }\n",
    "greet",
    false,
    vec![("name".to_string(), "string".to_string())],
    Some("string".to_string()),
)]
#[case(
    "function log(msg: string) { }\n",
    "log",
    false,
    vec![("msg".to_string(), "string".to_string())],
    None,
)]
fn function_api(
    #[case] src: &str,
    #[case] name: &str,
    #[case] is_extern: bool,
    #[case] params: Vec<(String, String)>,
    #[case] ret: Option<String>,
) {
    let parsed = parse(src);
    assert!(parsed.errors().is_empty());
    let funcs = parsed.root().functions();
    #[expect(clippy::expect_used, reason = "Using expect for clearer test failures")]
    let func = funcs.first().expect("function missing");
    assert_eq!(func.name(), Some(name.to_string()));
    assert_eq!(func.is_extern(), is_extern);
    assert_eq!(func.parameters(), params);
    assert_eq!(func.return_type(), ret);
}
