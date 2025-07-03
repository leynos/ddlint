//! Integration tests for the function parsing API.
//!
//! These tests verify that the parser correctly parses function declarations
//! and that the `Function` AST node provides the expected API methods.

use ddlint::parser::parse;

#[test]
#[expect(clippy::expect_used, reason = "Using expect for clearer test failures")]
fn extern_function_api() {
    let src = "extern function hash(data: string): u64\n";
    let parsed = parse(src);
    assert!(parsed.errors().is_empty());
    let funcs = parsed.root().functions();
    let func = funcs.first().expect("function missing");
    assert_eq!(func.name(), Some("hash".into()));
    assert!(func.is_extern());
    assert_eq!(func.parameters(), vec![("data".into(), "string".into())]);
    assert_eq!(func.return_type(), Some("u64".into()));
}

#[test]
#[expect(clippy::expect_used, reason = "Using expect for clearer test failures")]
fn regular_function_api() {
    let src = "function greet(name: string): string { }\n";
    let parsed = parse(src);
    assert!(parsed.errors().is_empty());
    let funcs = parsed.root().functions();
    let func = funcs.first().expect("function missing");
    assert_eq!(func.name(), Some("greet".into()));
    assert!(!func.is_extern());
    assert_eq!(func.parameters(), vec![("name".into(), "string".into())]);
    assert_eq!(func.return_type(), Some("string".into()));
}

#[test]
#[expect(clippy::expect_used, reason = "Using expect for clearer test failures")]
fn function_no_return_api() {
    let src = "function log(msg: string) { }\n";
    let parsed = parse(src);
    assert!(parsed.errors().is_empty());
    let funcs = parsed.root().functions();
    let func = funcs.first().expect("function missing");
    assert_eq!(func.name(), Some("log".into()));
    assert_eq!(func.return_type(), None);
}
