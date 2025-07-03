use ddlint::parser::parse;

#[test]
fn extern_function_api() {
    let src = "extern function hash(data: string): u64\n";
    let parsed = parse(src);
    assert!(parsed.errors().is_empty());
    let funcs = parsed.root().functions();
    let func = funcs.first().unwrap_or_else(|| panic!("function missing"));
    assert_eq!(func.name(), Some("hash".into()));
    assert!(func.is_extern());
    assert_eq!(func.parameters(), vec![("data".into(), "string".into())]);
    assert_eq!(func.return_type(), Some("u64".into()));
}

#[test]
fn regular_function_api() {
    let src = "function greet(name: string): string { }\n";
    let parsed = parse(src);
    assert!(parsed.errors().is_empty());
    let funcs = parsed.root().functions();
    let func = funcs.first().unwrap_or_else(|| panic!("function missing"));
    assert_eq!(func.name(), Some("greet".into()));
    assert!(!func.is_extern());
    assert_eq!(func.parameters(), vec![("name".into(), "string".into())]);
    assert_eq!(func.return_type(), Some("string".into()));
}

#[test]
fn function_no_return_api() {
    let src = "function log(msg: string) { }\n";
    let parsed = parse(src);
    assert!(parsed.errors().is_empty());
    let funcs = parsed.root().functions();
    let func = funcs.first().unwrap_or_else(|| panic!("function missing"));
    assert_eq!(func.name(), Some("log".into()));
    assert_eq!(func.return_type(), None);
}
