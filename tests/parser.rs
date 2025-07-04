//! Integration tests for the parser skeleton.
//!
//! These tests focus on verifying the CST construction and that the round-trip
//! property holds for simple inputs. Grammar-specific assertions will be added
//! once the parser rules are implemented.

use ddlint::{
    SyntaxKind,
    ast::{Import, TypeDef},
    parse,
};
use rstest::{fixture, rstest};

/// Collect the text of a syntax subtree.
///
/// This helper iteratively traverses the tree using an explicit stack so
/// deeply nested inputs do not risk recursion overflow. It enables
/// round-trip tests that assert the printed output matches the original
/// source.
fn pretty_print(node: &rowan::SyntaxNode<ddlint::DdlogLanguage>) -> String {
    let mut out = String::new();
    let mut stack = vec![rowan::SyntaxElement::Node(node.clone())];

    while let Some(item) = stack.pop() {
        match item {
            rowan::SyntaxElement::Token(t) => out.push_str(t.text()),
            rowan::SyntaxElement::Node(n) => {
                let children: Vec<rowan::SyntaxElement<ddlint::DdlogLanguage>> =
                    n.children_with_tokens().collect();
                for child in children.into_iter().rev() {
                    stack.push(child);
                }
            }
        }
    }

    out
}

/// Collapse runs of whitespace into single spaces.
///
/// ```
/// assert_eq!(normalise_whitespace("a  b\n c"), "a b c");
/// ```
fn normalise_whitespace(text: &str) -> String {
    text.split_whitespace().collect::<Vec<_>>().join(" ")
}

#[fixture]
fn simple_prog() -> &'static str {
    "input relation R(x: u32);"
}

#[fixture]
fn complex_prog() -> &'static str {
    "input relation R(x: u32);\noutput relation S(y: string);"
}

#[fixture]
fn empty_prog() -> &'static str {
    ""
}

#[fixture]
fn input_relation_pk() -> &'static str {
    "input relation User(user_id: u32, username: string) primary key (user_id)"
}

#[fixture]
fn output_relation_no_pk() -> &'static str {
    "output relation Alert(message: string, timestamp: u64)"
}

#[fixture]
fn internal_relation_compound_pk() -> &'static str {
    "relation UserSession(user_id: u32, session_id: string, start_time: u64) primary key (user_id, session_id)"
}

#[fixture]
fn multiline_relation() -> &'static str {
    "input relation Log(\n    id: u32,\n    message: string\n) primary key (id)\n"
}

#[fixture]
fn index_single_column() -> &'static str {
    "index Idx_User_username on User(username)"
}

#[fixture]
fn index_multi_column() -> &'static str {
    "index Idx_Session_user_time on UserSession(user_id, start_time)"
}

#[fixture]
fn index_invalid_missing_on() -> &'static str {
    "index Idx_Invalid User(username)"
}

#[fixture]
fn index_nested_function() -> &'static str {
    "index Idx_lower_username on User(lower(username))"
}

#[fixture]
fn index_unbalanced_parentheses() -> &'static str {
    "index Idx_Unbalanced on User(lower(username)"
}

#[fixture]
fn index_whitespace_variations() -> &'static str {
    "  index  Idx_User_ws \t on\n  User (\n    username  )  "
}

/// Verifies that parsing and pretty-printing preserves the original input text
/// and produces the expected root node kind.
#[rstest]
fn parse_round_trip(simple_prog: &str) {
    let parsed = parse(simple_prog);
    let text = pretty_print(parsed.root().syntax());
    assert_eq!(text, simple_prog);
    assert_eq!(parsed.root().kind(), SyntaxKind::N_DATALOG_PROGRAM);
}

/// Ensures that invalid tokens are represented by `N_ERROR` nodes in the CST.
#[rstest]
fn error_token_produces_error_node() {
    let source = "?";
    let parsed = parse(source);
    let root = parsed.root().syntax();
    let has_error = root
        .children_with_tokens()
        .filter_map(|child| match child {
            rowan::NodeOrToken::Node(node) => Some(node),
            rowan::NodeOrToken::Token(_) => None,
        })
        .any(|node| node.kind() == SyntaxKind::N_ERROR);
    assert!(has_error);
}

#[rstest]
fn import_statement_standard_case() {
    let src = "import standard_library";
    let parsed = parse(src);
    assert!(parsed.errors().is_empty());
    let imports = parsed.root().imports();
    assert_eq!(imports.len(), 1);
    let Some(imp) = imports.first() else {
        panic!("expected import");
    };
    assert_eq!(imp.path(), "standard_library");
    assert!(imp.alias().is_none());
}

#[rstest]
fn import_statement_with_alias() {
    let src = "import collections::vector as vec";
    let parsed = parse(src);
    assert!(parsed.errors().is_empty());
    let imports = parsed.root().imports();
    assert_eq!(imports.len(), 1);
    let Some(imp) = imports.first() else {
        panic!("expected import");
    };
    assert_eq!(imp.path(), "collections::vector");
    assert_eq!(imp.alias(), Some("vec".to_string()));
}

#[rstest]
fn import_statement_invalid_missing_path() {
    use chumsky::error::SimpleReason;

    let src = "import as missing_path";
    let parsed = parse(src);
    let errors = parsed.errors();
    assert_eq!(errors.len(), 1);
    let Some(error) = errors.first() else {
        panic!("expected error");
    };
    assert!(matches!(error.reason(), SimpleReason::Unexpected));
    assert!(
        error
            .expected()
            .any(|e| e.as_ref().is_some_and(|k| *k == SyntaxKind::T_IDENT))
    );
    assert_eq!(error.found(), Some(&SyntaxKind::K_AS));
    let imports = parsed.root().imports();
    assert!(imports.is_empty());
}

#[rstest]
fn import_invalid_then_valid() {
    let src = "import as\nimport foo";
    let parsed = parse(src);
    let imports = parsed.root().imports();
    assert_eq!(imports.len(), 1);
    assert_eq!(imports.first().map(Import::path), Some("foo".to_string()));
}

#[rstest]
fn import_statement_multi_segment() {
    let src = "import foo::bar::baz";
    let parsed = parse(src);
    assert!(parsed.errors().is_empty());
    let paths: Vec<_> = parsed.root().imports().iter().map(Import::path).collect();
    assert_eq!(paths, ["foo::bar::baz"]);
}

#[rstest]
fn import_statement_whitespace_variations() {
    let src = "  import  foo  as  f  ";
    let parsed = parse(src);
    assert!(parsed.errors().is_empty());
    let imports = parsed.root().imports();
    let Some(imp) = imports.first() else {
        panic!("expected import");
    };
    assert_eq!(imp.path(), "foo");
    assert_eq!(imp.alias(), Some("f".into()));
}

#[rstest]
fn import_multiple_statements() {
    let src = "import a\nimport b as c";
    let parsed = parse(src);
    assert!(parsed.errors().is_empty());
    let imports = parsed.root().imports();
    let paths: Vec<_> = imports.iter().map(|i| (i.path(), i.alias())).collect();
    assert_eq!(paths, [("a".into(), None), ("b".into(), Some("c".into()))]);
}

#[rstest]
fn standard_typedef() {
    let src = "typedef Uuid = string\n";
    let parsed = parse(src);
    assert!(parsed.errors().is_empty());
    let defs = parsed.root().type_defs();
    assert_eq!(defs.len(), 1);
    let def = defs.first().unwrap_or_else(|| panic!("typedef not found"));
    assert_eq!(def.name(), Some("Uuid".into()));
    assert!(!def.is_extern());
}

#[rstest]
fn complex_typedef() {
    let src = "typedef UserRecord = (name: string, age: u64, active: bool)\n";
    let parsed = parse(src);
    assert!(parsed.errors().is_empty());
    let defs = parsed.root().type_defs();
    assert_eq!(defs.len(), 1);
    let def = defs.first().unwrap_or_else(|| panic!("typedef not found"));
    assert_eq!(def.name(), Some("UserRecord".into()));
    assert!(!def.is_extern());
}

#[rstest]
fn extern_type() {
    let src = "extern type FfiHandle\n";
    let parsed = parse(src);
    assert!(parsed.errors().is_empty());
    let defs = parsed.root().type_defs();
    assert_eq!(defs.len(), 1);
    let def = defs.first().unwrap_or_else(|| panic!("typedef not found"));
    assert_eq!(def.name(), Some("FfiHandle".into()));
    assert!(def.is_extern());
}

#[rstest]
fn extern_without_type_is_ignored() {
    let src = "extern foo\nextern type Bar";
    let parsed = parse(src);
    let defs = parsed.root().type_defs();
    assert_eq!(defs.len(), 1);
    assert_eq!(
        defs.first().and_then(TypeDef::name),
        Some("Bar".to_string())
    );
}

#[rstest]
#[case("typedef Uuid = string\n", "Uuid", false, "typedef Uuid = string\n")]
#[case("typedef Foo=bar\n", "Foo", false, "typedef Foo=bar\n")]
#[case(
    "typedef Record = (name: string, active: bool)\n",
    "Record",
    false,
    "typedef Record = (name: string, active: bool)\n"
)]
#[case("extern type Handle\n", "Handle", true, "extern type Handle\n")]
#[case("extern type  Extra  \n", "Extra", true, "extern type  Extra  \n")]
fn typedef_variations(
    #[case] src: &str,
    #[case] expected: &str,
    #[case] is_extern: bool,
    #[case] expected_text: &str,
) {
    let parsed = parse(src);
    assert!(parsed.errors().is_empty());
    let defs = parsed.root().type_defs();
    assert_eq!(defs.len(), 1);
    let def = defs
        .first()
        .unwrap_or_else(|| panic!("typedef should exist for valid source"));
    assert_eq!(def.name(), Some(expected.to_string()));
    assert_eq!(def.is_extern(), is_extern);
    let text = pretty_print(def.syntax());
    assert_eq!(text, expected_text);
}

#[test]
fn typedef_nesting_and_whitespace() {
    let src = "typedef Foo=string\ntypedef   Bar = u64  \n";
    let parsed = parse(src);
    assert!(parsed.errors().is_empty());
    assert_eq!(pretty_print(parsed.root().syntax()), src);
    let defs = parsed.root().type_defs();
    assert_eq!(defs.len(), 2);
    let first = defs
        .first()
        .unwrap_or_else(|| panic!("first typedef missing"));
    let second = defs
        .get(1)
        .unwrap_or_else(|| panic!("second typedef missing"));
    assert_eq!(pretty_print(first.syntax()), "typedef Foo=string\n");
    assert_eq!(pretty_print(second.syntax()), "typedef   Bar = u64  \n");
    let first_end = first.syntax().text_range().end();
    let second_start = second.syntax().text_range().start();
    assert!(first_end <= second_start);
}

#[test]
fn typedef_missing_name_returns_none() {
    let src = "typedef = string\n";
    let parsed = parse(src);
    assert!(parsed.errors().is_empty());
    let defs = parsed.root().type_defs();
    assert_eq!(defs.len(), 1);
    let def = defs
        .first()
        .unwrap_or_else(|| panic!("typedef span exists"));
    assert_eq!(def.name(), None);
}

#[rstest]
fn input_relation_parsed(input_relation_pk: &str) {
    let parsed = parse(input_relation_pk);
    let relations = parsed.root().relations();
    assert_eq!(relations.len(), 1);
    let rel = relations
        .first()
        .cloned()
        .unwrap_or_else(|| panic!("relation missing"));
    assert!(rel.is_input());
    assert!(!rel.is_output());
    assert_eq!(rel.name(), Some("User".into()));
    assert_eq!(
        rel.columns(),
        vec![
            ("user_id".into(), "u32".into()),
            ("username".into(), "string".into()),
        ]
    );
    assert_eq!(rel.primary_key(), Some(vec!["user_id".into()]));
}

#[rstest]
fn output_relation_parsed(output_relation_no_pk: &str) {
    let parsed = parse(output_relation_no_pk);
    let relations = parsed.root().relations();
    assert_eq!(relations.len(), 1);
    let rel = relations
        .first()
        .cloned()
        .unwrap_or_else(|| panic!("relation missing"));
    assert!(!rel.is_input());
    assert!(rel.is_output());
    assert_eq!(rel.name(), Some("Alert".into()));
    assert_eq!(
        rel.columns(),
        vec![
            ("message".into(), "string".into()),
            ("timestamp".into(), "u64".into()),
        ]
    );
    assert!(rel.primary_key().is_none());
}

#[rstest]
fn internal_relation_parsed(internal_relation_compound_pk: &str) {
    let parsed = parse(internal_relation_compound_pk);
    let relations = parsed.root().relations();
    assert_eq!(relations.len(), 1);
    let rel = relations
        .first()
        .cloned()
        .unwrap_or_else(|| panic!("relation missing"));
    assert!(!rel.is_input());
    assert!(!rel.is_output());
    assert_eq!(rel.name(), Some("UserSession".into()));
    assert_eq!(
        rel.columns(),
        vec![
            ("user_id".into(), "u32".into()),
            ("session_id".into(), "string".into()),
            ("start_time".into(), "u64".into()),
        ]
    );
    assert_eq!(
        rel.primary_key(),
        Some(vec!["user_id".into(), "session_id".into()])
    );
}

#[rstest]
fn multiline_relation_parsed(multiline_relation: &str) {
    let parsed = parse(multiline_relation);
    assert!(parsed.errors().is_empty());
    let relations = parsed.root().relations();
    assert_eq!(relations.len(), 1);
    let rel = relations
        .first()
        .unwrap_or_else(|| panic!("relation missing"));
    assert!(rel.is_input());
    assert_eq!(rel.name(), Some("Log".into()));
    assert_eq!(
        rel.columns(),
        vec![
            ("id".into(), "u32".into()),
            ("message".into(), "string".into()),
        ]
    );
    assert_eq!(rel.primary_key(), Some(vec!["id".into()]));
    assert_eq!(pretty_print(rel.syntax()), multiline_relation);
}

#[rstest]
fn index_single_column_parsed(index_single_column: &str) {
    let parsed = parse(index_single_column);
    assert!(parsed.errors().is_empty());
    let indexes = parsed.root().indexes();
    assert_eq!(indexes.len(), 1);
    let Some(idx) = indexes.first() else {
        panic!("index should exist for valid source");
    };
    assert_eq!(idx.name(), Some("Idx_User_username".into()));
    assert_eq!(idx.relation(), Some("User".into()));
    assert_eq!(idx.columns(), vec![String::from("username")]);
}

#[rstest]
fn index_multi_column_parsed(index_multi_column: &str) {
    let parsed = parse(index_multi_column);
    assert!(parsed.errors().is_empty());
    let indexes = parsed.root().indexes();
    assert_eq!(indexes.len(), 1);
    let Some(idx) = indexes.first() else {
        panic!("index should exist for valid source");
    };
    assert_eq!(idx.name(), Some("Idx_Session_user_time".into()));
    assert_eq!(idx.relation(), Some("UserSession".into()));
    assert_eq!(
        idx.columns(),
        vec![String::from("user_id"), String::from("start_time")]
    );
}

#[rstest]
fn index_missing_on_is_error(index_invalid_missing_on: &str) {
    use chumsky::error::SimpleReason;

    let parsed = parse(index_invalid_missing_on);
    let errors = parsed.errors();
    assert_eq!(errors.len(), 1);
    let Some(err) = errors.first() else {
        panic!("expected parse error");
    };
    assert!(matches!(err.reason(), SimpleReason::Unexpected));
    assert_eq!(parsed.root().indexes().len(), 0);
}

#[rstest]
fn index_nested_function_parsed(index_nested_function: &str) {
    let parsed = parse(index_nested_function);
    assert!(parsed.errors().is_empty());
    let indexes = parsed.root().indexes();
    assert_eq!(indexes.len(), 1);
    let Some(idx) = indexes.first() else {
        panic!("index should exist for valid source");
    };
    assert_eq!(idx.name(), Some("Idx_lower_username".into()));
    assert_eq!(idx.relation(), Some("User".into()));
    assert_eq!(idx.columns(), vec![String::from("lower(username)")]);
}

#[rstest]
fn index_unbalanced_parentheses_is_error(index_unbalanced_parentheses: &str) {
    let parsed = parse(index_unbalanced_parentheses);
    assert!(!parsed.errors().is_empty());
    assert!(parsed.root().indexes().is_empty());
}

#[rstest]
#[case("index Idx_User_ws on User(username)")]
#[case(" index  Idx_User_ws  on  User( username ) ")]
#[case(index_whitespace_variations())]
fn index_declaration_whitespace_variations(#[case] src: &str) {
    let parsed = parse(src);
    assert!(parsed.errors().is_empty());
    let printed = pretty_print(parsed.root().syntax());
    assert_eq!(normalise_whitespace(&printed), normalise_whitespace(src));
    let indexes = parsed.root().indexes();
    let Some(idx) = indexes.first() else {
        panic!("expected index");
    };
    assert_eq!(idx.name(), Some("Idx_User_ws".into()));
    assert_eq!(idx.relation(), Some("User".into()));
    assert_eq!(idx.columns(), vec![String::from("username")]);
}

#[fixture]
fn simple_rule() -> &'static str {
    "ActiveUser(user_id) :- User(user_id, _, true)."
}

#[fixture]
fn multi_literal_rule() -> &'static str {
    "UserLogin(username, session_id) :- User(user_id, username, _), UserSession(user_id, session_id, _)."
}

#[fixture]
fn fact_rule() -> &'static str {
    "SystemAlert(\"System is now online.\")."
}

#[rstest]
fn simple_rule_parsed(simple_rule: &str) {
    let parsed = parse(simple_rule);
    assert!(parsed.errors().is_empty());
    let rules = parsed.root().rules();
    assert_eq!(rules.len(), 1);
    let Some(rule) = rules.first() else {
        panic!("rule missing");
    };
    assert_eq!(pretty_print(rule.syntax()), simple_rule);
}

#[rstest]
fn multi_literal_rule_parsed(multi_literal_rule: &str) {
    let parsed = parse(multi_literal_rule);
    assert!(parsed.errors().is_empty());
    let rules = parsed.root().rules();
    assert_eq!(rules.len(), 1);
    let Some(rule) = rules.first() else {
        panic!("rule missing");
    };
    assert_eq!(pretty_print(rule.syntax()), multi_literal_rule);
}

#[rstest]
fn fact_rule_parsed(fact_rule: &str) {
    let parsed = parse(fact_rule);
    assert!(parsed.errors().is_empty());
    let rules = parsed.root().rules();
    assert_eq!(rules.len(), 1);
    let Some(rule) = rules.first() else {
        panic!("rule missing");
    };
    assert_eq!(pretty_print(rule.syntax()), fact_rule);
}

#[test]
fn invalid_rule_missing_head() {
    let input = ":- User(user_id, username, _).";
    let parsed = parse(input);
    assert!(
        !parsed.errors().is_empty(),
        "Expected errors for missing head"
    );
}

#[test]
fn invalid_rule_missing_body() {
    let input = "UserLogin(username, session_id) :- .";
    let parsed = parse(input);
    assert!(
        !parsed.errors().is_empty(),
        "Expected errors for missing body"
    );
}

#[test]
fn invalid_rule_no_colon_dash() {
    let input = "UserLogin(username, session_id) User(user_id, username, _).";
    let parsed = parse(input);
    assert!(
        !parsed.errors().is_empty(),
        "Expected errors for missing ':-'"
    );
}

#[test]
fn invalid_rule_missing_period() {
    let input = "UserLogin(username, session_id) :- User(user_id, username, _)";
    let parsed = parse(input);
    assert!(
        !parsed.errors().is_empty(),
        "Expected errors for missing period at end"
    );
}

#[test]
fn invalid_rule_garbage() {
    let input = "This is not a rule!";
    let parsed = parse(input);
    assert!(
        !parsed.errors().is_empty(),
        "Expected errors for completely invalid input"
    );
}

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
fn extern_function_missing_colon() -> &'static str {
    "extern function missing_colon u32\n"
}

#[fixture]
fn function_unterminated_body() -> &'static str {
    "function foo() {"
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
    "function complex(p: (u32, (u8, string))): bool {\n}\n"
}

#[fixture]
fn function_ws_comments() -> &'static str {
    "function  spaced  (  x : string )  :  u8 { /*empty*/ }\n"
}

#[fixture]
fn function_unclosed_params() -> &'static str {
    "function bad(a: string { }\n"
}

#[rstest]
#[expect(clippy::expect_used, reason = "Using expect for clearer test failures")]
fn extern_function_parsed(extern_function: &str) {
    let parsed = parse(extern_function);
    assert!(parsed.errors().is_empty());
    let funcs = parsed.root().functions();
    assert_eq!(funcs.len(), 1);
    let func = funcs.first().expect("function missing");
    assert_eq!(func.name(), Some("hash".into()));
    assert!(func.is_extern());
    assert_eq!(func.parameters(), vec![("data".into(), "string".into())]);
    assert_eq!(func.return_type(), Some("u64".into()));
    assert_eq!(pretty_print(func.syntax()), extern_function);
}

#[rstest]
#[expect(clippy::expect_used, reason = "Using expect for clearer test failures")]
fn function_with_body_parsed(function_with_body: &str) {
    let parsed = parse(function_with_body);
    assert!(parsed.errors().is_empty());
    let funcs = parsed.root().functions();
    assert_eq!(funcs.len(), 1);
    let func = funcs.first().expect("function missing");
    assert_eq!(func.name(), Some("to_uppercase".into()));
    assert!(!func.is_extern());
    assert_eq!(func.parameters(), vec![("s".into(), "string".into())]);
    assert_eq!(func.return_type(), Some("string".into()));
    assert_eq!(pretty_print(func.syntax()), function_with_body);
}

#[rstest]
#[expect(clippy::expect_used, reason = "Using expect for clearer test failures")]
fn function_no_return_parsed(function_no_return: &str) {
    let parsed = parse(function_no_return);
    assert!(parsed.errors().is_empty());
    let funcs = parsed.root().functions();
    assert_eq!(funcs.len(), 1);
    let func = funcs.first().expect("function missing");
    assert_eq!(func.name(), Some("log_message".into()));
    assert!(!func.is_extern());
    assert_eq!(func.parameters(), vec![("msg".into(), "string".into())]);
    assert_eq!(func.return_type(), None);
    assert_eq!(pretty_print(func.syntax()), function_no_return);
}

#[rstest]
#[expect(clippy::expect_used, reason = "Using expect for clearer test failures")]
fn function_no_params_parsed(function_no_params: &str) {
    let parsed = parse(function_no_params);
    assert!(parsed.errors().is_empty());
    let funcs = parsed.root().functions();
    assert_eq!(funcs.len(), 1);
    let func = funcs.first().expect("function missing");
    assert_eq!(func.name(), Some("greet".into()));
    assert_eq!(func.parameters(), Vec::<(String, String)>::new());
    assert_eq!(func.return_type(), Some("string".into()));
}

#[rstest]
#[expect(clippy::expect_used, reason = "Using expect for clearer test failures")]
fn function_multi_params_parsed(function_multi_params: &str) {
    let parsed = parse(function_multi_params);
    assert!(parsed.errors().is_empty());
    let funcs = parsed.root().functions();
    assert_eq!(funcs.len(), 1);
    let func = funcs.first().expect("function missing");
    assert_eq!(
        func.parameters(),
        vec![("a".into(), "string".into()), ("b".into(), "string".into()),]
    );
    assert_eq!(func.return_type(), Some("string".into()));
}

#[rstest]
#[expect(clippy::expect_used, reason = "Using expect for clearer test failures")]
fn function_complex_params_parsed(function_complex_params: &str) {
    let parsed = parse(function_complex_params);
    assert!(parsed.errors().is_empty());
    let funcs = parsed.root().functions();
    assert_eq!(funcs.len(), 1);
    let func = funcs.first().expect("function missing");
    assert_eq!(
        func.parameters(),
        vec![("p".into(), "(u32, (u8, string))".into()),]
    );
    assert_eq!(func.return_type(), Some("bool".into()));
}

#[rstest]
#[expect(clippy::expect_used, reason = "Using expect for clearer test failures")]
fn function_ws_comments_parsed(function_ws_comments: &str) {
    let parsed = parse(function_ws_comments);
    assert!(parsed.errors().is_empty());
    let funcs = parsed.root().functions();
    assert_eq!(funcs.len(), 1);
    let func = funcs.first().expect("function missing");
    assert_eq!(func.name(), Some("spaced".into()));
    assert_eq!(func.parameters(), vec![("x".into(), "string".into())]);
    assert_eq!(func.return_type(), Some("u8".into()));
}

#[rstest]
fn function_unclosed_params_is_error(function_unclosed_params: &str) {
    let parsed = parse(function_unclosed_params);
    assert!(!parsed.errors().is_empty());
    assert!(parsed.root().functions().is_empty());
}

#[rstest]
fn extern_function_missing_colon_is_error(extern_function_missing_colon: &str) {
    let parsed = parse(extern_function_missing_colon);
    assert!(!parsed.errors().is_empty());
    assert!(parsed.root().functions().is_empty());
}

#[rstest]
fn function_unterminated_body_is_error(function_unterminated_body: &str) {
    let parsed = parse(function_unterminated_body);
    assert!(!parsed.errors().is_empty());
    assert!(parsed.root().functions().is_empty());
}
