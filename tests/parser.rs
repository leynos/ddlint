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
fn index_declaration_whitespace_variations() {
    let src = "  index  Idx_User_ws \t on\n  User (\n    username  )  ";
    let parsed = parse(src);
    assert!(parsed.errors().is_empty());
    assert_eq!(pretty_print(parsed.root().syntax()), src);
    let indexes = parsed.root().indexes();
    let Some(idx) = indexes.first() else {
        panic!("expected index");
    };
    assert_eq!(idx.name(), Some("Idx_User_ws".into()));
    assert_eq!(idx.relation(), Some("User".into()));
    assert_eq!(idx.columns(), vec![String::from("username")]);
}
