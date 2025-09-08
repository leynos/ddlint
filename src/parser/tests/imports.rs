//! Import statement parsing tests.
//!
//! Covers standard and aliased imports along with basic error recovery.

use crate::{
    ast::Import,
    parse,
    test_util::{assert_no_parse_errors, assert_parse_error},
};
use rstest::rstest;

use super::helpers::parse_import;

#[rstest]
#[case::standard("import standard_library", "standard_library", None)]
#[case::with_alias(
    "import collections::vector as vec",
    "collections::vector",
    Some("vec")
)]
#[case::multi_segment("import foo::bar::baz", "foo::bar::baz", None)]
#[case::whitespace("  import  foo  as  f  ", "foo", Some("f"))]
fn import_statement_parses(#[case] src: &str, #[case] path: &str, #[case] alias: Option<&str>) {
    let imp = parse_import(src);
    assert_eq!(imp.path(), path);
    assert_eq!(imp.alias().as_deref(), alias);
}

#[rstest]
fn import_statement_invalid_missing_path() {
    let src = "import as missing_path";
    let parsed = parse(src);
    assert_parse_error(parsed.errors(), "identifier", 7, 9);
    let imports = parsed.root().imports();
    assert!(imports.is_empty());
}

#[rstest]
fn import_invalid_then_valid() {
    let src = "import as\nimport foo";
    let parsed = parse(src);
    assert!(!parsed.errors().is_empty());
    let imports = parsed.root().imports();
    assert_eq!(imports.len(), 1);
    assert_eq!(imports.first().map(Import::path), Some("foo".to_string()));
}

#[rstest]
fn import_multiple_statements() {
    let src = "import a\nimport b as c";
    let parsed = parse(src);
    assert_no_parse_errors(parsed.errors());
    let imports = parsed.root().imports();
    let paths: Vec<_> = imports.iter().map(|i| (i.path(), i.alias())).collect();
    assert_eq!(paths, [("a".into(), None), ("b".into(), Some("c".into()))]);
}
