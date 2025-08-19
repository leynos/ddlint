//! Import statement parsing tests.
//!
//! Covers standard and aliased imports along with basic error recovery.

use crate::{SyntaxKind, ast::Import, parse};
use rstest::rstest;

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
