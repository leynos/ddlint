//! Name uniqueness validation across all top-level declarations.
//!
//! Ensures each type, relation, index, transformer, and import name is
//! defined at most once, and each function `(name, arity)` pair is unique.
//! Functions may be overloaded by arity per spec §8.

use std::collections::HashMap;

use chumsky::error::Simple;

use crate::parser::ast::rule::text_range_to_span;
use crate::parser::ast::{AstNode, Root};
use crate::{Span, SyntaxKind};

/// Validate that no two top-level declarations share the same name.
///
/// Returns a list of diagnostics, one per duplicate. The first occurrence
/// of each name is kept; subsequent occurrences are reported.
///
/// # Examples
///
/// ```rust,ignore
/// use ddlint::parse;
/// use ddlint::parser::validators::name_uniqueness::validate_name_uniqueness;
///
/// let parsed = parse("typedef A = u32\ntypedef A = string");
/// let errors = validate_name_uniqueness(parsed.root());
/// assert_eq!(errors.len(), 1);
/// ```
pub(crate) fn validate_name_uniqueness(root: &Root) -> Vec<Simple<SyntaxKind>> {
    let mut errors = Vec::new();

    check_unique(
        &mut errors,
        "type",
        root.type_defs().iter().filter_map(|td| {
            td.name()
                .map(|n| (n, text_range_to_span(td.syntax().text_range())))
        }),
    );

    check_unique(
        &mut errors,
        "relation",
        root.relations().iter().filter_map(|r| {
            r.name()
                .map(|n| (n, text_range_to_span(r.syntax().text_range())))
        }),
    );

    check_unique(
        &mut errors,
        "index",
        root.indexes().iter().filter_map(|i| {
            i.name()
                .map(|n| (n, text_range_to_span(i.syntax().text_range())))
        }),
    );

    check_unique(
        &mut errors,
        "transformer",
        root.transformers().iter().filter_map(|t| {
            t.name()
                .map(|n| (n, text_range_to_span(t.syntax().text_range())))
        }),
    );

    check_unique(
        &mut errors,
        "import",
        root.imports().iter().map(|imp| {
            let path = imp.path();
            (path, text_range_to_span(imp.syntax().text_range()))
        }),
    );

    check_function_uniqueness(&mut errors, root);

    errors
}

/// Check for duplicate names within a single item category.
fn check_unique(
    errors: &mut Vec<Simple<SyntaxKind>>,
    category: &str,
    items: impl Iterator<Item = (String, Span)>,
) {
    use std::collections::hash_map::Entry;
    let mut seen: HashMap<String, Span> = HashMap::new();
    for (name, span) in items {
        // Defensive: `.name()` may return `Some("")` for a malformed AST
        // node. Skip rather than panic or flag an empty identifier as a
        // duplicate.
        if name.is_empty() {
            continue;
        }
        match seen.entry(name) {
            Entry::Occupied(e) => {
                let name = e.key();
                errors.push(Simple::custom(
                    span,
                    format!("duplicate {category} name '{name}'"),
                ));
            }
            Entry::Vacant(e) => {
                e.insert(span);
            }
        }
    }
}

/// Check for duplicate `(name, arity)` pairs among functions.
///
/// Functions may be overloaded by arity, so two functions named `f` with
/// different parameter counts are permitted.
fn check_function_uniqueness(errors: &mut Vec<Simple<SyntaxKind>>, root: &Root) {
    use std::collections::hash_map::Entry;
    let mut seen: HashMap<(String, usize), Span> = HashMap::new();
    for func in root.functions() {
        let Some(name) = func.name() else {
            continue;
        };
        let arity = func.parameters().len();
        let span = text_range_to_span(func.syntax().text_range());
        let key = (name.clone(), arity);
        match seen.entry(key) {
            Entry::Occupied(_) => {
                errors.push(Simple::custom(
                    span,
                    format!("duplicate function '{name}' with arity {arity}"),
                ));
            }
            Entry::Vacant(e) => {
                e.insert(span);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::parse;

    /// Render a `Simple` error via `Debug` for pattern matching.
    fn render(err: &chumsky::error::Simple<crate::SyntaxKind>) -> String {
        format!("{err:?}")
    }

    /// Helper to assert a single duplicate-name error with the expected message.
    #[expect(clippy::expect_used, reason = "Using expect for clearer test failures")]
    fn assert_duplicate_name_error(src: &str, expected_message: &str) {
        let parsed = parse(src);
        let errors = super::validate_name_uniqueness(parsed.root());
        assert_eq!(errors.len(), 1);
        let e = errors.first().expect("expected one error");
        assert!(
            render(e).contains(expected_message),
            "unexpected error: {e:?}",
        );
    }

    #[test]
    fn no_duplicates_no_errors() {
        let src = concat!(
            "typedef A = u32\n",
            "typedef B = string\n",
            "input relation R(x: u32)\n",
            "index I on R(x)\n",
        );
        let parsed = parse(src);
        let errors = super::validate_name_uniqueness(parsed.root());
        assert!(errors.is_empty(), "unexpected errors: {errors:?}");
    }

    #[test]
    fn duplicate_typedef_names() {
        assert_duplicate_name_error(
            "typedef A = u32\ntypedef A = string",
            "duplicate type name 'A'",
        );
    }

    #[test]
    fn duplicate_relation_names() {
        assert_duplicate_name_error(
            "input relation R(x: u32)\noutput relation R(y: string)\n",
            "duplicate relation name 'R'",
        );
    }

    #[test]
    fn duplicate_index_names() {
        assert_duplicate_name_error(
            concat!("index I on R(x)\n", "index I on S(y)\n",),
            "duplicate index name 'I'",
        );
    }

    #[test]
    fn duplicate_transformer_names() {
        assert_duplicate_name_error(
            concat!(
                "extern transformer t(x: A): B\n",
                "extern transformer t(y: C): D\n",
            ),
            "duplicate transformer name 't'",
        );
    }

    #[test]
    fn duplicate_import_paths() {
        assert_duplicate_name_error("import foo\nimport foo", "duplicate import name 'foo'");
    }

    #[test]
    fn duplicate_function_name_and_arity() {
        assert_duplicate_name_error(
            "function f(x: u32) {}\nfunction f(y: u32) {}",
            "duplicate function 'f'",
        );
    }

    #[test]
    fn function_overload_different_arity_no_error() {
        let src = concat!("function f(x: u32) {}\n", "function f(x: u32, y: u32) {}\n",);
        let parsed = parse(src);
        let errors = super::validate_name_uniqueness(parsed.root());
        assert!(
            errors.is_empty(),
            "arity overloading should be permitted: {errors:?}"
        );
    }

    #[test]
    fn malformed_item_skipped() {
        // A typedef without a name should not cause a false positive
        let src = "typedef = u32\ntypedef A = string";
        let parsed = parse(src);
        let errors = super::validate_name_uniqueness(parsed.root());
        assert!(
            errors.is_empty(),
            "malformed items should be skipped: {errors:?}"
        );
    }
}
