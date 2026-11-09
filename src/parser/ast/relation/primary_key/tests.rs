//! Unit tests for typed primary-key clause access.

use googletest::{Result, prelude::*};
use pretty_assertions::assert_eq;
use rowan::{GreenNodeBuilder, Language, SyntaxNode};
use rstest::rstest;

use super::{PrimaryKey, PrimaryKeyExpression, Relation};
use crate::{DdlogLanguage, SyntaxKind, parse, parser::expression::parse_expression};

/// Return the first relation parsed from `src`.
fn relation(src: &str) -> Option<Relation> { parse(src).root().relations().into_iter().next() }

/// Return the primary key of `src` when the clause is present and valid.
fn primary_key_of(src: &str) -> Option<PrimaryKey> { relation(src)?.primary_key().ok()? }

/// Build a synthetic relation node from raw tokens.
fn relation_node(tokens: &[(SyntaxKind, &str)]) -> SyntaxNode<DdlogLanguage> {
    let mut builder = GreenNodeBuilder::new();
    builder.start_node(DdlogLanguage::kind_to_raw(SyntaxKind::N_RELATION_DECL));
    for (kind, text) in tokens {
        builder.token(DdlogLanguage::kind_to_raw(*kind), text);
    }
    builder.finish_node();
    SyntaxNode::new_root(builder.finish())
}

#[gtest]
#[rstest]
#[case::single(
    "input relation R(x: u32) primary key (x)",
    &["x"],
    "primary key (x)",
    None
)]
#[case::several(
    "input relation R(x: u32, y: string) primary key (x, y)",
    &["x", "y"],
    "primary key (x, y)",
    None
)]
#[case::spec_form(
    "input relation Book(row: BookRow) primary key (row) (row.author, row.title)",
    &["row"],
    "primary key (row) (row.author, row.title)",
    Some("(row.author, row.title)")
)]
fn exposes_binders_and_clause_span(
    #[case] src: &str,
    #[case] expected_binders: &[&str],
    #[case] expected_span: &str,
    #[case] expected_expression: Option<&str>,
) -> Result<()> {
    let key = primary_key_of(src).expect("primary key");

    verify_that!(key.binder_names(), eq(expected_binders))?;
    verify_that!(src.get(key.span()), eq(Some(expected_span)))?;
    verify_that!(
        key.expression().map(PrimaryKeyExpression::text),
        eq(expected_expression)
    )?;
    Ok(())
}

#[rstest]
#[case::space_before_comma("input relation R(a: u32, b: u32) primary key (a , b)")]
#[case::comment_between_name_and_comma(
    "input relation R(a: u32, b: u32) primary key (a /* first */, b)"
)]
fn binder_names_and_spans_exclude_surrounding_trivia(#[case] src: &str) {
    let key = primary_key_of(src).expect("primary key");

    assert_eq!(key.binder_names(), ["a", "b"]);
    for binder in key.binders() {
        assert_eq!(src.get(binder.span()), Some(binder.name()));
    }
}

/// A relation without a readable clause exposes no primary key.
///
/// `primary_key()` returns `Ok(None)` for a relation that has no clause and
/// for one whose clause the scanner could not attach to the relation node.
#[rstest]
#[case::no_clause("input relation R(x: u32)")]
#[case::bracket_body("input R[Row] primary key (row)")]
#[case::output_relation("output relation R(x: u32) primary key (x)")]
fn primary_key_is_absent(#[case] src: &str) {
    assert!(matches!(
        relation(src).expect("relation").primary_key(),
        Ok(None)
    ));
}

#[test]
fn trailing_line_comment_is_not_part_of_the_expression() {
    let src = "input relation R(x: R) primary key (x) x.id // note";
    let key = primary_key_of(src).expect("primary key");
    let expression = key.expression().expect("expression");

    assert_eq!(expression.text(), "x.id");
    assert_eq!(src.get(expression.span()), Some("x.id"));
}

#[test]
fn interior_block_comment_is_kept_in_the_expression() {
    let src = "input relation R(x: R) primary key (x) (x.a, /* keep */ x.b)";
    let key = primary_key_of(src).expect("primary key");
    let expression = key.expression().expect("expression");

    assert_eq!(expression.text(), "(x.a, /* keep */ x.b)");
}

#[test]
fn leading_comment_does_not_enter_the_expression_span() {
    let src = "input relation R(x: R) primary key (x) /* c */ x + * 1";
    let key = primary_key_of(src).expect("primary key");
    let expression = key.expression().expect("expression");

    assert_eq!(expression.span(), 47..54);
    assert_eq!(expression.text(), "x + * 1");
}

#[test]
fn expression_errors_are_shifted_into_the_clause() {
    let src = "input relation R(x: R) primary key (x) /* c */ x + * 1";
    let key = primary_key_of(src).expect("primary key");
    let expression = key.expression().expect("expression");
    let relative = parse_expression("x + * 1").expect_err("expression should be invalid");
    let absolute = expression
        .parse()
        .expect_err("expression should be invalid");

    assert_eq!(absolute.len(), relative.len());
    for (actual, expected) in absolute.iter().zip(&relative) {
        assert_eq!(actual.span().start, expected.span().start + 47);
        assert_eq!(actual.span().end, expected.span().end + 47);
    }
}

#[test]
fn multiline_expression_inside_delimiters_is_captured() {
    let src = "input relation R(x: R) primary key (x) (x.a,\n x.b)";
    let key = primary_key_of(src).expect("primary key");
    let expression = key.expression().expect("expression");

    assert_eq!(expression.text(), "(x.a,\n x.b)");
    assert_eq!(src.get(expression.span()), Some(expression.text()));
}

#[test]
fn expression_at_end_of_file_without_newline_is_captured() {
    let src = "input relation R(x: R) primary key (x) x.id";
    let key = primary_key_of(src).expect("primary key");
    let expression = key.expression().expect("expression");

    assert_eq!(expression.text(), "x.id");
    assert!(parse_expression(expression.text()).is_ok());
}

#[test]
fn unclosed_binder_list_is_error() {
    let node = relation_node(&[
        (SyntaxKind::T_IDENT, "R"),
        (SyntaxKind::T_LPAREN, "("),
        (SyntaxKind::T_IDENT, "x"),
        (SyntaxKind::T_COLON, ":"),
        (SyntaxKind::T_WHITESPACE, " "),
        (SyntaxKind::T_IDENT, "u32"),
        (SyntaxKind::T_RPAREN, ")"),
        (SyntaxKind::T_WHITESPACE, " "),
        (SyntaxKind::T_IDENT, "primary"),
        (SyntaxKind::T_WHITESPACE, " "),
        (SyntaxKind::T_IDENT, "key"),
        (SyntaxKind::T_WHITESPACE, " "),
        (SyntaxKind::T_LPAREN, "("),
    ]);
    let relation = Relation { syntax: node };

    assert!(relation.primary_key().is_err());
}

#[test]
fn missing_key_keyword_is_error() {
    let node = relation_node(&[
        (SyntaxKind::T_IDENT, "R"),
        (SyntaxKind::T_LPAREN, "("),
        (SyntaxKind::T_IDENT, "x"),
        (SyntaxKind::T_COLON, ":"),
        (SyntaxKind::T_WHITESPACE, " "),
        (SyntaxKind::T_IDENT, "u32"),
        (SyntaxKind::T_RPAREN, ")"),
        (SyntaxKind::T_WHITESPACE, " "),
        (SyntaxKind::T_IDENT, "primary"),
        (SyntaxKind::T_WHITESPACE, " "),
        (SyntaxKind::T_LPAREN, "("),
        (SyntaxKind::T_IDENT, "x"),
        (SyntaxKind::T_RPAREN, ")"),
    ]);
    let relation = Relation { syntax: node };
    let errors = relation
        .primary_key()
        .expect_err("clause should be malformed");

    assert!(
        errors
            .iter()
            .any(|message| message.contains("expected `key`")),
        "unexpected errors: {errors:?}"
    );
}

#[test]
fn renders_probe_corpus_as_binders_text_span_and_sexpr() {
    let corpus = [
        "input relation R(x: u32) primary key (x)",
        "input relation R(x: R) primary key (x) x.id",
        "input relation Book(row: BookRow) primary key (row) (row.author, row.title)",
    ];
    let rendered = corpus
        .iter()
        .map(|src| render_key(src))
        .collect::<Option<Vec<_>>>()
        .expect("corpus should parse")
        .join("\n");

    insta::assert_snapshot!(rendered, @r###"
    x | <none> | 25..40 | <none>
    x | x.id | 39..43 | (field x id)
    row | (row.author, row.title) | 52..75 | (tuple (field row author) (field row title))
    "###);
}

/// Render one source as `binders | text | span | s-expression`.
fn render_key(src: &str) -> Option<String> {
    let key = primary_key_of(src)?;
    let binders = key.binder_names().join(",");
    let Some(expression) = key.expression() else {
        return Some(format!("{binders} | <none> | {:?} | <none>", key.span()));
    };
    let sexpr = expression
        .parse()
        .map_or_else(|_| "<error>".to_owned(), |expr| expr.to_sexpr());
    Some(format!(
        "{binders} | {} | {:?} | {sexpr}",
        expression.text(),
        expression.span()
    ))
}
