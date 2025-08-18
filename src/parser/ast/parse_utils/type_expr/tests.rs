use super::super::errors::ParseError;
use super::super::outputs::skip_to_top_level_colon;
use super::*;
use crate::SyntaxKind;
use crate::parser::ast::AstNode;
use crate::parser::parse;
use rowan::SyntaxElement;
use rstest::{fixture, rstest};

#[fixture]
fn tokens_for(#[default("function t() {}")] src: &str) -> Vec<SyntaxElement<DdlogLanguage>> {
    let parsed = parse(src);
    let functions = parsed.root().functions();
    functions.first().map_or_else(
        || {
            let relations = parsed.root().relations();
            relations.first().map_or_else(
                || parsed.root().syntax().children_with_tokens().collect(),
                |rel| rel.syntax().children_with_tokens().collect(),
            )
        },
        |func| func.syntax().children_with_tokens().collect(),
    )
}

#[fixture]
fn return_type_for(#[default("function t() {}")] src: &str) -> Option<String> {
    let parsed = parse(src);
    let functions = parsed.root().functions();
    #[expect(clippy::expect_used, reason = "Using expect for clearer test failures")]
    let func = functions.first().expect("function missing");
    let mut iter = func.syntax().children_with_tokens().peekable();
    let mut depth = 0usize;
    for e in &mut iter {
        match e.kind() {
            SyntaxKind::T_LPAREN => depth += 1,
            SyntaxKind::T_RPAREN => {
                depth = depth.saturating_sub(1);
                if depth == 0 {
                    break;
                }
            }
            _ => {}
        }
    }
    parse_type_after_colon(&mut iter)
}

#[test]
fn type_expr_unclosed_delimiter_span() {
    let src = "function bad(x: Vec<u32) {}";
    let elements = tokens_for(src);
    let mut iter = elements.clone().into_iter().peekable();
    for e in iter.by_ref() {
        if e.kind() == SyntaxKind::T_LPAREN {
            break;
        }
    }
    skip_to_top_level_colon(&mut iter);
    let (_ty, errors) = parse_type_expr(&mut iter);
    assert_eq!(errors.len(), 1);
    #[expect(clippy::expect_used, reason = "Using expect for clearer test failures")]
    let angle_span = elements
        .iter()
        .find_map(|e| match e {
            SyntaxElement::Token(t) if t.kind() == SyntaxKind::T_LT => Some(t.text_range()),
            _ => None,
        })
        .expect("angle token missing");
    #[expect(clippy::expect_used, reason = "Using expect for clearer test failures")]
    let err = errors.first().expect("no error");
    match err {
        ParseError::UnclosedDelimiter {
            delimiter: '>',
            span,
        } => {
            assert_eq!(*span, angle_span);
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn mismatched_closing_delimiter_records_error() {
    let src = "function f(): u32> {}";
    let mut iter = tokens_for(src).into_iter().peekable();
    skip_to_top_level_colon(&mut iter);
    let (_ty, errors) = parse_type_expr(&mut iter);
    assert!(
        errors.iter().any(|e| matches!(e, ParseError::Delimiter(_))),
        "expected delimiter error, got: {:?}",
        errors
    );
}

#[rstest]
#[case("function f(): u32 {}", Some("u32".to_string()))]
#[case("extern function f(): bool;", Some("bool".to_string()))]
#[case("function f() {}", None)]
#[case("function f():\n    u32 {}", Some("u32".to_string()))]
#[case("function f(): /* c */ u32 {}", Some("u32".to_string()))]
#[case("function f(): {}", None)]
#[case(
    "extern function f(): Map<string, Vec<(u32, Option<bool>)>>;",
    Some("Map<string,Vec<(u32,Option<bool>)>>".to_string()),
)]
#[case(
    "extern function f(): Map<Vec<u32>, (bool, [string])>;",
    Some("Map<Vec<u32>,(bool,[string])>".to_string()),
)]
#[case(
    "extern function f(): Map<Vec<Map<string, (u32, [Option<bool>])>>,(bool,[Map<string,u8>])>;",
    Some(
        "Map<Vec<Map<string,(u32,[Option<bool>])>>,(bool,[Map<string,u8>])>".to_string(),
    ),
)]
fn trailing_type(
    #[case] src: &str,
    #[case] expected: Option<String>,
    #[with(src)] return_type_for: Option<String>,
) {
    let _ = src;
    assert_eq!(return_type_for, expected);
}
