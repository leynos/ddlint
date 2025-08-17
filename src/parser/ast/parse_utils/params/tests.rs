use super::*;
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

#[rstest]
#[case(
    "function f(a: u32, b: string) {}",
    vec![("a".into(), "u32".into()), ("b".into(), "string".into())],
    0
)]
#[case(
    "input relation R(id: u32, name: string)",
    vec![("id".into(), "u32".into()), ("name".into(), "string".into())],
    0
)]
#[case(
    "function wrap(t: Option<(u32, string)>) {}",
    vec![("t".into(), "Option<(u32,string)>".into())],
    0
)]
#[case(
    "function g(m: Map<string, u64>) {}",
    vec![("m".into(), "Map<string,u64>".into())],
    0
)]
#[case(
    "function nested(p: Vec<Map<string, Vec<u8>>>) {}",
    vec![("p".into(), "Vec<Map<string,Vec<u8>>>".into())],
    0
)]
#[case(
    "function array(a: [Vec<u32>]) {}",
    vec![("a".into(), "[Vec<u32>]".into())],
    0
)]
#[case(
    "function nested_vec(v: Vec<Vec<u8>>) {}",
    vec![("v".into(), "Vec<Vec<u8>>".into())],
    0
)]
#[case(
    "function weird(x: Vec<<u8>>) {}",
    vec![("x".into(), "Vec<<u8>>".into())],
    0
)]
#[case(
    "function with_comment(a /* c */: u32) {}",
    vec![("a".into(), "u32".into())],
    0,
)]
#[case(
    "function with_space(a   : u32) {}",
    vec![("a".into(), "u32".into())],
    0,
)]
#[case("function empty() {}", Vec::new(), 0)]
#[case(
    "function missing(a u32, b: bool) {}",
    vec![("b".into(), "bool".into())],
    1
)]
fn name_type_pairs(
    #[case] src: &str,
    #[case] expected: Vec<(String, String)>,
    #[case] err_count: usize,
    #[with(src)] tokens_for: Vec<SyntaxElement<DdlogLanguage>>,
) {
    let _ = src;
    let elements = tokens_for;
    let (result, errors) = parse_name_type_pairs(elements.into_iter());
    assert_eq!(errors.len(), err_count);
    assert_eq!(result, expected);
}

#[test]
fn unmatched_shift_errors() {
    let src = "function bad(x: Vec<u8>>): bool {}";
    let elements = tokens_for(src);
    let (_pairs, errors) = parse_name_type_pairs(elements.into_iter());
    assert_eq!(errors.len(), 1);
}

#[test]
fn unmatched_bracket_error() {
    let src = "function bad(x: Vec<u8>], y: u32) {}";
    let elements = tokens_for(src);
    let (_pairs, errors) = parse_name_type_pairs(elements.into_iter());
    assert_eq!(errors.len(), 1);
}

#[test]
fn unmatched_brace_error() {
    let src = "function bad(x: u32}, y: bool) {}";
    let elements = tokens_for(src);
    let (_pairs, errors) = parse_name_type_pairs(elements.into_iter());
    assert_eq!(errors.len(), 1);
}

#[test]
fn trailing_closer_after_trivia() {
    let src = "function bad(x: u32)   ) {}";
    let elements = tokens_for(src);
    let (_pairs, errors) = parse_name_type_pairs(elements.into_iter());
    assert_eq!(errors.len(), 1);
}

fn assert_unclosed_angle_span<F, T>(src: &str, parser: F)
where
    F: Fn(Vec<SyntaxElement<DdlogLanguage>>) -> (T, Vec<ParseError>),
{
    let elements = tokens_for(src);
    let (_result, errors) = parser(elements.clone());
    assert_eq!(errors.len(), 1);
    #[expect(clippy::expect_used, reason = "Using expect for clearer test failures")]
    let angle_span = elements
        .iter()
        .find_map(|e| match e {
            SyntaxElement::Token(t) if t.text() == "<" => Some(t.text_range()),
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
fn unclosed_angle_error() {
    assert_unclosed_angle_span("function bad(x: Vec<u32) {}", |elements| {
        parse_name_type_pairs(elements.into_iter())
    });
}

#[rstest]
#[case("function bad(x: (u32 {}", ')', "(")]
#[case("function bad(x: [u32 {}", ']', "[")]
#[case("function bad(x: {u32 {}", '}', "{")]
fn unclosed_other_delims(#[case] src: &str, #[case] expected_delim: char, #[case] opener: &str) {
    let elements = tokens_for(src);
    let (_pairs, errors) = parse_name_type_pairs(elements.clone().into_iter());
    let spans: Vec<TextRange> = elements
        .iter()
        .filter_map(|e| match e {
            SyntaxElement::Token(t) if t.text() == opener => Some(t.text_range()),
            SyntaxElement::Token(_) | SyntaxElement::Node(_) => None,
        })
        .collect();
    let open_span = if opener == "(" {
        #[expect(clippy::expect_used, reason = "Using expect for clearer test failures")]
        spans.last().copied().expect("opening token missing")
    } else {
        #[expect(clippy::expect_used, reason = "Using expect for clearer test failures")]
        spans.first().copied().expect("opening token missing")
    };
    #[expect(clippy::expect_used, reason = "Using expect for clearer test failures")]
    let err = errors
        .iter()
        .find(|e| {
            matches!(
                e,
                ParseError::UnclosedDelimiter { delimiter, .. }
                    if *delimiter == expected_delim
            )
        })
        .expect("expected unclosed delimiter missing");
    match err {
        ParseError::UnclosedDelimiter { span, .. } => {
            assert_eq!(*span, open_span);
        }
        _ => unreachable!(),
    }
}

#[test]
fn empty_name_error() {
    let src = "function bad(: u32) {}";
    let elements = tokens_for(src);
    let (_pairs, errors) = parse_name_type_pairs(elements.into_iter());
    assert_eq!(errors.len(), 1);
}

#[test]
fn empty_type_error() {
    let src = "function bad(x:) {}";
    let elements = tokens_for(src);
    let (_pairs, errors) = parse_name_type_pairs(elements.into_iter());
    assert_eq!(errors.len(), 1);
}
