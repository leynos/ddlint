use super::super::errors::{Delim, ParseError};
use super::*;
use crate::SyntaxKind;
use crate::parser::ast::AstNode;
use crate::parser::parse;
use rowan::{SyntaxElement, TextRange};
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
fn unclosed_angle_error() {
    assert_unclosed_angle_span("function bad(x: Vec<u32) {}", |elements| {
        parse_name_type_pairs(elements.into_iter())
    });
}

#[rstest]
#[case("function bad(x: (u32 {}", ')', "(")]
#[case("function bad(x: [u32 {}", ']', "[")]
fn unclosed_other_delims(#[case] src: &str, #[case] expected_delim: char, #[case] opener: &str) {
    let elements = tokens_for(src);
    let (_pairs, errors) = parse_name_type_pairs(elements.clone().into_iter());
    let opener_kind = match opener {
        "(" => SyntaxKind::T_LPAREN,
        "[" => SyntaxKind::T_LBRACKET,
        _ => unreachable!("unsupported opener"),
    };
    let spans: Vec<TextRange> = elements
        .iter()
        .filter_map(|e| match e {
            SyntaxElement::Token(t) if t.kind() == opener_kind => Some(t.text_range()),
            SyntaxElement::Token(_) | SyntaxElement::Node(_) => None,
        })
        .collect();
    let open_span = if opener_kind == SyntaxKind::T_LPAREN {
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

enum ErrorExpectation {
    Delimiter { expected: Delim, found: SyntaxKind },
    MissingName,
    MissingType,
}

impl ErrorExpectation {
    fn assert_matches(&self, err: &ParseError) {
        match (self, err) {
            (Self::Delimiter { expected, found }, ParseError::Delimiter(delim)) => {
                assert_eq!(delim.expected, *expected);
                assert_eq!(delim.found, *found);
            }
            (Self::MissingName, ParseError::MissingName { .. })
            | (Self::MissingType, ParseError::MissingType { .. }) => {}
            (_, other) => panic!("unexpected error: {other:?}"),
        }
    }
}

#[rstest]
#[case(
    "function bad(x: Vec<u8>>): bool {}",
    ErrorExpectation::Delimiter {
        expected: Delim::Angle,
        found: SyntaxKind::T_SHR,
    },
)]
#[case(
    "function bad(x: Vec<u8>], y: u32) {}",
    ErrorExpectation::Delimiter {
        expected: Delim::Bracket,
        found: SyntaxKind::T_RBRACKET,
    },
)]
#[case(
    "function bad(x: u32}, y: bool) {}",
    ErrorExpectation::Delimiter {
        expected: Delim::Brace,
        found: SyntaxKind::T_RBRACE,
    },
)]
#[case(
    "function bad(x: u32)   ) {}",
    ErrorExpectation::Delimiter {
        expected: Delim::Paren,
        found: SyntaxKind::T_RPAREN,
    },
)]
#[case("function bad(: u32) {}", ErrorExpectation::MissingName)]
#[case("function bad(x:) {}", ErrorExpectation::MissingType)]
fn parse_error_cases(
    #[case] src: &str,
    #[case] expectation: ErrorExpectation,
    #[with(src)] tokens_for: Vec<SyntaxElement<DdlogLanguage>>,
) {
    let _ = src;
    let (_pairs, errors) = parse_name_type_pairs(tokens_for.into_iter());
    assert_eq!(errors.len(), 1);
    #[expect(clippy::expect_used, reason = "Using expect for clearer test failures")]
    let err = errors.first().expect("missing error");
    expectation.assert_matches(err);
}

#[rstest]
#[case(
    "function f(x:, y:) {}",
    Some(2),
    |errors: &[ParseError]| errors.iter().all(|e| matches!(e, ParseError::MissingType { .. }))
)]
#[case(
    "function f(:u32, :u64) {}",
    Some(2),
    |errors: &[ParseError]| errors.iter().all(|e| matches!(e, ParseError::MissingName { .. }))
)]
#[case(
    "function f(x: {) {}",
    None,
    |errors: &[ParseError]| errors.iter().any(|e| matches!(e, ParseError::MissingType { .. }))
)]
fn multiple_error_scenarios(
    #[case] src: &str,
    #[case] expected_count: Option<usize>,
    #[case] error_validator: fn(&[ParseError]) -> bool,
    #[with(src)] tokens_for: Vec<SyntaxElement<DdlogLanguage>>,
) {
    let _ = src;
    let (pairs, errors) = parse_name_type_pairs(tokens_for.into_iter());
    assert!(pairs.is_empty());

    if let Some(count) = expected_count {
        assert_eq!(errors.len(), count);
    }

    assert!(error_validator(&errors));
}

#[test]
fn missing_colon_does_not_consume_closing_paren() {
    let src = "function f(x y) {}";
    let elements = tokens_for(src);
    let (pairs, errors) = parse_name_type_pairs(elements.clone().into_iter());
    assert!(pairs.is_empty());
    assert_eq!(errors.len(), 1);
    #[expect(clippy::expect_used, reason = "Using expect for clearer test failures")]
    let err = errors.first().expect("missing error");
    assert!(matches!(err, ParseError::MissingColon { .. }));

    let paren_index = elements
        .iter()
        .position(|e| matches!(e, SyntaxElement::Token(t) if t.kind() == SyntaxKind::T_RPAREN));
    let brace_index = elements
        .iter()
        .position(|e| matches!(e, SyntaxElement::Token(t) if t.kind() == SyntaxKind::T_LBRACE));
    assert!(paren_index.is_some() && brace_index.is_some() && paren_index < brace_index);
}

#[test]
fn missing_closing_paren_error() {
    let src = "function f(x: u32, y: bool {";
    let elements = tokens_for(src);
    let (pairs, errors) = parse_name_type_pairs(elements.clone().into_iter());
    assert!(pairs.is_empty(), "pairs: {pairs:?}");
    assert_eq!(errors.len(), 1);
    #[expect(clippy::expect_used, reason = "Using expect for clearer test failures")]
    let err = errors.first().expect("missing UnclosedDelimiter error");
    match err {
        ParseError::UnclosedDelimiter { delimiter, span } if *delimiter == ')' => {
            #[expect(clippy::expect_used, reason = "Using expect for clearer test failures")]
            let open_span = elements
                .iter()
                .find_map(|e| match e {
                    SyntaxElement::Token(t) if t.kind() == SyntaxKind::T_LPAREN => {
                        Some(t.text_range())
                    }
                    _ => None,
                })
                .expect("opening paren token missing");
            assert_eq!(*span, open_span);
        }
        other => panic!("unexpected error: {other:?}"),
    }
}
