//! Utilities for parsing parameter name and type pairs.
//!
//! Functions in this module walk the CST tokens for a parameter list and
//! return any `(name, type)` pairs alongside parsing errors. Delimiter checking
//! is delegated to `token_utils` so that delimiter handling remains
//! consistent across modules.

use rowan::{SyntaxElement, TextRange, TextSize};

use crate::{DdlogLanguage, SyntaxKind};

use super::super::skip_whitespace_and_comments;
use super::{
    errors::{Delim, ParseError},
    token_utils::push_error,
    type_expr::parse_type_expr,
};

/// Parse comma separated `name: type` pairs enclosed in parentheses.
///
/// The iterator may contain leading tokens before the opening `(`. Any
/// unmatched closing delimiters following the list are reported as errors.
pub(crate) fn parse_name_type_pairs<I>(iter: I) -> (Vec<(String, String)>, Vec<ParseError>)
where
    I: Iterator<Item = SyntaxElement<DdlogLanguage>>,
{
    use rowan::NodeOrToken;

    let mut iter = iter.peekable();

    for e in &mut iter {
        if e.kind() == SyntaxKind::T_LPAREN {
            break;
        }
    }

    let mut pairs = Vec::new();
    let mut errors = Vec::new();

    loop {
        skip_whitespace_and_comments(&mut iter);

        match iter.peek() {
            Some(NodeOrToken::Token(t)) if t.kind() == SyntaxKind::T_RPAREN => {
                iter.next();
                break;
            }
            None => break,
            _ => {}
        }

        let (name, found_colon, span) = collect_parameter_name(&mut iter);
        if let Some(pair) = finalise_parameter(name, found_colon, span, &mut iter, &mut errors) {
            pairs.push(pair);
        }

        skip_whitespace_and_comments(&mut iter);
        match iter.peek() {
            Some(NodeOrToken::Token(t)) if t.kind() == SyntaxKind::T_COMMA => {
                iter.next();
            }
            Some(NodeOrToken::Token(t)) if t.kind() == SyntaxKind::T_RPAREN => {
                iter.next();
                break;
            }
            _ => {}
        }
    }

    for e in iter {
        if let NodeOrToken::Token(t) = e {
            match t.kind() {
                SyntaxKind::T_RPAREN => push_error(&mut errors, Delim::Paren, &t),
                SyntaxKind::T_RBRACKET => push_error(&mut errors, Delim::Bracket, &t),
                SyntaxKind::T_RBRACE => push_error(&mut errors, Delim::Brace, &t),
                SyntaxKind::T_GT | SyntaxKind::T_SHR => push_error(&mut errors, Delim::Angle, &t),
                _ => break,
            }
        } else {
            break;
        }
    }

    (pairs, errors)
}

fn track_span_for_element(
    element: &SyntaxElement<DdlogLanguage>,
    start_pos: &mut Option<TextSize>,
    end_pos: &mut Option<TextSize>,
) {
    let range = element.text_range();
    if start_pos.is_none() {
        *start_pos = Some(range.start());
    }
    *end_pos = Some(range.end());
}

fn collect_parameter_name<I>(iter: &mut std::iter::Peekable<I>) -> (String, bool, Option<TextRange>)
where
    I: Iterator<Item = SyntaxElement<DdlogLanguage>>,
{
    use rowan::NodeOrToken;

    let mut name_buf = String::new();
    let mut found_colon = false;
    let mut start_pos: Option<TextSize> = None;
    let mut end_pos: Option<TextSize> = None;

    while let Some(e) = iter.peek() {
        match e {
            NodeOrToken::Token(t) => match t.kind() {
                SyntaxKind::T_COLON => {
                    track_span_for_element(e, &mut start_pos, &mut end_pos);
                    iter.next();
                    found_colon = true;
                    break;
                }
                SyntaxKind::T_COMMA | SyntaxKind::T_RPAREN => {
                    track_span_for_element(e, &mut start_pos, &mut end_pos);
                    break;
                }
                _ => {
                    track_span_for_element(e, &mut start_pos, &mut end_pos);
                    name_buf.push_str(t.text());
                    iter.next();
                }
            },
            NodeOrToken::Node(_) => {
                track_span_for_element(e, &mut start_pos, &mut end_pos);
                if let NodeOrToken::Node(n) = e {
                    name_buf.push_str(&n.text().to_string());
                }
                iter.next();
            }
        }
    }

    let span = match (start_pos, end_pos) {
        (Some(start), Some(end)) => Some(TextRange::new(start, end)),
        _ => None,
    };
    (name_buf.trim().to_string(), found_colon, span)
}

fn finalise_parameter<I>(
    name: String,
    found_colon: bool,
    span: Option<TextRange>,
    iter: &mut std::iter::Peekable<I>,
    errors: &mut Vec<ParseError>,
) -> Option<(String, String)>
where
    I: Iterator<Item = SyntaxElement<DdlogLanguage>>,
{
    if !found_colon {
        if let Some(s) = span {
            errors.push(ParseError::MissingColon {
                message: "Expected ':' after name, but found ',' or ')' instead".to_string(),
                span: s,
            });
        }

        while let Some(e) = iter.peek() {
            match e.kind() {
                SyntaxKind::T_COMMA => {
                    iter.next();
                    break;
                }
                SyntaxKind::T_RPAREN => {
                    iter.next();
                    return None;
                }
                _ => {
                    iter.next();
                }
            }
        }
        return None;
    }

    skip_whitespace_and_comments(iter);
    let (ty, mut errs) = parse_type_expr(iter);
    errors.append(&mut errs);

    if name.is_empty()
        && !ty.is_empty()
        && let Some(s) = span
    {
        errors.push(ParseError::MissingName { span: s });
    }
    if !name.is_empty()
        && ty.is_empty()
        && let Some(s) = span
    {
        errors.push(ParseError::MissingType { span: s });
    }

    if !name.is_empty() && !ty.is_empty() {
        Some((name, ty))
    } else {
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::ast::AstNode;
    use crate::parser::parse;
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
        vec![("t".into(), "Option<(u32, string)>".into())],
        0
    )]
    #[case(
        "function g(m: Map<string, u64>) {}",
        vec![("m".into(), "Map<string, u64>".into())],
        0
    )]
    #[case(
        "function nested(p: Vec<Map<string, Vec<u8>>>) {}",
        vec![("p".into(), "Vec<Map<string, Vec<u8>>>".into())],
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
    #[case("function empty() {}", Vec::new(), 0)]
    #[case(
        "function missing(a u32, b: bool) {}",
        vec![("b".into(), "bool".into())],
        1
    )]
    fn parse_pairs(
        #[case] src: &str,
        #[case] expected: Vec<(String, String)>,
        #[case] error_count: usize,
        #[with(src)] tokens_for: Vec<SyntaxElement<DdlogLanguage>>,
    ) {
        let _ = src;
        let (pairs, errors) = parse_name_type_pairs(tokens_for.into_iter());
        assert_eq!(pairs, expected);
        assert_eq!(errors.len(), error_count);
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
    fn unclosed_other_delims(
        #[case] src: &str,
        #[case] expected_delim: char,
        #[case] opener: &str,
    ) {
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
                    ParseError::UnclosedDelimiter { delimiter, .. } if *delimiter == expected_delim
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
    fn type_expr_unclosed_delimiter_span() {
        assert_unclosed_angle_span("function bad(x: Vec<u32) {}", |elements| {
            let mut iter = elements.into_iter().peekable();
            for e in iter.by_ref() {
                if e.kind() == SyntaxKind::T_LPAREN {
                    break;
                }
            }
            // Advance to the parameter type position.
            skip_to_top_level_colon(&mut iter);
            parse_type_expr(&mut iter)
        });
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
}
