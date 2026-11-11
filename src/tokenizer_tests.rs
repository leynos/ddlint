//! Tests for the tokenizer.
use rstest::rstest;

use super::*;

#[rstest]
#[case::internal_identifier("internal", SyntaxKind::T_IDENT)]
#[case::stream_keyword("stream", SyntaxKind::K_STREAM)]
#[case::multiset_keyword("multiset", SyntaxKind::K_MULTISET)]
fn tokenize_relation_role_and_kind_words(#[case] src: &str, #[case] expected: SyntaxKind) {
    let tokens = tokenize_without_trivia(src);
    let kinds: Vec<SyntaxKind> = tokens.iter().map(|(kind, _)| *kind).collect();
    assert_eq!(kinds, vec![expected]);
}

#[test]
fn tokenize_recognises_ampersand_tokens() {
    let tokens = tokenize_without_trivia("&a & b");
    let kinds: Vec<SyntaxKind> = tokens.iter().map(|(k, _)| *k).collect();
    assert_eq!(
        kinds,
        vec![
            SyntaxKind::T_AMP,
            SyntaxKind::T_IDENT,
            SyntaxKind::T_AMP,
            SyntaxKind::T_IDENT,
        ]
    );
}

#[test]
fn tokenize_recognises_ampersand_adjacency() {
    let tokens = tokenize_without_trivia("&a&b");
    let kinds: Vec<SyntaxKind> = tokens.iter().map(|(k, _)| *k).collect();
    assert_eq!(
        kinds,
        vec![
            SyntaxKind::T_AMP,
            SyntaxKind::T_IDENT,
            SyntaxKind::T_AMP,
            SyntaxKind::T_IDENT,
        ]
    );
}

#[test]
fn tokenize_recognises_dual_use_ampersand_sequence() {
    let tokens = tokenize_without_trivia("a& &b");
    let kinds: Vec<SyntaxKind> = tokens.iter().map(|(k, _)| *k).collect();
    assert_eq!(
        kinds,
        vec![
            SyntaxKind::T_IDENT,
            SyntaxKind::T_AMP,
            SyntaxKind::T_AMP,
            SyntaxKind::T_IDENT,
        ]
    );
}

#[test]
fn tokenize_recognises_caret_tokens() {
    let tokens = tokenize_without_trivia("a ^ b");
    let kinds: Vec<SyntaxKind> = tokens.iter().map(|(k, _)| *k).collect();
    assert_eq!(
        kinds,
        vec![
            SyntaxKind::T_IDENT,
            SyntaxKind::T_CARET,
            SyntaxKind::T_IDENT,
        ]
    );
}

/// Trivia-bearing token boundaries that scanner code depends on.
///
/// A line-comment token must stop before its `\n` so the newline stays in a
/// separate token and a statement break remains visible. A CRLF line ending is
/// split rather than excluded: the comment token keeps the `\r` and the `\n`
/// begins the following whitespace token, which may also absorb indentation.
/// Away from comments, `\r\n` stays one whitespace token so a carriage-return
/// pair is never split.
#[rstest]
#[case::line_comment_excludes_its_newline(
    "// note\nx",
    vec![
        (SyntaxKind::T_COMMENT, 0..7),
        (SyntaxKind::T_WHITESPACE, 7..8),
        (SyntaxKind::T_IDENT, 8..9),
    ]
)]
#[case::comment_before_crlf_keeps_the_carriage_return(
    "// note\r\nx",
    vec![
        (SyntaxKind::T_COMMENT, 0..8),
        (SyntaxKind::T_WHITESPACE, 8..9),
        (SyntaxKind::T_IDENT, 9..10),
    ]
)]
#[case::crlf_whitespace_absorbs_following_indentation(
    "// note\r\n  x",
    vec![
        (SyntaxKind::T_COMMENT, 0..8),
        (SyntaxKind::T_WHITESPACE, 8..11),
        (SyntaxKind::T_IDENT, 11..12),
    ]
)]
#[case::crlf_is_one_whitespace_token(
    "a\r\nb",
    vec![
        (SyntaxKind::T_IDENT, 0..1),
        (SyntaxKind::T_WHITESPACE, 1..3),
        (SyntaxKind::T_IDENT, 3..4),
    ]
)]
fn trivia_boundaries_are_single_tokens(
    #[case] src: &str,
    #[case] expected: Vec<(SyntaxKind, Span)>,
) {
    assert_eq!(tokenize_with_trivia(src), expected);
}
