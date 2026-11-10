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

/// A line-comment token stops before its newline.
///
/// Scanner boundaries treat a newline-bearing token as a statement break, so
/// the comment must leave the newline in a separate token for
/// `starts_statement_boundary` to see it.
#[test]
fn line_comment_token_excludes_its_newline() {
    let tokens = tokenize_with_trivia("// note\nx");
    assert_eq!(tokens.first(), Some(&(SyntaxKind::T_COMMENT, 0..7)));
    assert_eq!(tokens.get(1), Some(&(SyntaxKind::T_WHITESPACE, 7..8)));
    assert_eq!(
        tokens.get(2).map(|(kind, _)| *kind),
        Some(SyntaxKind::T_IDENT)
    );
}

/// `\r\n` is a single whitespace token rather than two.
#[test]
fn crlf_is_one_whitespace_token() {
    assert_eq!(
        tokenize_with_trivia("a\r\nb"),
        vec![
            (SyntaxKind::T_IDENT, 0..1),
            (SyntaxKind::T_WHITESPACE, 1..3),
            (SyntaxKind::T_IDENT, 3..4),
        ]
    );
}
