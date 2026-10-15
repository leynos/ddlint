//! Tests for the tokenizer.
use super::*;
use rstest::rstest;

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
