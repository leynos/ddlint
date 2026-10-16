//! Characterisation tests for the parser's shared lexer helpers.

use super::*;
use chumsky::{
    Parser, Stream,
    error::Simple,
    prelude::{end, just},
};
use rstest::rstest;

use crate::{SyntaxKind, test_util::tokenize};

fn parses_complete<P>(parser: P, src: &str) -> bool
where
    P: Parser<SyntaxKind, (), Error = Simple<SyntaxKind>>,
{
    let tokens = tokenize(src);
    parser
        .then_ignore(end())
        .parse(Stream::from_iter(0..src.len(), tokens.into_iter()))
        .is_ok()
}

fn paren_block() -> impl Parser<SyntaxKind, (), Error = Simple<SyntaxKind>> + Clone {
    balanced_block(SyntaxKind::T_LPAREN, SyntaxKind::T_RPAREN)
}

#[rstest]
#[case(SyntaxKind::T_LPAREN, "(")]
#[case(SyntaxKind::T_RPAREN, ")")]
#[case(SyntaxKind::T_LBRACE, "{")]
#[case(SyntaxKind::T_RBRACE, "}")]
#[case(SyntaxKind::T_LBRACKET, "[")]
#[case(SyntaxKind::T_RBRACKET, "]")]
#[case(SyntaxKind::T_COMMA, ",")]
#[case(SyntaxKind::T_COLON, ":")]
#[case(SyntaxKind::T_SEMI, ";")]
#[case(SyntaxKind::K_IMPORT, "")]
fn display_matches(#[case] kind: SyntaxKind, #[case] expected: &str) {
    assert_eq!(token_display(kind), expected);
}

#[test]
fn inline_ws_consumes_whitespace_and_comments() {
    assert!(parses_complete(
        inline_ws().repeated().ignored(),
        " \t// comment\n"
    ));
}

#[test]
fn ident_parses_with_padding() {
    assert!(parses_complete(ident(), "  foo  "));
}

#[test]
fn atom_accepts_optional_args() {
    assert!(parses_complete(atom(), "foo(bar)"));
}

#[rstest]
#[case("(a(b)c)")]
#[case("(())")]
#[case("(()())")]
fn balanced_block_parses_nested_blocks(#[case] src: &str) {
    assert!(parses_complete(paren_block(), src));
}

#[rstest]
#[case("( a )")]
#[case("(\n// comment\n a \n)")]
fn balanced_block_accepts_whitespace_and_comments(#[case] src: &str) {
    assert!(parses_complete(paren_block(), src));
}

#[test]
fn balanced_block_treats_different_delimiters_as_tokens() {
    let parser = paren_block()
        .then_ignore(just(SyntaxKind::T_RBRACKET))
        .then_ignore(end());
    let tokens = tokenize("([)]");
    let result = parser.parse(Stream::from_iter(0..4, tokens.into_iter()));

    assert!(result.is_ok());
}

#[rstest]
#[case("a)")]
#[case("(a")]
#[case("(a))")]
fn balanced_block_rejects_missing_delimiters_and_trailing_tokens(#[case] src: &str) {
    assert!(!parses_complete(paren_block(), src));
}

#[rstest]
#[case("()")]
#[case("( )")]
fn nonempty_balanced_block_rejects_empty_blocks(#[case] src: &str) {
    let parser = balanced_block_nonempty(SyntaxKind::T_LPAREN, SyntaxKind::T_RPAREN);

    assert!(!parses_complete(parser, src));
}

#[rstest]
#[case("(a)")]
#[case("(())")]
fn nonempty_balanced_block_accepts_tokens_and_nested_blocks(#[case] src: &str) {
    let parser = balanced_block_nonempty(SyntaxKind::T_LPAREN, SyntaxKind::T_RPAREN);

    assert!(parses_complete(parser, src));
}

#[test]
fn balanced_block_parser_can_be_reused_after_a_failure() {
    let parser = paren_block().then_ignore(end());
    let invalid_tokens = tokenize("((a");
    let invalid = parser.parse(Stream::from_iter(0..4, invalid_tokens.into_iter()));
    let valid_tokens = tokenize("(b)");
    let valid = parser.parse(Stream::from_iter(0..3, valid_tokens.into_iter()));

    assert!(invalid.is_err());
    assert!(valid.is_ok());
}

#[test]
fn balanced_block_parses_deep_nesting() {
    let nesting = 256;
    let src = format!("{}a{}", "(".repeat(nesting), ")".repeat(nesting));
    std::thread::Builder::new()
        .stack_size(16 * 1024 * 1024)
        .spawn(move || assert!(parses_complete(paren_block(), &src)))
        .expect("deep nesting test thread should start")
        .join()
        .expect("recursive parser should handle a few hundred nested blocks");
}
