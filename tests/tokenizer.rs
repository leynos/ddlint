//! Integration tests for the tokenizer module.
//!
//! Tests verify that the logos-based lexer correctly tokenises `DDlog` source
//! code into `(SyntaxKind, Span)` pairs, covering keywords, literals, trivia,
//! and error cases.

use ddlint::{SyntaxKind, test_util::tokenize};
use rstest::{fixture, rstest};

#[fixture]
fn simple_input() -> &'static str {
    "input relation R(x: u32)"
}

#[rstest]
#[case("input", vec![SyntaxKind::K_INPUT])]
#[case("relation", vec![SyntaxKind::K_RELATION])]
#[case("R", vec![SyntaxKind::T_IDENT])]
fn single_tokens(#[case] source: &str, #[case] expected: Vec<SyntaxKind>) {
    let tokens = tokenize(source);
    let kinds: Vec<SyntaxKind> = tokens.iter().map(|(k, _)| *k).collect();
    assert_eq!(kinds, expected);
}

mod expect_used {
    #![expect(clippy::expect_used, reason = "tests assert exact behaviour")]

    use super::*;
    use rstest::rstest;

    #[rstest]
    fn token_spans(simple_input: &str) {
        let tokens = tokenize(simple_input);
        for (kind, span) in tokens {
            let text = simple_input
                .get(span.clone())
                .expect("span should be valid for input");
            if let SyntaxKind::K_INPUT = kind {
                assert_eq!(text, "input");
            } else if let SyntaxKind::K_RELATION = kind {
                assert_eq!(text, "relation");
            }
        }
    }

    #[rstest]
    #[case("123", SyntaxKind::T_NUMBER)]
    #[case("\"foo\"", SyntaxKind::T_STRING)]
    fn literal_tokens(#[case] source: &str, #[case] expected: SyntaxKind) {
        let tokens = tokenize(source);
        assert_eq!(tokens.len(), 1);
        let first = tokens
            .first()
            .cloned()
            .expect("tokenizer should produce at least one token");
        assert_eq!(first.0, expected);
    }

    #[rstest]
    #[case("0xFF", SyntaxKind::T_NUMBER)]
    #[case("0b1010", SyntaxKind::T_NUMBER)]
    #[case("0o77", SyntaxKind::T_NUMBER)]
    #[case("1e10", SyntaxKind::T_NUMBER)]
    fn extended_number_tokens(#[case] source: &str, #[case] expected: SyntaxKind) {
        let tokens = tokenize(source);
        assert_eq!(tokens.len(), 1);
        let first = tokens
            .first()
            .cloned()
            .expect("tokenizer should produce at least one token");
        assert_eq!(first.0, expected);
    }

    #[rstest]
    #[case(" ", SyntaxKind::T_WHITESPACE)]
    #[case("\n", SyntaxKind::T_WHITESPACE)]
    #[case("\t", SyntaxKind::T_WHITESPACE)]
    #[case("/* c */", SyntaxKind::T_COMMENT)]
    #[case("// line", SyntaxKind::T_COMMENT)]
    fn trivia_tokens(#[case] source: &str, #[case] expected: SyntaxKind) {
        let tokens = tokenize(source);
        assert_eq!(tokens.len(), 1);
        let first = tokens
            .first()
            .cloned()
            .expect("tokenizer should produce at least one token");
        assert_eq!(first.0, expected);
    }

    #[rstest]
    #[case("$")]
    fn unknown_character_produces_error(#[case] source: &str) {
        // Use the trivia-preserving tokenizer to keep tests consistent with the suite.
        let tokens = tokenize(source);
        assert_eq!(tokens.len(), 1);
        let first = tokens
            .first()
            .cloned()
            .expect("tokenizer should produce at least one token");
        assert_eq!(first.0, SyntaxKind::N_ERROR);
        // Harden the check: the error span should map precisely
        // to the offending lexeme. For single-char invalid inputs
        // like "$" or a stray "!", this must equal the source.
        let span = first.1.clone();
        let slice = source.get(span).expect("span should be valid for source");
        assert_eq!(slice, source);
    }

    // Malformed multi-character operators should surface an error token.
    // When users type a stray '!' not forming '!=', the lexer must
    // emit a single error token covering only '!'. Whitespace or
    // newlines between '!' and '=' must not merge into a valid token.
    #[rstest]
    #[case("!+")]
    #[case("++!")]
    #[case("?!")]
    #[case("! =")]
    #[case("!\n=")]
    #[case("!\t=")]
    #[case("!\r\n=")]
    fn malformed_multi_character_tokens_produce_error(#[case] source: &str) {
        let tokens = tokenize(source);
        let errors: Vec<_> = tokens
            .iter()
            .filter(|(k, _)| *k == SyntaxKind::N_ERROR)
            .collect();
        assert_eq!(errors.len(), 1);
        let span = errors.first().expect("error token should exist").1.clone();
        let slice = source.get(span).expect("span should be valid for source");
        assert_eq!(slice, "!");
    }

    // Removed duplicate multi-character lexeme test; covered by operator_tokens.

    #[test]
    fn unterminated_string_is_error() {
        let tokens = tokenize("\"foo");
        assert_eq!(tokens.len(), 1);
        let first = tokens
            .first()
            .expect("tokenizer should produce at least one token");
        assert_eq!(first.0, SyntaxKind::N_ERROR);
    }

    #[rstest]
    #[case("(", SyntaxKind::T_LPAREN)]
    #[case(")", SyntaxKind::T_RPAREN)]
    #[case(":", SyntaxKind::T_COLON)]
    #[case("::", SyntaxKind::T_COLON_COLON)]
    fn punctuation_tokens(#[case] source: &str, #[case] expected: SyntaxKind) {
        let tokens = tokenize(source);
        assert_eq!(tokens.len(), 1);
        let first = tokens
            .first()
            .cloned()
            .expect("tokenizer should produce at least one token");
        assert_eq!(first.0, expected);
    }

    #[rstest]
    #[case("{", SyntaxKind::T_LBRACE)]
    #[case("}", SyntaxKind::T_RBRACE)]
    #[case("[", SyntaxKind::T_LBRACKET)]
    #[case("]", SyntaxKind::T_RBRACKET)]
    #[case(";", SyntaxKind::T_SEMI)]
    #[case(",", SyntaxKind::T_COMMA)]
    #[case(".", SyntaxKind::T_DOT)]
    fn delimiter_tokens(#[case] source: &str, #[case] expected: SyntaxKind) {
        let tokens = tokenize(source);
        assert_eq!(tokens.len(), 1);
        let first = tokens
            .first()
            .cloned()
            .expect("tokenizer should produce at least one token");
        assert_eq!(first.0, expected);
    }

    #[rstest]
    #[case("|", SyntaxKind::T_PIPE)]
    #[case("&", SyntaxKind::T_AMP)]
    #[case("=", SyntaxKind::T_EQ)]
    #[case("==", SyntaxKind::T_EQEQ)]
    #[case(":-", SyntaxKind::T_IMPLIES)]
    #[case("%", SyntaxKind::T_PERCENT)]
    #[case("*", SyntaxKind::T_STAR)]
    #[case("/", SyntaxKind::T_SLASH)]
    #[case("++", SyntaxKind::T_PLUSPLUS)]
    #[case("+", SyntaxKind::T_PLUS)]
    #[case("-", SyntaxKind::T_MINUS)]
    #[case("->", SyntaxKind::T_ARROW)]
    #[case("=>", SyntaxKind::T_FAT_ARROW)]
    #[case("<=", SyntaxKind::T_LTE)]
    #[case("<=>", SyntaxKind::T_SPACESHIP)]
    #[case(">=", SyntaxKind::T_GTE)]
    #[case("<", SyntaxKind::T_LT)]
    #[case(">", SyntaxKind::T_GT)]
    #[case("!=", SyntaxKind::T_NEQ)]
    #[case(">>", SyntaxKind::T_SHR)]
    #[case("<<", SyntaxKind::T_SHL)]
    #[case("?", SyntaxKind::T_QUESTION)]
    #[case("~", SyntaxKind::T_TILDE)]
    #[case("@", SyntaxKind::T_AT)]
    #[case("#", SyntaxKind::T_HASH)]
    #[case("'", SyntaxKind::T_APOSTROPHE)]
    fn operator_tokens(#[case] source: &str, #[case] expected: SyntaxKind) {
        let tokens = tokenize(source);
        assert_eq!(tokens.len(), 1);
        let first = tokens
            .first()
            .cloned()
            .expect("tokenizer should produce at least one token");
        // Assert the kind matches and the span maps exactly to the
        // full input lexeme. Also ensure no error tokens are present.
        assert_eq!(first.0, expected);
        let span = first.1.clone();
        let slice = source.get(span).expect("span should be valid for source");
        assert_eq!(slice, source);
        assert!(tokens.iter().all(|(k, _)| *k != SyntaxKind::N_ERROR));
    }

    #[test]
    fn escaped_string_token() {
        let tokens = tokenize("\"a\\\"b\"");
        assert_eq!(tokens.len(), 1);
        assert_eq!(
            tokens
                .first()
                .expect("tokenizer should produce at least one token")
                .0,
            SyntaxKind::T_STRING
        );
    }

    #[test]
    fn unterminated_comment_is_error() {
        let tokens = tokenize("/* comment");
        assert_eq!(tokens.len(), 1);
        assert_eq!(
            tokens
                .first()
                .expect("tokenizer should produce at least one token")
                .0,
            SyntaxKind::N_ERROR
        );
    }
}

#[test]
fn negative_number_tokens() {
    // Simple negative number
    let tokens = tokenize("-1");
    let kinds: Vec<SyntaxKind> = tokens.iter().map(|(k, _)| *k).collect();
    assert_eq!(kinds, vec![SyntaxKind::T_MINUS, SyntaxKind::T_NUMBER]);

    // Negative number with whitespace
    let tokens_ws = tokenize("-   1");
    let kinds_ws: Vec<SyntaxKind> = tokens_ws.iter().map(|(k, _)| *k).collect();
    assert_eq!(
        kinds_ws,
        vec![
            SyntaxKind::T_MINUS,
            SyntaxKind::T_WHITESPACE,
            SyntaxKind::T_NUMBER
        ]
    );

    // Negative number with comment
    let tokens_comment = tokenize("-/* comment */1");
    let kinds_comment: Vec<SyntaxKind> = tokens_comment.iter().map(|(k, _)| *k).collect();
    assert_eq!(
        kinds_comment,
        vec![
            SyntaxKind::T_MINUS,
            SyntaxKind::T_COMMENT,
            SyntaxKind::T_NUMBER
        ]
    );

    // Negative number with whitespace and comment
    let tokens_ws_comment = tokenize(
        "- 	 // comment
 1",
    );
    let kinds_ws_comment: Vec<SyntaxKind> = tokens_ws_comment.iter().map(|(k, _)| *k).collect();
    assert_eq!(
        kinds_ws_comment,
        vec![
            SyntaxKind::T_MINUS,
            SyntaxKind::T_WHITESPACE,
            SyntaxKind::T_COMMENT,
            SyntaxKind::T_WHITESPACE,
            SyntaxKind::T_NUMBER
        ]
    );
}

#[test]
fn empty_input_produces_no_tokens() {
    let tokens = tokenize("");
    assert!(tokens.is_empty());
}

#[test]
fn complex_expression() {
    let src = "R(a, b) :- Q(a) && S(b).";
    let tokens = tokenize(src);
    // ensure we tokenise without errors and capture punctuation
    assert!(tokens.iter().all(|(k, _)| *k != SyntaxKind::N_ERROR));
    assert!(tokens.iter().any(|(k, _)| *k == SyntaxKind::T_IMPLIES));
    assert!(tokens.iter().any(|(k, _)| *k == SyntaxKind::T_DOT));
}
