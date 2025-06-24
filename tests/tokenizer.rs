use ddlint::{SyntaxKind, tokenize};
use rstest::{fixture, rstest};

/// Provides a static example input string for use in tokenizer tests.
///
/// # Examples
///
/// ```no_run
/// let input = simple_input();
/// assert_eq!(input, "input relation R(x: u32)");
/// ```
#[fixture]
fn simple_input() -> &'static str {
    "input relation R(x: u32)"
}

/// Tests that tokenising a single keyword or identifier produces the expected sequence of token kinds.
///
/// # Examples
///
/// ```no_run
/// single_tokens("input", vec![SyntaxKind::K_INPUT]);
/// single_tokens("relation", vec![SyntaxKind::K_RELATION]);
/// single_tokens("R", vec![SyntaxKind::T_IDENT]);
/// ```
#[rstest]
#[case("input", vec![SyntaxKind::K_INPUT])]
#[case("relation", vec![SyntaxKind::K_RELATION])]
#[case("R", vec![SyntaxKind::T_IDENT])]
fn single_tokens(#[case] source: &str, #[case] expected: Vec<SyntaxKind>) {
    let tokens = tokenize(source);
    let kinds: Vec<SyntaxKind> = tokens.iter().map(|(k, _)| *k).collect();
    assert_eq!(kinds, expected);
}

/// Verifies that the spans of keyword tokens correspond to the correct substrings in the input.
///
/// This test checks that the `K_INPUT` and `K_RELATION` tokens produced by the tokenizer
/// map to the expected text slices within the provided input string.
///
/// # Examples
///
/// ```no_run
/// let input = "input relation R(x: u32)";
/// token_spans(input); // Asserts that "input" and "relation" tokens have correct spans.
/// ```
#[rstest]
fn token_spans(simple_input: &str) {
    let tokens = tokenize(simple_input);
    for (kind, span) in tokens {
        let text = simple_input.get(span.clone()).unwrap_or("");
        if let SyntaxKind::K_INPUT = kind {
            assert_eq!(text, "input");
        } else if let SyntaxKind::K_RELATION = kind {
            assert_eq!(text, "relation");
        }
    }
}

/// Tests that a single literal input is tokenised as the expected literal token kind.
///
/// Asserts that tokenising the input string produces exactly one token of the expected
/// `SyntaxKind` for literals such as numbers or strings.
///
/// # Examples
///
/// ```no_run
/// // Number literal
/// literal_tokens("123", SyntaxKind::T_NUMBER);
///
/// // String literal
/// literal_tokens("\"foo\"", SyntaxKind::T_STRING);
/// ```
#[rstest]
#[case("123", SyntaxKind::T_NUMBER)]
#[case("\"foo\"", SyntaxKind::T_STRING)]
fn literal_tokens(#[case] source: &str, #[case] expected: SyntaxKind) {
    let tokens = tokenize(source);
    assert_eq!(tokens.len(), 1);
    let first = tokens
        .first()
        .cloned()
        .unwrap_or_else(|| panic!("no token"));
    assert_eq!(first.0, expected);
}

/// Verifies that whitespace and comment inputs are tokenised as the expected trivia token kind.
///
/// Asserts that tokenising the given source string produces exactly one token of the expected
/// `SyntaxKind` for trivia (whitespace or comment).
///
/// # Examples
///
/// ```no_run
/// use ddlint::tokenize;
/// use ddlint::SyntaxKind;
///
/// let tokens = tokenize(" ");
/// assert_eq!(tokens.len(), 1);
/// assert_eq!(tokens[0].0, SyntaxKind::T_WHITESPACE);
/// ```
#[rstest]
#[case(" ", SyntaxKind::T_WHITESPACE)]
#[case("\n", SyntaxKind::T_WHITESPACE)]
#[case("/* c */", SyntaxKind::T_COMMENT)]
#[case("// line", SyntaxKind::T_COMMENT)]
fn trivia_tokens(#[case] source: &str, #[case] expected: SyntaxKind) {
    let tokens = tokenize(source);
    assert_eq!(tokens.len(), 1);
    let first = tokens
        .first()
        .cloned()
        .unwrap_or_else(|| panic!("no token"));
    assert_eq!(first.0, expected);
}

/// Verifies that tokenising an unknown character produces a single error token.
///
/// This test ensures that when the tokenizer encounters an unrecognised character,
/// it emits exactly one token of kind `N_ERROR`.
///
/// # Examples
///
/// ```no_run
/// // The test will pass if the tokenizer returns a single error token for '?'
/// unknown_character_produces_error("?");
/// ```
#[rstest]
#[case("?")]
#[case("$")]
fn unknown_character_produces_error(#[case] source: &str) {
    let tokens = tokenize(source);
    assert_eq!(tokens.len(), 1);
    let first = tokens
        .first()
        .cloned()
        .unwrap_or_else(|| panic!("no token"));
    assert_eq!(first.0, SyntaxKind::N_ERROR);
}
