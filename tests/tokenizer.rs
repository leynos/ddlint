use ddlint::{SyntaxKind, tokenize};
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
