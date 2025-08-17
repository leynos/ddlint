//! Parsing helpers for transformer output lists.
//!
//! The helper functions here scan tokens following a transformer declaration
//! and collect top-level identifiers after a colon. Nested parentheses are
//! handled so only names at the outermost level are returned.

use rowan::SyntaxElement;

use crate::{DdlogLanguage, SyntaxKind};

use super::super::skip_whitespace_and_comments;

pub(super) fn skip_to_top_level_colon<I>(iter: &mut std::iter::Peekable<I>)
where
    I: Iterator<Item = SyntaxElement<DdlogLanguage>>,
{
    let mut depth = 0isize;
    for e in iter.by_ref() {
        match e.kind() {
            SyntaxKind::T_LPAREN => depth += 1,
            SyntaxKind::T_RPAREN => {
                depth -= 1;
                if depth < 0 {
                    log::warn!("excess closing parenthesis detected in skip_to_top_level_colon",);
                    depth = 0;
                }
            }
            SyntaxKind::T_COLON if depth == 0 => break,
            _ => {}
        }
    }
}

/// Parse comma separated output names after a `:` token.
pub(crate) fn parse_output_list<I>(iter: I) -> Vec<String>
where
    I: Iterator<Item = SyntaxElement<DdlogLanguage>>,
{
    use rowan::NodeOrToken;

    let mut iter = iter.peekable();
    skip_to_top_level_colon(&mut iter);

    let mut names = Vec::new();
    loop {
        skip_whitespace_and_comments(&mut iter);
        match iter.next() {
            Some(NodeOrToken::Token(t)) if t.kind() == SyntaxKind::T_IDENT => {
                names.push(t.text().to_string());
            }
            _ => break,
        }
        skip_whitespace_and_comments(&mut iter);
        match iter.peek() {
            Some(NodeOrToken::Token(t)) if t.kind() == SyntaxKind::T_COMMA => {
                iter.next();
            }
            _ => break,
        }
    }

    names
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parse;
    use crate::parser::ast::AstNode;
    use rstest::rstest;

    #[rstest]
    #[case("extern transformer t(a: A, b: B): C, D", vec!["C", "D"])]
    #[case("extern transformer t(): X", vec!["X"])]
    fn outputs(#[case] src: &str, #[case] expected: Vec<&str>) {
        let parsed = parse(src);
        #[expect(clippy::expect_used, reason = "Using expect for clearer test failures")]
        let transformer = parsed
            .root()
            .transformers()
            .first()
            .cloned()
            .expect("transformer missing");
        let names = parse_output_list(transformer.syntax().children_with_tokens());
        let expected: Vec<String> = expected.into_iter().map(String::from).collect();
        assert_eq!(names, expected);
    }
}
