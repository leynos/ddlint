//! Output list parsing utilities.
//!
//! Parses comma-separated output relation names following a transformer
//! declaration's colon.

use rowan::SyntaxElement;

use crate::{DdlogLanguage, SyntaxKind};

use super::super::skip_whitespace_and_comments;

pub(crate) fn skip_to_top_level_colon<I>(iter: &mut std::iter::Peekable<I>)
where
    I: Iterator<Item = SyntaxElement<DdlogLanguage>>,
{
    let mut depth = 0usize;
    for e in iter.by_ref() {
        match e.kind() {
            SyntaxKind::T_LPAREN => depth += 1,
            SyntaxKind::T_RPAREN => depth = depth.saturating_sub(1),
            SyntaxKind::T_COLON if depth == 0 => break,
            _ => {}
        }
    }
}

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
    use crate::parser::ast::AstNode;
    use crate::parser::parse;

    #[test]
    fn collects_output_names() {
        let src = "extern transformer t(a: X): Out1, Out2";
        let parsed = parse(src);
        #[expect(clippy::expect_used, reason = "Using expect for clearer test failures")]
        let tr = parsed
            .root()
            .transformers()
            .first()
            .cloned()
            .expect("transformer missing");
        let names = parse_output_list(tr.syntax().children_with_tokens());
        assert_eq!(names, vec!["Out1".to_string(), "Out2".to_string()]);
    }
}
