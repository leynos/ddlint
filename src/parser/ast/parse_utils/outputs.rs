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

fn try_parse_identifier<I>(iter: &mut std::iter::Peekable<I>) -> Option<String>
where
    I: Iterator<Item = SyntaxElement<DdlogLanguage>>,
{
    use rowan::NodeOrToken;

    skip_whitespace_and_comments(iter);
    match iter.next() {
        Some(NodeOrToken::Token(t)) if t.kind() == SyntaxKind::T_IDENT => {
            Some(t.text().to_string())
        }
        _ => None,
    }
}

fn has_comma_separator<I>(iter: &mut std::iter::Peekable<I>) -> bool
where
    I: Iterator<Item = SyntaxElement<DdlogLanguage>>,
{
    use rowan::NodeOrToken;

    skip_whitespace_and_comments(iter);
    if let Some(NodeOrToken::Token(t)) = iter.peek()
        && t.kind() == SyntaxKind::T_COMMA
    {
        iter.next();
        true
    } else {
        false
    }
}

pub(crate) fn parse_output_list<I>(iter: I) -> Vec<String>
where
    I: Iterator<Item = SyntaxElement<DdlogLanguage>>,
{
    let mut iter = iter.peekable();
    skip_to_top_level_colon(&mut iter);
    collect_comma_separated_identifiers(&mut iter)
}

fn collect_comma_separated_identifiers<I>(iter: &mut std::iter::Peekable<I>) -> Vec<String>
where
    I: Iterator<Item = SyntaxElement<DdlogLanguage>>,
{
    let mut names = Vec::new();
    while let Some(name) = try_parse_identifier(iter) {
        names.push(name);
        if !has_comma_separator(iter) {
            break;
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

    #[test]
    fn collects_single_output_name() {
        let src = "extern transformer t(a: X): Out1";
        let parsed = parse(src);
        #[expect(clippy::expect_used, reason = "Using expect for clearer test failures")]
        let tr = parsed
            .root()
            .transformers()
            .first()
            .cloned()
            .expect("transformer missing");
        let names = parse_output_list(tr.syntax().children_with_tokens());
        assert_eq!(names, vec!["Out1".to_string()]);
    }

    #[test]
    fn collects_no_output_names() {
        let src = "extern transformer t(a: X):";
        let parsed = parse(src);
        let names = parse_output_list(parsed.root().syntax().children_with_tokens());
        assert!(names.is_empty());
    }

    #[test]
    fn collects_output_name_with_trailing_comma() {
        let src = "extern transformer t(a: X): Out1,";
        let parsed = parse(src);
        let names = parse_output_list(parsed.root().syntax().children_with_tokens());
        assert!(names.is_empty());
    }
}
