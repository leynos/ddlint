//! AST wrapper for `apply` declarations.
//!
//! This module provides typed access to `apply` statements, exposing the
//! transformer name alongside input and output identifiers.

use rowan::SyntaxElement;

use super::{AstNode, skip_to_apply_keyword, skip_whitespace_and_comments, take_first_ident};
use crate::{DdlogLanguage, SyntaxKind};

/// Typed wrapper for an `apply` declaration.
#[derive(Debug, Clone)]
pub struct Apply {
    pub(crate) syntax: rowan::SyntaxNode<DdlogLanguage>,
}

impl Apply {
    /// Name of the transformer if present.
    #[must_use]
    pub fn transformer_name(&self) -> Option<String> {
        let mut iter = self.syntax.children_with_tokens();
        if !skip_to_apply_keyword(&mut iter) {
            return None;
        }
        take_first_ident(iter)
    }

    /// Input identifiers passed to the transformer.
    #[must_use]
    pub fn inputs(&self) -> Vec<String> {
        self.collect_lists().0
    }

    /// Output relation identifiers produced by the transformer.
    #[must_use]
    pub fn outputs(&self) -> Vec<String> {
        self.collect_lists().1
    }

    fn collect_lists(&self) -> (Vec<String>, Vec<String>) {
        let mut iter = self.syntax.children_with_tokens().peekable();
        if !skip_to_apply_keyword(&mut iter) {
            return (Vec::new(), Vec::new());
        }

        let _ = take_first_ident(iter.by_ref());
        let inputs = collect_parenthesized_ident_list(&mut iter);
        let outputs = collect_parenthesized_ident_list(&mut iter);
        (inputs, outputs)
    }
}

impl_ast_node!(Apply);

fn collect_parenthesized_ident_list<I>(iter: &mut std::iter::Peekable<I>) -> Vec<String>
where
    I: Iterator<Item = SyntaxElement<DdlogLanguage>>,
{
    let mut names = Vec::new();
    if !skip_to_open_paren(iter) {
        return names;
    }

    let mut depth = 1usize;
    for e in iter.by_ref() {
        match e.kind() {
            SyntaxKind::T_LPAREN => depth += 1,
            SyntaxKind::T_RPAREN if depth == 1 => break,
            SyntaxKind::T_RPAREN => depth -= 1,
            SyntaxKind::T_IDENT if depth == 1 => {
                let SyntaxElement::Token(t) = e else {
                    continue;
                };
                names.push(t.text().to_string());
            }
            _ => {}
        }
    }

    names
}

fn skip_to_open_paren<I>(iter: &mut std::iter::Peekable<I>) -> bool
where
    I: Iterator<Item = SyntaxElement<DdlogLanguage>>,
{
    loop {
        skip_whitespace_and_comments(iter);
        let Some(e) = iter.next() else {
            return false;
        };
        if e.kind() == SyntaxKind::T_LPAREN {
            return true;
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::parse;

    #[test]
    #[expect(clippy::expect_used, reason = "Using expect for clearer test failures")]
    fn parses_apply_lists() {
        let src = "apply transform(RelA, func_b) -> (Out1, Out2)";
        let parsed = parse(src);
        crate::test_util::assert_no_parse_errors(parsed.errors());
        let apply = parsed
            .root()
            .applys()
            .first()
            .cloned()
            .expect("apply missing");
        assert_eq!(apply.transformer_name().as_deref(), Some("transform"));
        assert_eq!(
            apply.inputs(),
            vec!["RelA".to_string(), "func_b".to_string()]
        );
        assert_eq!(
            apply.outputs(),
            vec!["Out1".to_string(), "Out2".to_string()]
        );
    }
}
