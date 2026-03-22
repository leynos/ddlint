//!
//! AST wrapper for index declarations.
//!
//! This module provides a typed wrapper around `DDlog` index syntax nodes,
//! enabling structured access to the optional name, typed field list, and
//! `on` target used to build the index.

use super::AstNode;
use super::parse_utils::is_trivia;
use crate::{DdlogLanguage, SyntaxKind};

/// Public error surface for malformed index field lists.
pub type IndexFieldParseErrors = Vec<String>;

/// Typed wrapper for an index declaration.
#[derive(Debug, Clone)]
pub struct Index {
    pub(crate) syntax: rowan::SyntaxNode<DdlogLanguage>,
}

impl Index {
    /// Name of the index if present.
    #[must_use]
    pub fn name(&self) -> Option<String> {
        self.find_identifier_after_keyword(SyntaxKind::K_INDEX)
    }

    /// Typed field list declared for the index.
    ///
    /// # Errors
    ///
    /// Returns formatted parse errors when the field list cannot be interpreted
    /// as one or more `name: Type` pairs.
    pub fn fields(&self) -> Result<Vec<(String, String)>, IndexFieldParseErrors> {
        let (fields, errors) =
            super::parse_utils::parse_name_type_pairs(self.syntax.children_with_tokens());
        if errors.is_empty() {
            Ok(fields)
        } else {
            Err(errors.into_iter().map(|error| error.to_string()).collect())
        }
    }

    /// Normalized `on` target text with trivia removed.
    #[must_use]
    pub fn on_target(&self) -> Option<String> {
        self.target_text_after_keyword(SyntaxKind::K_ON)
    }

    fn find_identifier_after_keyword(&self, keyword: SyntaxKind) -> Option<String> {
        let mut iter = self.syntax.children_with_tokens();
        if !super::skip_to_match(&mut iter, |k| k == keyword) {
            return None;
        }
        super::take_first_ident(iter)
    }

    fn target_text_after_keyword(&self, keyword: SyntaxKind) -> Option<String> {
        let mut iter = self.syntax.children_with_tokens();
        if !super::skip_to_match(&mut iter, |k| k == keyword) {
            return None;
        }

        let elements: Vec<_> = iter.filter(|element| !is_trivia(element)).collect();

        let first = elements.first()?;
        let mut target = element_text(first);
        let atom_len = atom_element_len(&elements);
        let delay_end = atom_len + delay_suffix_len(elements.get(atom_len..).unwrap_or_default());
        for element in elements.iter().skip(1).take(delay_end.saturating_sub(1)) {
            target.push_str(&element_text(element));
        }

        Some(target)
    }
}

fn atom_element_len(elements: &[rowan::SyntaxElement<DdlogLanguage>]) -> usize {
    if elements.is_empty() {
        return 0;
    }

    let mut index = 0usize;
    if token_kind(elements.get(index)) == Some(SyntaxKind::T_AMP) {
        index += 1;
    }

    if token_kind(elements.get(index)) != Some(SyntaxKind::T_IDENT) {
        return 1;
    }
    index += 1;

    while matches!(
        token_kind(elements.get(index)),
        Some(SyntaxKind::T_COLON_COLON | SyntaxKind::T_DOT)
    ) && token_kind(elements.get(index + 1)) == Some(SyntaxKind::T_IDENT)
    {
        index += 2;
    }

    if token_kind(elements.get(index)) == Some(SyntaxKind::T_APOSTROPHE) {
        index += 1;
    }

    match token_kind(elements.get(index)) {
        Some(SyntaxKind::T_LPAREN) => {
            index
                + balanced_suffix_len(
                    elements.get(index..).unwrap_or_default(),
                    SyntaxKind::T_LPAREN,
                    SyntaxKind::T_RPAREN,
                )
        }
        Some(SyntaxKind::T_LBRACKET) => {
            index
                + balanced_suffix_len(
                    elements.get(index..).unwrap_or_default(),
                    SyntaxKind::T_LBRACKET,
                    SyntaxKind::T_RBRACKET,
                )
        }
        Some(SyntaxKind::T_LBRACE) => {
            index
                + balanced_suffix_len(
                    elements.get(index..).unwrap_or_default(),
                    SyntaxKind::T_LBRACE,
                    SyntaxKind::T_RBRACE,
                )
        }
        _ => index,
    }
}

fn delay_suffix_len(elements: &[rowan::SyntaxElement<DdlogLanguage>]) -> usize {
    if matches!(
        (
            token_kind(elements.first()),
            token_kind(elements.get(1)),
            token_kind(elements.get(2)),
            token_kind(elements.get(3))
        ),
        (
            Some(SyntaxKind::T_MINUS),
            Some(SyntaxKind::T_LT),
            Some(SyntaxKind::T_NUMBER),
            Some(SyntaxKind::T_GT)
        )
    ) {
        4
    } else {
        0
    }
}

fn balanced_suffix_len(
    elements: &[rowan::SyntaxElement<DdlogLanguage>],
    open: SyntaxKind,
    close: SyntaxKind,
) -> usize {
    let mut depth = 0usize;
    for (index, element) in elements.iter().enumerate() {
        match token_kind(Some(element)) {
            Some(kind) if kind == open => depth += 1,
            Some(kind) if kind == close => {
                depth = depth.saturating_sub(1);
                if depth == 0 {
                    return index + 1;
                }
            }
            _ => {}
        }
    }
    elements.len()
}

fn token_kind(element: Option<&rowan::SyntaxElement<DdlogLanguage>>) -> Option<SyntaxKind> {
    match element {
        Some(rowan::NodeOrToken::Token(token)) => Some(token.kind()),
        _ => None,
    }
}

fn element_text(element: &rowan::SyntaxElement<DdlogLanguage>) -> String {
    match element {
        rowan::NodeOrToken::Token(token) => token.text().to_string(),
        rowan::NodeOrToken::Node(node) => node.text().to_string(),
    }
}

impl_ast_node!(Index);

#[cfg(test)]
mod tests {

    use crate::parse;

    #[expect(clippy::expect_used, reason = "Using expect for clearer test failures")]
    #[test]
    fn index_fields_and_target() {
        let parsed = parse("index I(name: string) on R[lower(name)]");
        crate::test_util::assert_no_parse_errors(parsed.errors());
        let idx = parsed
            .root()
            .indexes()
            .first()
            .cloned()
            .expect("index missing");
        assert_eq!(idx.name().as_deref(), Some("I"));
        assert_eq!(
            idx.fields(),
            Ok(vec![("name".to_string(), "string".to_string())])
        );
        assert_eq!(idx.on_target().as_deref(), Some("R[lower(name)]"));
    }
}
