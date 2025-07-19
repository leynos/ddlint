//!
//! AST wrapper for relation declarations.

use super::AstNode;
use crate::{DdlogLanguage, SyntaxKind};

/// Typed wrapper for a relation declaration.
#[derive(Debug, Clone)]
pub struct Relation {
    pub(crate) syntax: rowan::SyntaxNode<DdlogLanguage>,
}

impl Relation {
    /// Name of the relation if present.
    #[must_use]
    pub fn name(&self) -> Option<String> {
        self.syntax
            .children_with_tokens()
            .skip_while(|e| !matches!(e.kind(), SyntaxKind::K_RELATION))
            .skip(1)
            .find_map(|e| match e {
                rowan::NodeOrToken::Token(t) if t.kind() == SyntaxKind::T_IDENT => {
                    Some(t.text().to_string())
                }
                _ => None,
            })
    }

    /// Returns `true` if declared with the `input` keyword.
    #[must_use]
    pub fn is_input(&self) -> bool {
        self.syntax
            .children_with_tokens()
            .any(|e| e.kind() == SyntaxKind::K_INPUT)
    }

    /// Returns `true` if declared with the `output` keyword.
    #[must_use]
    pub fn is_output(&self) -> bool {
        self.syntax
            .children_with_tokens()
            .any(|e| e.kind() == SyntaxKind::K_OUTPUT)
    }

    /// Columns as pairs of (name, type).
    #[must_use]
    pub fn columns(&self) -> Vec<(String, String)> {
        let (pairs, errors) =
            super::parse_utils::parse_name_type_pairs(self.syntax.children_with_tokens());
        let _ = errors;
        pairs
    }

    /// Primary key column names if specified.
    #[must_use]
    pub fn primary_key(&self) -> Option<Vec<String>> {
        use rowan::NodeOrToken;

        let mut iter = self.syntax.children_with_tokens().peekable();
        for e in &mut iter {
            if e.kind() == SyntaxKind::T_LPAREN {
                break;
            }
        }
        let mut depth = 1usize;
        for e in iter.by_ref() {
            match e.kind() {
                SyntaxKind::T_LPAREN => depth += 1,
                SyntaxKind::T_RPAREN => {
                    depth -= 1;
                    if depth == 0 {
                        break;
                    }
                }
                _ => {}
            }
        }

        super::skip_whitespace_and_comments(&mut iter);

        match iter.next() {
            Some(NodeOrToken::Token(t))
                if t.kind() == SyntaxKind::T_IDENT && t.text() == "primary" => {}
            _ => return None,
        }

        super::skip_whitespace_and_comments(&mut iter);

        match iter.next() {
            Some(NodeOrToken::Token(t)) if t.kind() == SyntaxKind::T_IDENT && t.text() == "key" => {
            }
            _ => return None,
        }

        super::skip_whitespace_and_comments(&mut iter);

        if !matches!(
            iter.peek().map(rowan::SyntaxElement::kind),
            Some(SyntaxKind::T_LPAREN)
        ) {
            return None;
        }
        iter.next();
        depth = 1;
        let mut buf = String::new();
        for e in iter.by_ref() {
            match e {
                NodeOrToken::Token(t) => match t.kind() {
                    SyntaxKind::T_LPAREN => {
                        depth += 1;
                        buf.push_str(t.text());
                    }
                    SyntaxKind::T_RPAREN => {
                        if depth == 1 {
                            break;
                        }
                        depth -= 1;
                        buf.push_str(t.text());
                    }
                    _ => buf.push_str(t.text()),
                },
                NodeOrToken::Node(n) => buf.push_str(&n.text().to_string()),
            }
            if depth == 0 {
                break;
            }
        }

        let keys = buf
            .split(',')
            .map(|s| s.trim().to_string())
            .filter(|s| !s.is_empty())
            .collect::<Vec<_>>();
        if keys.is_empty() { None } else { Some(keys) }
    }
}

impl_ast_node!(Relation);

#[cfg(test)]
mod tests {

    use crate::parse;

    #[expect(clippy::expect_used, reason = "Using expect for clearer test failures")]
    #[test]
    fn relation_name() {
        let parsed = parse("input relation R(x: u32)");
        let rel = parsed
            .root()
            .relations()
            .first()
            .cloned()
            .expect("relation missing");
        assert_eq!(rel.name(), Some("R".into()));
        assert!(rel.is_input());
    }
}
