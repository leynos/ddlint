//!
//! AST wrapper for rule declarations.

use super::AstNode;
use crate::{DdlogLanguage, SyntaxKind};

/// Typed wrapper for a rule declaration.
#[derive(Debug, Clone)]
pub struct Rule {
    pub(crate) syntax: rowan::SyntaxNode<DdlogLanguage>,
}

impl Rule {
    /// Text of the rule head atom.
    #[must_use]
    pub fn head(&self) -> Option<String> {
        use rowan::NodeOrToken;

        let mut buf = String::new();
        for e in self.syntax.children_with_tokens() {
            match e {
                NodeOrToken::Token(t) => match t.kind() {
                    SyntaxKind::T_IMPLIES | SyntaxKind::T_DOT => break,
                    _ => buf.push_str(t.text()),
                },
                NodeOrToken::Node(n) => buf.push_str(&n.text().to_string()),
            }
        }

        let text = buf.trim();
        if text.is_empty() {
            None
        } else {
            Some(text.to_string())
        }
    }

    /// Text of each body literal in order of appearance.
    #[must_use]
    pub fn body_literals(&self) -> Vec<String> {
        use rowan::NodeOrToken;

        let mut iter = self
            .syntax
            .children_with_tokens()
            .skip_while(|e| e.kind() != SyntaxKind::T_IMPLIES);

        if matches!(iter.next().map(|e| e.kind()), Some(SyntaxKind::T_IMPLIES)) {
            let mut buf = String::new();
            let mut lits = Vec::new();
            for e in iter {
                match e {
                    NodeOrToken::Token(t) => match t.kind() {
                        SyntaxKind::T_COMMA => {
                            let lit = buf.trim();
                            if !lit.is_empty() {
                                lits.push(lit.to_string());
                            }
                            buf.clear();
                        }
                        SyntaxKind::T_DOT => {
                            let lit = buf.trim();
                            if !lit.is_empty() {
                                lits.push(lit.to_string());
                            }
                            break;
                        }
                        _ => buf.push_str(t.text()),
                    },
                    NodeOrToken::Node(n) => buf.push_str(&n.text().to_string()),
                }
            }
            lits
        } else {
            Vec::new()
        }
    }
}

impl_ast_node!(Rule);

#[cfg(test)]
mod tests {

    use crate::parse;

    #[expect(clippy::expect_used, reason = "Using expect for clearer test failures")]
    #[test]
    fn rule_head_and_body() {
        let parsed = parse("A(x) :- B(x).");
        let rule = parsed
            .root()
            .rules()
            .first()
            .cloned()
            .expect("rule missing");
        assert_eq!(rule.head(), Some("A(x)".into()));
        assert_eq!(rule.body_literals(), vec!["B(x)".to_string()]);
    }
}
