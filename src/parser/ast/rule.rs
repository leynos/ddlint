//!
//! AST wrapper for rule declarations.
//!
//! The [`Rule`] type is a lightweight view over a rule declaration in the
//! syntax tree. It exposes helpers for reading the rule head and for iterating
//! over the comma-separated body literals. This allows analyses to reason about
//! the dependencies between relations without building a full semantic model.
//!
//! Rules are discovered via the [`Root`](super::root::Root) wrapper and then
//! inspected using these methods.
//!
//! # Examples
//!
//! ```rust,ignore
//! # use ddlint::parse;
//! # use ddlint::parser::ast::Rule;
//! # fn first_rule(src: &str) -> Rule {
//! #     parse(src)
//! #         .root()
//! #         .rules()
//! #         .into_iter()
//! #         .next()
//! #         .expect("rule missing")
//! # }
//! let rule = first_rule("R(x) :- S(x), T(x).");
//! assert_eq!(rule.head().as_deref(), Some("R(x)"));
//! assert_eq!(rule.body_literals(), vec!["S(x)".into(), "T(x)".into()]);
//! ```

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
        let mut iter = self
            .syntax
            .children_with_tokens()
            .skip_while(|e| e.kind() != SyntaxKind::T_IMPLIES);

        if matches!(iter.next().map(|e| e.kind()), Some(SyntaxKind::T_IMPLIES)) {
            Self::extract_literals_from_body(iter)
        } else {
            Vec::new()
        }
    }

    /// Iterate over the body and collect literals.
    fn extract_literals_from_body<I>(iter: I) -> Vec<String>
    where
        I: Iterator<Item = rowan::SyntaxElement<DdlogLanguage>>,
    {
        let mut buf = String::new();
        let mut lits = Vec::new();

        for e in iter {
            if Self::process_body_element(e, &mut buf, &mut lits) {
                break;
            }
        }

        lits
    }

    /// Process a single syntax element of the rule body.
    ///
    /// Returns `true` if the body has ended.
    fn process_body_element(
        element: rowan::SyntaxElement<DdlogLanguage>,
        buf: &mut String,
        lits: &mut Vec<String>,
    ) -> bool {
        use rowan::NodeOrToken;

        match element {
            NodeOrToken::Token(t) => Self::process_token(&t, buf, lits),
            NodeOrToken::Node(n) => {
                buf.push_str(&n.text().to_string());
                false
            }
        }
    }

    /// Handle a token in the rule body.
    ///
    /// Returns `true` when the terminating `.` is encountered.
    fn process_token(
        token: &rowan::SyntaxToken<DdlogLanguage>,
        buf: &mut String,
        lits: &mut Vec<String>,
    ) -> bool {
        match token.kind() {
            SyntaxKind::T_COMMA => {
                Self::add_literal_if_not_empty(buf, lits);
                buf.clear();
                false
            }
            SyntaxKind::T_DOT => {
                Self::add_literal_if_not_empty(buf, lits);
                true
            }
            _ => {
                buf.push_str(token.text());
                false
            }
        }
    }

    /// Add a literal to the list if it contains non-whitespace text.
    fn add_literal_if_not_empty(buf: &str, lits: &mut Vec<String>) {
        let lit = buf.trim();
        if !lit.is_empty() {
            lits.push(lit.to_string());
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
        assert_eq!(rule.head().as_deref(), Some("A(x)"));
        assert_eq!(rule.body_literals(), vec!["B(x)".to_string()]);
    }
}
