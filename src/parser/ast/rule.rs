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
//! let exprs = rule.body_expressions();
//! assert_eq!(exprs.len(), 2);
//! assert!(exprs.into_iter().all(|expr| expr.is_ok()));
//! ```

use chumsky::error::Simple;

use super::{AstNode, Expr};
use crate::{DdlogLanguage, SyntaxKind, parser::expression::parse_expression};

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
        self.body_expression_texts()
    }

    /// Parse the rule body into structured expressions.
    #[must_use]
    pub fn body_expressions(&self) -> Vec<Result<Expr, Vec<Simple<SyntaxKind>>>> {
        self.body_expression_texts()
            .into_iter()
            .map(|text| parse_expression(&text))
            .collect()
    }

    fn body_expression_texts(&self) -> Vec<String> {
        self.syntax
            .children()
            .filter(|node| node.kind() == SyntaxKind::N_EXPR_NODE)
            .map(|node| {
                let text = node.text().to_string();
                text.trim().to_string()
            })
            .filter(|text| !text.is_empty())
            .collect()
    }
}

impl_ast_node!(Rule);

#[cfg(test)]
mod tests {

    use crate::{parse, parser::ast::{BinaryOp, Expr}};

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

    #[expect(clippy::expect_used, reason = "tests expect parsed rule bodies")]
    #[test]
    fn rule_body_expressions_parse_structures() {
        let parsed = parse("R(x) :- 1 + 2 * 3, if (cond) Foo(cond) else Bar().");
        let rule = parsed
            .root()
            .rules()
            .first()
            .cloned()
            .expect("rule missing");
        let exprs = rule.body_expressions();
        assert_eq!(exprs.len(), 2);
        let first = exprs[0].as_ref().expect("first literal failed to parse");
        match first {
            Expr::Binary { op: BinaryOp::Add, lhs, rhs } => match rhs.as_ref() {
                Expr::Binary { op: BinaryOp::Mul, .. } => {
                    assert!(matches!(lhs.as_ref(), Expr::Literal(_)));
                }
                other => panic!("expected multiplication RHS, got {other:?}"),
            },
            other => panic!("unexpected expression: {other:?}"),
        }
        let second = exprs[1]
            .as_ref()
            .expect("second literal failed to parse");
        assert!(matches!(second, Expr::IfElse { .. }));
    }
}
