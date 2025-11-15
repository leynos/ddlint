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

use chumsky::error::Simple;

use super::{AstNode, Expr};
use crate::parser::expression::parse_expression;
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

    /// Return the CST nodes representing the rule body expressions.
    #[must_use]
    pub fn body_expression_nodes(&self) -> Vec<RuleBodyExpression> {
        self.syntax
            .children()
            .filter(|n| n.kind() == SyntaxKind::N_EXPR_NODE)
            .map(|syntax| RuleBodyExpression { syntax })
            .collect()
    }

    /// Text of each body literal in order of appearance.
    #[must_use]
    pub fn body_literals(&self) -> Vec<String> {
        self.body_expression_nodes()
            .into_iter()
            .map(|expr| expr.text().trim().to_string())
            .collect()
    }

    /// Parse the body literals into structured expressions.
    ///
    /// # Errors
    /// Returns aggregated expression parser diagnostics if any literal fails to parse.
    pub fn body_expressions(&self) -> Result<Vec<Expr>, Vec<Simple<SyntaxKind>>> {
        let mut exprs = Vec::new();
        let mut errors = Vec::new();

        for literal in self.body_expression_nodes() {
            let text = literal.text();
            let trimmed = text.trim();
            if trimmed.is_empty() {
                continue;
            }
            match parse_expression(trimmed) {
                Ok(expr) => exprs.push(expr),
                Err(mut errs) => errors.append(&mut errs),
            }
        }

        if errors.is_empty() {
            Ok(exprs)
        } else {
            Err(errors)
        }
    }
}

impl_ast_node!(Rule);

/// Wrapper for a single rule body expression CST node.
#[derive(Debug, Clone)]
pub struct RuleBodyExpression {
    syntax: rowan::SyntaxNode<DdlogLanguage>,
}

impl RuleBodyExpression {
    /// Full text of the expression as written in the source.
    #[must_use]
    pub fn text(&self) -> String {
        self.syntax.text().to_string()
    }
}

impl_ast_node!(RuleBodyExpression);

#[cfg(test)]
mod tests {

    use super::Expr;
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

    #[expect(clippy::expect_used, reason = "test requires parsed expressions")]
    #[test]
    fn body_expressions_parse_control_flow() {
        let src = "R(x) :- for (item in Items(item)) Process(item), if (ready(x)) { Accept(x) } else { Skip() }.";
        let parsed = parse(src);
        crate::test_util::assert_no_parse_errors(parsed.errors());
        let rule = parsed
            .root()
            .rules()
            .first()
            .cloned()
            .expect("rule missing");
        let exprs = rule.body_expressions().expect("expressions should parse");
        assert_eq!(exprs.len(), 2);
        assert!(matches!(exprs.first(), Some(Expr::ForLoop { .. })));
        assert!(matches!(exprs.get(1), Some(Expr::IfElse { .. })));
    }
}
