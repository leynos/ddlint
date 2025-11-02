//! Literal parsing helpers for prefix expressions.

use crate::parser::ast::{Expr, Literal};
use crate::{Span, SyntaxKind};

use super::pratt::Pratt;

impl<I> Pratt<'_, I>
where
    I: Iterator<Item = (SyntaxKind, Span)> + Clone,
{
    /// Parses a recognized literal token into an `Expr::Literal` node.
    ///
    /// # Parameters
    /// - `kind`: The syntax kind returned by the lexer for the current token.
    /// - `span`: The span identifying where the literal appears in the source.
    ///
    /// # Returns
    /// Returns `Some(Expr)` when the token kind represents a literal supported by
    /// the Pratt parser, or `None` when the token does not correspond to a
    /// literal expression.
    ///
    /// # Examples
    /// ```ignore
    /// let span = Span::new(0, 4);
    /// let expr = parser.parse_literal(SyntaxKind::K_TRUE, &span);
    /// assert!(matches!(expr, Some(Expr::Literal(_))));
    /// ```
    #[expect(
        clippy::expect_used,
        reason = "the lexer enforces quoted string tokens"
    )]
    pub(super) fn parse_literal(&self, kind: SyntaxKind, span: &Span) -> Option<Expr> {
        match kind {
            SyntaxKind::T_NUMBER => Some(Expr::Literal(Literal::Number(self.ts.slice(span)))),
            SyntaxKind::T_STRING => {
                let raw = self.ts.slice(span);
                let value = raw
                    .strip_prefix('"')
                    .and_then(|s| s.strip_suffix('"'))
                    .expect("tokenizer guarantees T_STRING tokens are quoted")
                    .to_string();
                Some(Expr::Literal(Literal::String(value)))
            }
            SyntaxKind::K_TRUE => Some(Expr::Literal(Literal::Bool(true))),
            SyntaxKind::K_FALSE => Some(Expr::Literal(Literal::Bool(false))),
            _ => None,
        }
    }
}
