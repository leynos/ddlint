//! Literal parsing helpers for prefix expressions.

use crate::parser::ast::{Expr, Literal, StringLiteral};
use crate::{Span, SyntaxKind};

use super::numeric::parse_numeric_literal;
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
    pub(super) fn parse_literal(&mut self, kind: SyntaxKind, span: &Span) -> Option<Expr> {
        match kind {
            SyntaxKind::T_NUMBER => match parse_numeric_literal(&self.ts.slice(span)) {
                Ok(number) => Some(Expr::Literal(Literal::Number(number))),
                Err(err) => {
                    self.ts.push_error(span.clone(), err.message());
                    None
                }
            },
            SyntaxKind::T_STRING => {
                // Check cache first
                if let Some(cached) = self.string_literal_cache.remove(&span.start) {
                    return Some(Expr::Literal(Literal::String(cached)));
                }

                // Cache miss: parse as before
                match StringLiteral::parse(&self.ts.slice(span)) {
                    Ok(s) => Some(Expr::Literal(Literal::String(s))),
                    Err(msg) => {
                        self.ts.push_error(span.clone(), msg.to_string());
                        None
                    }
                }
            }
            SyntaxKind::K_TRUE => Some(Expr::Literal(Literal::Bool(true))),
            SyntaxKind::K_FALSE => Some(Expr::Literal(Literal::Bool(false))),
            _ => None,
        }
    }
}
