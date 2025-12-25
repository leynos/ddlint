//! Parsing helpers for vector and map literal expressions.

use crate::parser::ast::Expr;
use crate::{Span, SyntaxKind};

use super::pratt::Pratt;

/// Binding power above which the colon operator will not be consumed.
///
/// Map literal keys must be parsed with this binding power so that
/// `{a: 1}` parses as a map entry rather than an ascription expression.
/// The colon's left binding power is 50, so parsing with 51 stops before it.
const BP_ABOVE_COLON: u8 = 51;

impl<I> Pratt<'_, I>
where
    I: Iterator<Item = (SyntaxKind, Span)> + Clone,
{
    /// Parse a vector literal `[e1, e2, ...]`.
    ///
    /// Called after consuming the opening bracket. Supports trailing commas.
    pub(super) fn parse_vector_literal(&mut self) -> Option<Expr> {
        self.with_struct_literals_suspended(|this| {
            if matches!(this.ts.peek_kind(), Some(SyntaxKind::T_RBRACKET)) {
                this.ts.next_tok();
                return Some(Expr::VecLit(Vec::new()));
            }
            let items = this.parse_comma_separated_exprs(SyntaxKind::T_RBRACKET)?;
            if !this.ts.expect(SyntaxKind::T_RBRACKET) {
                return None;
            }
            Some(Expr::VecLit(items))
        })
    }

    /// Parse a brace-delimited expression, which may be a map literal or a grouping.
    ///
    /// Map literals: `{k: v, ...}` where each entry is a key-value pair.
    /// Brace groups: `{ expr }` where the braces are used for grouping.
    ///
    /// Disambiguation: if the expression after the first item is followed by a
    /// colon, we parse as a map literal; otherwise it's a group expression.
    pub(super) fn parse_brace_or_map_literal(&mut self) -> Option<Expr> {
        // Empty braces `{}` produce an empty map literal.
        if matches!(self.ts.peek_kind(), Some(SyntaxKind::T_RBRACE)) {
            self.ts.next_tok();
            return Some(Expr::MapLit(Vec::new()));
        }

        self.with_struct_literals_suspended(|this| {
            // Parse the first expression as a potential key, stopping before
            // the colon so we can check if this is a map literal.
            let first = this.parse_expr(BP_ABOVE_COLON)?;

            // Check if this is a map literal (followed by colon).
            if matches!(this.ts.peek_kind(), Some(SyntaxKind::T_COLON)) {
                this.ts.next_tok();
                let value = this.parse_map_value()?;
                let mut entries = vec![(first, value)];

                // Parse remaining entries.
                while matches!(this.ts.peek_kind(), Some(SyntaxKind::T_COMMA)) {
                    this.ts.next_tok();
                    // Allow trailing comma.
                    if matches!(this.ts.peek_kind(), Some(SyntaxKind::T_RBRACE)) {
                        break;
                    }
                    let key = this.parse_expr(BP_ABOVE_COLON)?;
                    if !this.ts.expect(SyntaxKind::T_COLON) {
                        return None;
                    }
                    let value = this.parse_map_value()?;
                    entries.push((key, value));
                }

                if !this.ts.expect(SyntaxKind::T_RBRACE) {
                    return None;
                }
                Some(Expr::MapLit(entries))
            } else {
                // Not a map literal; treat as a brace group expression.
                // Continue parsing the expression from where we left off.
                let full_expr = this.complete_expr_from(first)?;
                if !this.ts.expect(SyntaxKind::T_RBRACE) {
                    return None;
                }
                Some(Expr::Group(Box::new(full_expr)))
            }
        })
    }

    /// Parse a map value expression, stopping at comma or closing brace.
    ///
    /// Unlike keys, values need full expression parsing (binding power 0) so that
    /// low-precedence operators like `==`, `&&`, `||` can appear in values:
    /// `{k: a == b}` or `{k: x && y}`.
    fn parse_map_value(&mut self) -> Option<Expr> {
        self.parse_expr(0)
    }

    /// Continue parsing an expression that was started with a higher binding power.
    ///
    /// This is used when we started parsing with `BP_ABOVE_COLON` and found that
    /// the expression is not a map key, so we need to continue parsing any
    /// remaining infix operators.
    fn complete_expr_from(&mut self, lhs: Expr) -> Option<Expr> {
        // Continue with infix parsing at binding power 0.
        let lhs = self.parse_postfix(lhs)?;
        self.parse_infix(lhs, 0)
    }

    /// Parse a comma-separated list of expressions up to (but not consuming) `terminator`.
    fn parse_comma_separated_exprs(&mut self, terminator: SyntaxKind) -> Option<Vec<Expr>> {
        let mut items = Vec::new();
        loop {
            let expr = self.parse_expr(0)?;
            items.push(expr);
            if !matches!(self.ts.peek_kind(), Some(SyntaxKind::T_COMMA)) {
                break;
            }
            self.ts.next_tok();
            // Allow trailing comma.
            if matches!(self.ts.peek_kind(), Some(k) if k == terminator) {
                break;
            }
        }
        Some(items)
    }
}
