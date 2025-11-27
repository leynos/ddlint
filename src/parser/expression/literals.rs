//! Literal parsing helpers for prefix expressions.

use crate::parser::ast::{Expr, Literal, StringKind, StringLiteral};
use crate::{Span, SyntaxKind};

use super::pratt::Pratt;

fn strip_intern_prefix(text: &str) -> (bool, &str) {
    if let Some(rest) = text.strip_prefix('i')
        && (rest.starts_with('"') || rest.starts_with("[|") || rest.starts_with("$[|"))
    {
        return (true, rest);
    }
    (false, text)
}

fn contains_unescaped_interpolation(body: &str) -> bool {
    let bytes = body.as_bytes();
    let mut idx = 0;
    while let Some(window) = bytes.get(idx..idx + 2) {
        if window == b"${" {
            let mut escaped = false;
            let mut back = idx;
            while let Some(prev_idx) = back.checked_sub(1) {
                let Some(prev) = bytes.get(prev_idx) else {
                    break;
                };
                if *prev == b'\\' {
                    escaped = !escaped;
                    back -= 1;
                    continue;
                }
                break;
            }
            if !escaped {
                return true;
            }
        }
        idx += 1;
    }
    false
}

#[must_use]
pub(super) fn parse_string_literal_text(text: &str) -> Option<StringLiteral> {
    let (interned, rest) = strip_intern_prefix(text);

    if let Some(content) = rest.strip_prefix("$[|") {
        let body = content.strip_suffix("|]")?;
        return Some(StringLiteral {
            body: body.to_string(),
            kind: StringKind::Raw { interpolated: true },
            interned,
        });
    }

    if let Some(content) = rest.strip_prefix("[|") {
        let body = content.strip_suffix("|]")?;
        return Some(StringLiteral {
            body: body.to_string(),
            kind: StringKind::Raw {
                interpolated: false,
            },
            interned,
        });
    }

    let content = rest.strip_prefix('"')?;
    let body = content.strip_suffix('"')?;
    let interpolated = contains_unescaped_interpolation(body);
    Some(StringLiteral {
        body: body.to_string(),
        kind: StringKind::Standard { interpolated },
        interned,
    })
}

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
            SyntaxKind::T_NUMBER => Some(Expr::Literal(Literal::Number(self.ts.slice(span)))),
            SyntaxKind::T_STRING => parse_string_literal_text(&self.ts.slice(span))
                .map(Literal::String)
                .map(Expr::Literal)
                .or_else(|| {
                    self.ts
                        .push_error(span.clone(), "invalid string literal".to_string());
                    None
                }),
            SyntaxKind::K_TRUE => Some(Expr::Literal(Literal::Bool(true))),
            SyntaxKind::K_FALSE => Some(Expr::Literal(Literal::Bool(false))),
            _ => None,
        }
    }
}
