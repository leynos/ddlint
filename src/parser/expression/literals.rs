//! Literal parsing helpers for prefix expressions.

use crate::parser::ast::{Expr, Literal, StringKind, StringLiteral};
use crate::{Span, SyntaxKind};

use super::pratt::Pratt;

fn is_valid_string_prefix(s: &str) -> bool {
    s.starts_with('"') || s.starts_with("[|") || s.starts_with("$[|")
}

fn contains_unescaped_interpolation(body: &str) -> bool {
    let mut backslashes = 0;
    let mut chars = body.chars().peekable();

    while let Some(ch) = chars.next() {
        match ch {
            '\\' => backslashes += 1,
            '$' => {
                if backslashes & 1 == 0 && matches!(chars.peek(), Some('{')) {
                    return true;
                }
                backslashes = 0;
            }
            _ => backslashes = 0,
        }
    }

    false
}

enum StringPrefix {
    Standard,
    Raw { interpolated: bool },
}

fn parse_prefix(text: &str) -> (bool, StringPrefix, &str) {
    let (interned, rest) = text.strip_prefix('i').map_or((false, text), |rest| {
        if is_valid_string_prefix(rest) {
            (true, rest)
        } else {
            (false, text)
        }
    });

    if let Some(content) = rest.strip_prefix("$[|") {
        return (interned, StringPrefix::Raw { interpolated: true }, content);
    }
    if let Some(content) = rest.strip_prefix("[|") {
        return (
            interned,
            StringPrefix::Raw {
                interpolated: false,
            },
            content,
        );
    }

    (interned, StringPrefix::Standard, rest)
}

pub(super) fn parse_string_literal_text(text: &str) -> Result<StringLiteral, &'static str> {
    let (interned, prefix, rest) = parse_prefix(text);

    match prefix {
        StringPrefix::Raw { interpolated } => {
            let body = rest
                .strip_suffix("|]")
                .ok_or("unterminated raw string literal")?;
            Ok(StringLiteral {
                body: body.to_string(),
                kind: StringKind::Raw { interpolated },
                interned,
            })
        }
        StringPrefix::Standard => {
            let content = rest.strip_prefix('"').ok_or("expected '\"'")?;
            let body = content
                .strip_suffix('"')
                .ok_or("unterminated string literal")?;
            let interpolated = contains_unescaped_interpolation(body);
            Ok(StringLiteral {
                body: body.to_string(),
                kind: StringKind::Standard { interpolated },
                interned,
            })
        }
    }
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
            SyntaxKind::T_STRING => match parse_string_literal_text(&self.ts.slice(span)) {
                Ok(s) => Some(Expr::Literal(Literal::String(s))),
                Err(msg) => {
                    self.ts.push_error(span.clone(), msg.to_string());
                    None
                }
            },
            SyntaxKind::K_TRUE => Some(Expr::Literal(Literal::Bool(true))),
            SyntaxKind::K_FALSE => Some(Expr::Literal(Literal::Bool(false))),
            _ => None,
        }
    }
}
