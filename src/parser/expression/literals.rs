//! Literal parsing helpers for prefix expressions.

use crate::parser::ast::{Expr, Literal, StringKind, StringLiteral};
use crate::{Span, SyntaxKind};

use super::numeric::parse_numeric_literal;
use super::pratt::Pratt;

fn is_valid_string_prefix(s: &str) -> bool {
    s.starts_with('"') || s.starts_with("[|") || s.starts_with("$[|")
}

fn contains_unescaped_interpolation(body: &str) -> bool {
    let mut search_from = 0;

    while let Some(pos) = body.get(search_from..).and_then(|s| s.find("${")) {
        let absolute_pos = search_from + pos;

        // Count consecutive backslashes before the '$'
        let mut backslash_count = 0;
        let mut check_pos = absolute_pos;

        while check_pos > 0 {
            check_pos -= 1;
            if matches!(body.as_bytes().get(check_pos), Some(b'\\')) {
                backslash_count += 1;
            } else {
                break;
            }
        }

        // Even number of backslashes (including zero) means unescaped
        if backslash_count & 1 == 0 {
            return true;
        }

        // Move past this match and continue searching
        search_from = absolute_pos + 1;
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
                match parse_string_literal_text(&self.ts.slice(span)) {
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
