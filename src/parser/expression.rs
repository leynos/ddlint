//! Pratt parser for `DDlog` expressions.
//!
//! The implementation is a small hand-rolled Pratt parser. `chumsky`
//! 0.9 does not provide a built-in Pratt combinator, so this module
//! offers a focused parser that operates over the token stream
//! produced by the lexer. It recognises a subset of operators needed
//! for arithmetic and logical expressions.

use chumsky::error::Simple;

use crate::parser::ast::expr::infix_binding_power;
use crate::parser::ast::{Expr, Literal, UnaryOp};
use crate::{Span, SyntaxKind, tokenize};

/// Parse a source string into an [`Expr`].
///
/// This is the public entry point for the Pratt parser. It tokenizes the
/// provided source and then walks those tokens to build an expression tree.
/// Errors are collected rather than causing a panic so callers can decide how
/// to recover.
///
/// # Parameters
/// - `src`: The snippet of `DDlog` code containing an expression.
///
/// # Returns
/// A tuple of the parsed expression, if successful, and any syntax errors
/// encountered.
///
/// # Examples
///
/// ```rust,no_run
/// use ddlint::parser::expression::parse_expression;
///
/// let (expr, errs) = parse_expression("1 + 2 * 3");
/// assert!(errs.is_empty());
/// assert_eq!(expr.unwrap().to_sexpr(), "(+ 1 (* 2 3))");
/// ```
#[must_use]
pub fn parse_expression(src: &str) -> (Option<Expr>, Vec<Simple<SyntaxKind>>) {
    let tokens = tokenize(src);
    let mut parser = Pratt::new(&tokens, src);
    let expr = parser.parse_expr(0);
    (expr, parser.errors)
}

struct Pratt<'a> {
    tokens: &'a [(SyntaxKind, Span)],
    src: &'a str,
    pos: usize,
    errors: Vec<Simple<SyntaxKind>>,
}

impl<'a> Pratt<'a> {
    /// Construct a new Pratt parser over a pre-tokenised input.
    ///
    /// The parser keeps a reference to the token slice and original source so
    /// spans can be resolved back to strings when constructing literals or
    /// reporting errors.
    ///
    /// # Parameters
    /// - `tokens`: The sequence of `(SyntaxKind, Span)` pairs produced by the
    ///   lexer.
    /// - `src`: The original source text used when slicing spans.
    #[must_use]
    fn new(tokens: &'a [(SyntaxKind, Span)], src: &'a str) -> Self {
        Self {
            tokens,
            src,
            pos: 0,
            errors: Vec::new(),
        }
    }

    /// Consume the next non-trivia token from the stream.
    ///
    /// Whitespace and comments are skipped automatically. The returned value is
    /// the token kind along with its span within the original source.
    ///
    /// # Returns
    /// `Some((kind, span))` if a token was found, or `None` when the end of the
    /// stream is reached.
    fn next(&mut self) -> Option<(SyntaxKind, Span)> {
        while let Some((kind, _)) = self.tokens.get(self.pos) {
            if matches!(kind, SyntaxKind::T_WHITESPACE | SyntaxKind::T_COMMENT) {
                self.pos += 1;
            } else {
                break;
            }
        }
        let tok = self.tokens.get(self.pos).cloned();
        if tok.is_some() {
            self.pos += 1;
        }
        tok
    }

    /// Look ahead to the next non-trivia token without consuming it.
    ///
    /// # Returns
    /// The [`SyntaxKind`] of the next token or `None` if the stream is
    /// exhausted.
    fn peek(&self) -> Option<SyntaxKind> {
        let mut idx = self.pos;
        while let Some((kind, _)) = self.tokens.get(idx) {
            if matches!(kind, SyntaxKind::T_WHITESPACE | SyntaxKind::T_COMMENT) {
                idx += 1;
            } else {
                return Some(*kind);
            }
        }
        None
    }

    /// Parse an expression with the given minimum binding power.
    ///
    /// This is the core Pratt parsing routine. It consumes a prefix expression
    /// and then repeatedly folds in infix operations while their binding power
    /// is high enough. The algorithm mirrors the precedence table described in
    /// `docs/haskell-parser-analysis.md`.
    ///
    /// # Parameters
    /// - `min_bp`: The minimum binding power required to continue parsing.
    ///
    /// # Returns
    /// The parsed [`Expr`] or `None` if a fatal error was encountered.
    fn parse_expr(&mut self, min_bp: u8) -> Option<Expr> {
        let (lhs_kind, lhs_span) = self.next()?;
        let mut lhs = match lhs_kind {
            SyntaxKind::T_NUMBER => Some(Expr::Literal(Literal::Number(self.slice(lhs_span)))),
            SyntaxKind::T_STRING => Some(Expr::Literal(Literal::String(self.slice(lhs_span)))),
            SyntaxKind::K_TRUE => Some(Expr::Literal(Literal::Bool(true))),
            SyntaxKind::K_FALSE => Some(Expr::Literal(Literal::Bool(false))),
            SyntaxKind::T_IDENT => Some(Expr::Variable(self.slice(lhs_span))),
            SyntaxKind::T_LPAREN => {
                let expr = self.parse_expr(0);
                if !self.expect(SyntaxKind::T_RPAREN) {
                    return None;
                }
                expr.map(|e| Expr::Group(Box::new(e)))
            }
            k => {
                if let Some((_, op)) = prefix_binding_power(k) {
                    let rhs = self.parse_expr(60)?;
                    Some(Expr::Unary {
                        op,
                        expr: Box::new(rhs),
                    })
                } else {
                    self.errors
                        .push(Simple::custom(lhs_span.clone(), "unexpected token"));
                    None
                }
            }
        }?;

        while let Some(op_kind) = self.peek() {
            let Some((l_bp, r_bp, op)) = infix_binding_power(op_kind) else {
                break;
            };
            if l_bp < min_bp {
                break;
            }
            self.next();
            let rhs = self.parse_expr(r_bp)?;
            lhs = Expr::Binary {
                op,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            };
        }
        Some(lhs)
    }

    /// Require the next token to be of a specific kind.
    ///
    /// If the next token matches `kind` it is consumed and the function
    /// returns `true`. Otherwise an error is recorded and `false` is returned.
    /// This helper keeps the main parsing loop readable.
    ///
    /// # Parameters
    /// - `kind`: The expected token kind.
    ///
    /// # Returns
    /// `true` if the token was present, `false` otherwise.
    fn expect(&mut self, kind: SyntaxKind) -> bool {
        if matches!(self.peek(), Some(k) if k == kind) {
            self.next();
            true
        } else {
            let span = self
                .tokens
                .get(self.pos)
                .map(|t| t.1.clone())
                .unwrap_or(self.src.len()..self.src.len());
            self.errors
                .push(Simple::custom(span, format!("expected {kind:?}")));
            false
        }
    }

    /// Extract a string slice for the given span.
    ///
    /// # Parameters
    /// - `span`: The byte range within the original source.
    ///
    /// # Returns
    /// The corresponding substring, or an empty string if the span is invalid.
    fn slice(&self, span: Span) -> String {
        self.src.get(span).unwrap_or("").to_string()
    }
}

/// Determine the binding power and operator variant for a prefix token.
///
/// # Parameters
/// - `kind`: The token kind representing a potential unary operator.
///
/// # Returns
/// A tuple containing the binding power and the corresponding [`UnaryOp`], or
/// `None` if the token is not a recognised prefix operator.
fn prefix_binding_power(kind: SyntaxKind) -> Option<(u8, UnaryOp)> {
    use UnaryOp::{Neg, Not};
    match kind {
        SyntaxKind::T_MINUS => Some((60, Neg)),
        SyntaxKind::K_NOT => Some((60, Not)),
        _ => None,
    }
}
