//! Pratt parser for `DDlog` expressions.
//!
//! The implementation is a small hand-rolled Pratt parser. `chumsky`
//! 0.9 does not provide a built-in Pratt combinator, so this module
//! offers a focused parser that operates over the token stream
//! produced by the lexer. It recognises a subset of operators needed
//! for arithmetic and logical expressions.

use chumsky::error::Simple;

use crate::parser::ast::{Expr, Literal, infix_binding_power, prefix_binding_power};
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
/// `Ok(expr)` when parsing succeeds without errors or `Err(errors)` when the
/// source contains syntax issues.
///
/// # Examples
///
/// ```rust,no_run
/// use ddlint::parser::expression::parse_expression;
///
/// let expr = parse_expression("1 + 2 * 3").expect("valid expression");
/// assert_eq!(expr.to_sexpr(), "(+ 1 (* 2 3))");
/// ```
///
/// # Errors
/// Returns a vector of [`Simple`] syntax errors if the expression contains
/// invalid tokens or unbalanced constructs.
#[must_use = "discarding the Result will ignore parse errors"]
pub fn parse_expression(src: &str) -> Result<Expr, Vec<Simple<SyntaxKind>>> {
    let tokens = tokenize(src);
    let iter = tokens
        .iter()
        .filter(|(k, _)| !matches!(k, SyntaxKind::T_WHITESPACE | SyntaxKind::T_COMMENT))
        .cloned();
    let mut parser = Pratt::new(iter, src);
    let expr = parser.parse_expr(0);
    if let (Some(_), Some(sp)) = (&expr, parser.check_unexpected_token()) {
        parser.push_error(sp, "unexpected token");
    }
    if parser.errors.is_empty() {
        if let Some(expr) = expr {
            return Ok(expr);
        }
        parser.push_error(src.len()..src.len(), "invalid expression");
    }
    Err(parser.errors)
}

use std::iter::Peekable;

struct Pratt<'a, I>
where
    I: Iterator<Item = (SyntaxKind, Span)>,
{
    tokens: Peekable<I>,
    src: &'a str,
    errors: Vec<Simple<SyntaxKind>>,
}

impl<'a, I> Pratt<'a, I>
where
    I: Iterator<Item = (SyntaxKind, Span)>,
{
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
    fn new(tokens: I, src: &'a str) -> Self {
        let tokens = tokens.peekable();
        Self {
            tokens,
            src,
            errors: Vec::new(),
        }
    }

    /// Record a parsing error at the given span.
    ///
    /// The error message is wrapped into a [`Simple`] so that callers can
    /// aggregate detailed diagnostics. Errors do not abort parsing; instead the
    /// parser attempts to recover and continue.
    fn push_error(&mut self, span: Span, msg: impl Into<String>) {
        self.errors.push(Simple::custom(span, msg.into()));
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
        self.tokens.next()
    }

    /// Look ahead to the next non-trivia token without consuming it.
    ///
    /// # Returns
    /// The [`SyntaxKind`] of the next token or `None` if the stream is
    /// exhausted.
    fn peek(&mut self) -> Option<SyntaxKind> {
        self.tokens.peek().map(|(k, _)| *k)
    }

    /// Consume the next token if any remain and return its span.
    ///
    /// Used after parsing an expression to detect trailing tokens. When a
    /// token is present it is consumed so the caller can report an
    /// "unexpected token" error using the returned span.
    ///
    /// # Returns
    /// `Some(span)` if a token was consumed, `None` otherwise.
    fn check_unexpected_token(&mut self) -> Option<Span> {
        if self.peek().is_some() {
            self.next().map(|(_, sp)| sp)
        } else {
            None
        }
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
        let lhs = self.parse_prefix()?;
        self.parse_infix(lhs, min_bp)
    }

    /// Parse the next prefix expression from the token stream.
    ///
    /// This handles literals, variable references, grouped sub-expressions and
    /// unary operators defined in the precedence table. Errors are recorded
    /// when tokens do not match any prefix production.
    fn parse_prefix(&mut self) -> Option<Expr> {
        let (kind, span) = self.next()?;
        match kind {
            SyntaxKind::T_NUMBER => Some(Expr::Literal(Literal::Number(self.slice(span)))),
            SyntaxKind::T_STRING => Some(Expr::Literal(Literal::String(self.slice(span)))),
            SyntaxKind::K_TRUE => Some(Expr::Literal(Literal::Bool(true))),
            SyntaxKind::K_FALSE => Some(Expr::Literal(Literal::Bool(false))),
            SyntaxKind::T_IDENT => Some(Expr::Variable(self.slice(span))),
            SyntaxKind::T_LPAREN => {
                let expr = self.parse_expr(0);
                if !self.expect(SyntaxKind::T_RPAREN) {
                    return None;
                }
                expr.map(|e| Expr::Group(Box::new(e)))
            }
            k => {
                let Some((bp, op)) = prefix_binding_power(k) else {
                    self.push_error(span, "unexpected token");
                    return None;
                };
                let rhs = self.parse_expr(bp)?;
                Some(Expr::Unary {
                    op,
                    expr: Box::new(rhs),
                })
            }
        }
    }

    /// Parse a chain of infix operations with respect to `min_bp`.
    ///
    /// The function repeatedly folds operators whose left binding power is at
    /// least `min_bp`, descending recursively with the corresponding right
    /// binding power. It returns the fully constructed expression once no
    /// further operators can be consumed.
    fn parse_infix(&mut self, mut lhs: Expr, min_bp: u8) -> Option<Expr> {
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
                .peek()
                .map(|t| t.1.clone())
                .unwrap_or(self.src.len()..self.src.len());
            self.push_error(span, format!("expected {kind:?}"));
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
