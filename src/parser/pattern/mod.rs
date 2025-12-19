//! Dedicated parser for `DDlog` patterns.
//!
//! Patterns are used in `match` arms, `for` bindings, and FlatMap-style rule
//! assignments. This parser produces structured [`crate::parser::ast::Pattern`]
//! values instead of preserving raw pattern text.

use chumsky::error::Simple;

use crate::parser::ast::{Pattern, PatternLiteral, StringLiteral};
use crate::parser::expression::parse_numeric_literal;
use crate::parser::token_stream::TokenStream;
use crate::{Span, SyntaxKind};

mod type_expr;

#[derive(Debug)]
struct PatternTokenStream<'a> {
    stream: TokenStream<'a>,
    errors: Vec<Simple<SyntaxKind>>,
}

impl<'a> PatternTokenStream<'a> {
    fn new(tokens: &'a [(SyntaxKind, Span)], src: &'a str) -> Self {
        Self {
            stream: TokenStream::new(tokens, src),
            errors: Vec::new(),
        }
    }

    fn peek_kind(&self) -> Option<SyntaxKind> {
        self.stream.peek().map(|(k, _)| *k)
    }

    fn peek_span(&self) -> Option<Span> {
        self.stream.peek().map(|(_, sp)| sp.clone())
    }

    fn next_tok(&mut self) -> Option<(SyntaxKind, Span)> {
        self.stream
            .peek()
            .cloned()
            .inspect(|_| self.stream.advance())
    }

    fn expect(&mut self, kind: SyntaxKind, msg: &'static str) -> bool {
        if self.peek_kind() == Some(kind) {
            self.next_tok();
            true
        } else {
            let span = self.peek_span().unwrap_or_else(|| self.eof_span());
            self.errors.push(Simple::custom(span, msg));
            false
        }
    }

    fn eof_span(&self) -> Span {
        self.stream.src().len()..self.stream.src().len()
    }

    fn slice(&self, span: &Span) -> String {
        debug_assert!(
            span.end <= self.stream.src().len(),
            "invalid span for pattern source slice"
        );
        self.stream
            .src()
            .get(span.clone())
            .unwrap_or("")
            .to_string()
    }

    fn take_errors(self) -> Vec<Simple<SyntaxKind>> {
        self.errors
    }
}

/// Parse a pattern from a standalone string.
///
/// # Errors
/// Returns a vector of parse errors when the input does not match the pattern
/// grammar.
pub fn parse_pattern(src: &str) -> Result<Pattern, Vec<Simple<SyntaxKind>>> {
    let tokens = crate::tokenize_without_trivia(src);
    parse_pattern_tokens(&tokens, src)
}

/// Parse a pattern from a pre-tokenised slice.
///
/// `tokens` must use spans that refer into `src`.
///
/// # Errors
/// Returns a vector of parse errors when the token stream does not match the
/// pattern grammar.
pub fn parse_pattern_tokens(
    tokens: &[(SyntaxKind, Span)],
    src: &str,
) -> Result<Pattern, Vec<Simple<SyntaxKind>>> {
    let mut ts = PatternTokenStream::new(tokens, src);
    let Some(pattern) = parse_pattern_inner(&mut ts) else {
        if ts.errors.is_empty() {
            ts.errors
                .push(Simple::custom(ts.eof_span(), "expected pattern"));
        }
        return Err(ts.take_errors());
    };

    if let Some(span) = ts.peek_span() {
        ts.errors
            .push(Simple::custom(span, "unexpected token in pattern"));
    }

    let errs = ts.take_errors();
    if errs.is_empty() {
        Ok(pattern)
    } else {
        Err(errs)
    }
}

fn parse_pattern_inner(ts: &mut PatternTokenStream<'_>) -> Option<Pattern> {
    let mut pattern = parse_primary_pattern(ts)?;

    if ts.peek_kind() == Some(SyntaxKind::T_COLON) {
        ts.next_tok();
        let ty = type_expr::parse_type_expr(ts)?;
        pattern = Pattern::Typed {
            pattern: Box::new(pattern),
            ty,
        };
    }

    Some(pattern)
}

fn parse_primary_pattern(ts: &mut PatternTokenStream<'_>) -> Option<Pattern> {
    match ts.peek_kind()? {
        SyntaxKind::K_UNDERSCORE => {
            ts.next_tok();
            Some(Pattern::Wildcard)
        }
        SyntaxKind::K_VAR => parse_var_pattern(ts, true),
        SyntaxKind::T_IDENT
        | SyntaxKind::K_FLATMAP
        | SyntaxKind::K_AGGREGATE
        | SyntaxKind::K_INSPECT => parse_identifier_pattern(ts),
        SyntaxKind::T_LPAREN => parse_tuple_pattern(ts),
        SyntaxKind::T_STRING => parse_string_literal_pattern(ts),
        SyntaxKind::T_NUMBER => parse_number_literal_pattern(ts),
        SyntaxKind::K_TRUE => {
            ts.next_tok();
            Some(Pattern::Literal(PatternLiteral::Bool(true)))
        }
        SyntaxKind::K_FALSE => {
            ts.next_tok();
            Some(Pattern::Literal(PatternLiteral::Bool(false)))
        }
        _ => {
            let span = ts.peek_span().unwrap_or_else(|| ts.eof_span());
            ts.errors.push(Simple::custom(span, "expected pattern"));
            None
        }
    }
}

fn parse_identifier_pattern(ts: &mut PatternTokenStream<'_>) -> Option<Pattern> {
    let (_, span) = ts.next_tok()?;
    let name = ts.slice(&span);
    if is_uppercase_ident(&name) {
        if ts.peek_kind() == Some(SyntaxKind::T_LBRACE) {
            return parse_struct_pattern(ts, name);
        }
        ts.errors.push(Simple::custom(
            span,
            "expected '{' to start a struct pattern",
        ));
        return None;
    }

    Some(Pattern::Var {
        declared: false,
        name,
    })
}

fn parse_string_literal_pattern(ts: &mut PatternTokenStream<'_>) -> Option<Pattern> {
    let (_, span) = ts.next_tok()?;
    let text = ts.slice(&span);
    let literal = match StringLiteral::parse(&text) {
        Ok(lit) => lit,
        Err(msg) => {
            ts.errors.push(Simple::custom(span, msg));
            return None;
        }
    };
    if literal.is_interpolated() {
        ts.errors.push(Simple::custom(
            span,
            "interpolated strings are not allowed in patterns",
        ));
        return None;
    }
    Some(Pattern::Literal(PatternLiteral::String(literal)))
}

fn parse_number_literal_pattern(ts: &mut PatternTokenStream<'_>) -> Option<Pattern> {
    let (_, span) = ts.next_tok()?;
    let text = ts.slice(&span);
    match parse_numeric_literal(&text) {
        Ok(crate::parser::ast::NumberLiteral::Int(int)) => {
            Some(Pattern::Literal(PatternLiteral::Int(int)))
        }
        Ok(crate::parser::ast::NumberLiteral::Float(_)) => {
            ts.errors
                .push(Simple::custom(span, "expected integer literal in pattern"));
            None
        }
        Err(err) => {
            ts.errors.push(Simple::custom(span, err.message()));
            None
        }
    }
}

fn parse_var_pattern(ts: &mut PatternTokenStream<'_>, declared: bool) -> Option<Pattern> {
    if declared {
        ts.next_tok(); // 'var'
    }

    let (kind, span) = ts.next_tok()?;
    if !matches!(
        kind,
        SyntaxKind::T_IDENT
            | SyntaxKind::K_FLATMAP
            | SyntaxKind::K_AGGREGATE
            | SyntaxKind::K_INSPECT
    ) {
        ts.errors
            .push(Simple::custom(span, "expected identifier in pattern"));
        return None;
    }

    let name = ts.slice(&span);
    if !is_lowercase_ident(&name) {
        ts.errors.push(Simple::custom(
            span,
            "expected a lower-case identifier in variable pattern",
        ));
        return None;
    }

    Some(Pattern::Var { declared, name })
}

fn parse_tuple_pattern(ts: &mut PatternTokenStream<'_>) -> Option<Pattern> {
    ts.next_tok(); // '('
    if ts.peek_kind() == Some(SyntaxKind::T_RPAREN) {
        ts.next_tok();
        return Some(Pattern::Tuple(Vec::new()));
    }

    let mut items = Vec::new();
    items.push(parse_pattern_inner(ts)?);

    while ts.peek_kind() == Some(SyntaxKind::T_COMMA) {
        ts.next_tok();
        if ts.peek_kind() == Some(SyntaxKind::T_RPAREN) {
            break;
        }
        items.push(parse_pattern_inner(ts)?);
    }

    if !ts.expect(SyntaxKind::T_RPAREN, "expected ')' to close tuple pattern") {
        return None;
    }

    Some(Pattern::Tuple(items))
}

fn parse_struct_pattern(ts: &mut PatternTokenStream<'_>, name: String) -> Option<Pattern> {
    ts.next_tok(); // '{'

    let mut fields = Vec::new();
    if ts.peek_kind() == Some(SyntaxKind::T_RBRACE) {
        ts.next_tok();
        return Some(Pattern::Struct { name, fields });
    }

    while let Some((field, pattern)) = parse_struct_field(ts) {
        fields.push((field, pattern));
        if !parse_field_delimiter(ts)? {
            break;
        }
    }

    if !ts.expect(SyntaxKind::T_RBRACE, "expected '}' to close struct pattern") {
        return None;
    }

    Some(Pattern::Struct { name, fields })
}

fn parse_struct_field(ts: &mut PatternTokenStream<'_>) -> Option<(String, Pattern)> {
    let (kind, span) = ts.next_tok()?;
    if !matches!(
        kind,
        SyntaxKind::T_IDENT
            | SyntaxKind::K_FLATMAP
            | SyntaxKind::K_AGGREGATE
            | SyntaxKind::K_INSPECT
    ) {
        ts.errors.push(Simple::custom(
            span,
            "expected field name in struct pattern",
        ));
        return None;
    }

    let field_name = ts.slice(&span);
    if !is_lowercase_ident(&field_name) {
        ts.errors.push(Simple::custom(
            span,
            "expected lower-case field name in struct pattern",
        ));
        return None;
    }

    if !ts.expect(SyntaxKind::T_COLON, "expected ':' after field name") {
        return None;
    }

    let value = parse_pattern_inner(ts)?;
    Some((field_name, value))
}

fn parse_field_delimiter(ts: &mut PatternTokenStream<'_>) -> Option<bool> {
    match ts.peek_kind() {
        Some(SyntaxKind::T_COMMA) => {
            ts.next_tok();
            Some(ts.peek_kind() != Some(SyntaxKind::T_RBRACE))
        }
        Some(SyntaxKind::T_RBRACE) => Some(false),
        Some(_) => {
            let span = ts.peek_span().unwrap_or_else(|| ts.eof_span());
            ts.errors.push(Simple::custom(
                span,
                "expected ',' or '}' in struct pattern",
            ));
            None
        }
        None => {
            ts.errors.push(Simple::custom(
                ts.eof_span(),
                "expected '}' to close struct pattern",
            ));
            None
        }
    }
}

fn is_uppercase_ident(ident: &str) -> bool {
    ident
        .chars()
        .next()
        .is_some_and(|ch| ch.is_ascii_uppercase())
}

fn is_lowercase_ident(ident: &str) -> bool {
    ident
        .chars()
        .next()
        .is_some_and(|ch| ch == '_' || ch.is_ascii_lowercase())
}

#[cfg(test)]
mod tests;
