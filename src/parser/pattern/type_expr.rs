//! Type expression parsing helpers for pattern annotations.
//!
//! Kept in a dedicated module to keep the primary pattern parser readable and
//! to ensure the parent module remains below the file-size threshold.

use chumsky::error::Simple;

use crate::SyntaxKind;

use super::PatternTokenStream;

pub(super) fn parse_type_expr(ts: &mut PatternTokenStream<'_>) -> Option<String> {
    let mut buf = String::new();
    let mut depths = DelimiterDepths::default();

    while let Some(kind) = ts.peek_kind() {
        if depths.at_top_level() && is_type_delimiter(kind) {
            break;
        }

        let (kind, span) = ts.next_tok()?;
        depths.adjust_for_token(kind);
        buf.push_str(&ts.slice(&span));
    }

    validate_type_result(ts, &buf)
}

#[derive(Debug, Default)]
struct DelimiterDepths {
    paren: usize,
    brace: usize,
    bracket: usize,
    angle: usize,
}

impl DelimiterDepths {
    fn at_top_level(&self) -> bool {
        self.paren == 0 && self.brace == 0 && self.bracket == 0 && self.angle == 0
    }

    fn adjust_for_token(&mut self, kind: SyntaxKind) {
        match kind {
            SyntaxKind::T_LPAREN => self.paren += 1,
            SyntaxKind::T_RPAREN => self.paren = self.paren.saturating_sub(1),
            SyntaxKind::T_LBRACE => self.brace += 1,
            SyntaxKind::T_RBRACE => self.brace = self.brace.saturating_sub(1),
            SyntaxKind::T_LBRACKET => self.bracket += 1,
            SyntaxKind::T_RBRACKET => self.bracket = self.bracket.saturating_sub(1),
            SyntaxKind::T_LT => self.angle += 1,
            SyntaxKind::T_GT => self.angle = self.angle.saturating_sub(1),
            SyntaxKind::T_SHL => self.angle += 2,
            SyntaxKind::T_SHR => self.angle = self.angle.saturating_sub(2),
            _ => {}
        }
    }
}

fn is_type_delimiter(kind: SyntaxKind) -> bool {
    matches!(
        kind,
        SyntaxKind::T_COMMA
            | SyntaxKind::T_RPAREN
            | SyntaxKind::T_RBRACE
            | SyntaxKind::T_RBRACKET
            | SyntaxKind::T_ARROW
            | SyntaxKind::T_EQ
            | SyntaxKind::K_IN
            | SyntaxKind::T_COLON
    )
}

fn validate_type_result(ts: &mut PatternTokenStream<'_>, buf: &str) -> Option<String> {
    let text = buf.trim();
    if text.is_empty() {
        let span = ts.peek_span().unwrap_or_else(|| ts.eof_span());
        ts.errors
            .push(Simple::custom(span, "expected type after ':' in pattern"));
        None
    } else {
        Some(text.to_string())
    }
}
