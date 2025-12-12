//! Handles delimiter depth updates during pattern collection.
//!
//! These helpers cooperate with pattern collection routines to track opening
//! and closing tokens, ensuring diagnostics remain precise without bloating the
//! control-flow parsing module.

use crate::{Span, SyntaxKind};

use super::pattern_collection::{DelimiterState, DelimiterToken, PatternContext};
use super::pratt::Pratt;

impl<I> Pratt<'_, I>
where
    I: Iterator<Item = (SyntaxKind, Span)> + Clone,
{
    pub(super) fn process_delimiter_token(
        &mut self,
        token: &DelimiterToken,
        state: &mut DelimiterState,
        context: &PatternContext,
    ) -> Option<()> {
        let kind = token.kind;
        let span = &token.span;

        match kind {
            SyntaxKind::T_LPAREN | SyntaxKind::T_LBRACE | SyntaxKind::T_LBRACKET => {
                Self::process_opening_delimiter(kind, span, state);
            }
            SyntaxKind::T_RPAREN | SyntaxKind::T_RBRACE | SyntaxKind::T_RBRACKET => {
                self.process_closing_delimiter(token, state, context)?;
            }
            SyntaxKind::T_STRING => {
                if !self.reject_interpolated_string_in_pattern(span) {
                    return None; // diagnostic already emitted
                }
                state.start.get_or_insert(span.start);
                state.end = Some(span.end);
            }
            _ => {
                state.start.get_or_insert(span.start);
                state.end = Some(span.end);
            }
        }

        Some(())
    }

    #[expect(
        clippy::unreachable,
        reason = "caller ensures kind is opening delimiter"
    )]
    pub(super) fn process_opening_delimiter(
        kind: SyntaxKind,
        span: &Span,
        state: &mut DelimiterState,
    ) {
        let depth = match kind {
            SyntaxKind::T_LPAREN => &mut state.paren_depth,
            SyntaxKind::T_LBRACE => &mut state.brace_depth,
            SyntaxKind::T_LBRACKET => &mut state.bracket_depth,
            _ => unreachable!("process_opening_delimiter called with non-opening delimiter"),
        };

        Self::handle_open_delimiter(&mut state.start, &mut state.end, depth, span);
    }

    #[expect(
        clippy::unreachable,
        reason = "caller ensures kind is closing delimiter"
    )]
    pub(super) fn process_closing_delimiter(
        &mut self,
        token: &DelimiterToken,
        state: &mut DelimiterState,
        context: &PatternContext,
    ) -> Option<()> {
        let kind = token.kind;
        let span = &token.span;

        match kind {
            SyntaxKind::T_RPAREN => {
                if context.use_for_paren {
                    let other_open = state.brace_depth > 0 || state.bracket_depth > 0;
                    let (new_depth, new_end) =
                        self.handle_close_paren(span, state.paren_depth, other_open)?;
                    state.paren_depth = new_depth;
                    state.end = Some(new_end);
                } else {
                    let new_end = self.handle_close_delimiter(
                        span,
                        &mut state.paren_depth,
                        context.unmatched_paren_msg,
                    )?;
                    state.end = Some(new_end);
                }
            }
            SyntaxKind::T_RBRACE => {
                let new_end = self.handle_close_delimiter(
                    span,
                    &mut state.brace_depth,
                    context.unmatched_brace_msg,
                )?;
                state.end = Some(new_end);
            }
            SyntaxKind::T_RBRACKET => {
                let new_end = self.handle_close_delimiter(
                    span,
                    &mut state.bracket_depth,
                    context.unmatched_bracket_msg,
                )?;
                state.end = Some(new_end);
            }
            _ => unreachable!("process_closing_delimiter called with non-closing delimiter"),
        }

        Some(())
    }

    fn reject_interpolated_string_in_pattern(&mut self, span: &Span) -> bool {
        let literal = match crate::parser::ast::StringLiteral::parse(&self.ts.slice(span)) {
            Ok(lit) => lit,
            Err(msg) => {
                self.ts.push_error(span.clone(), msg.to_string());
                return false;
            }
        };

        // Cache the parsed literal for later reuse
        self.string_literal_cache
            .insert(span.start, literal.clone());

        if literal.is_interpolated() {
            self.ts.push_error(
                span.clone(),
                "interpolated strings are not allowed in patterns".to_string(),
            );
            return false;
        }

        true
    }
}
