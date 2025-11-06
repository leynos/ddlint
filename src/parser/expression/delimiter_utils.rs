//! Utilities for tracking delimiters whilst parsing composite prefix forms.
//!
//! Provides depth tracking for parentheses, braces, and brackets alongside
//! delimiter-balance validation for composite prefix forms.

use crate::{Span, SyntaxKind};

use super::pratt::Pratt;

impl<I> Pratt<'_, I>
where
    I: Iterator<Item = (SyntaxKind, Span)> + Clone,
{
    /// Determines whether all delimiter depths are at the top level (zero).
    ///
    /// # Parameters
    ///
    /// - `paren_depth`: nesting count for parentheses.
    /// - `brace_depth`: nesting count for braces.
    /// - `bracket_depth`: nesting count for brackets.
    ///
    /// # Returns
    ///
    /// `true` if all delimiter depths are zero; `false` otherwise.
    pub(super) fn is_at_top_level(
        paren_depth: usize,
        brace_depth: usize,
        bracket_depth: usize,
    ) -> bool {
        paren_depth == 0 && brace_depth == 0 && bracket_depth == 0
    }

    /// Records the first and most recent span for an opening delimiter.
    ///
    /// Increments the provided depth counter and captures the start and end of
    /// the delimiter, ensuring later diagnostics can refer to the original
    /// location.
    ///
    /// # Parameters
    ///
    /// - `start`: mutable reference updated with the first observed offset.
    /// - `end`: mutable reference updated with the most recent offset.
    /// - `depth`: mutable reference to the delimiter depth counter.
    /// - `span`: span covering the opening delimiter token.
    pub(super) fn handle_open_delimiter(
        start: &mut Option<usize>,
        end: &mut Option<usize>,
        depth: &mut usize,
        span: &Span,
    ) {
        *depth += 1;
        start.get_or_insert(span.start);
        *end = Some(span.end);
    }

    /// Applies delimiter depth tracking for a closing token.
    ///
    /// Returns the span end when the closing token matches an outstanding
    /// delimiter. Emits a diagnostic, and returns `None`, when the close is
    /// unmatched.
    ///
    /// # Parameters
    ///
    /// - `span`: span covering the closing delimiter token.
    /// - `depth`: mutable reference to the tracked delimiter depth.
    /// - `unexpected_msg`: diagnostic emitted when the delimiter is unmatched.
    pub(super) fn handle_close_delimiter(
        &mut self,
        span: &Span,
        depth: &mut usize,
        unexpected_msg: &'static str,
    ) -> Option<usize> {
        if *depth == 0 {
            self.ts.push_error(span.clone(), unexpected_msg);
            return None;
        }
        *depth -= 1;
        Some(span.end)
    }

    /// Handles a closing parenthesis whilst collecting a for-loop pattern.
    ///
    /// Returns the updated depth and span end when the parenthesis matches.
    /// Returns `None` if the parser emits a diagnostic for an unmatched token
    /// or a missing `in` keyword.
    ///
    /// # Parameters
    ///
    /// - `span`: span covering the closing parenthesis.
    /// - `paren_depth`: the current parenthesis nesting depth.
    /// - `other_delimiters_open`: flag indicating whether braces remain open
    ///   alongside any brackets.
    pub(super) fn handle_close_paren(
        &mut self,
        span: &Span,
        paren_depth: usize,
        other_delimiters_open: bool,
    ) -> Option<(usize, usize)> {
        if paren_depth > 0 {
            return Some((paren_depth - 1, span.end));
        }

        if other_delimiters_open {
            self.ts.push_error(
                span.clone(),
                "unmatched closing parenthesis in for-loop pattern",
            );
        } else {
            self.ts
                .push_error(span.clone(), "expected 'in' before ')' in for-loop header");
        }

        None
    }

    fn validate_single_delimiter(
        &mut self,
        depth: usize,
        delimiter_name: &str,
        last_span: Option<&Span>,
    ) -> bool {
        if depth > 0 {
            let span = last_span.cloned().unwrap_or_else(|| self.ts.eof_span());
            self.ts.push_error(
                span,
                format!("unmatched opening {delimiter_name} in for-loop pattern: {depth} unclosed"),
            );
            return false;
        }
        true
    }

    /// Validates that parenthesis, brace, and bracket depths have all returned
    /// to zero.
    ///
    /// Emits diagnostics anchored at the most recent span (or EOF) when
    /// imbalance is detected, and returns whether the delimiters are balanced.
    ///
    /// # Parameters
    ///
    /// - `depths`: tuple of tracked depths for parentheses, braces, and
    ///   brackets.
    /// - `last_span`: the most recent delimiter span, used to anchor
    ///   diagnostics when an opener remains unmatched.
    ///
    /// # Returns
    ///
    /// `true` if all delimiters are balanced (all depths are zero); `false`
    /// if any are unmatched.
    pub(super) fn validate_delimiter_balance(
        &mut self,
        depths: (usize, usize, usize),
        last_span: Option<&Span>,
    ) -> bool {
        let (paren_depth, brace_depth, bracket_depth) = depths;
        self.validate_single_delimiter(paren_depth, "parenthesis", last_span)
            && self.validate_single_delimiter(brace_depth, "brace", last_span)
            && self.validate_single_delimiter(bracket_depth, "bracket", last_span)
    }
}
