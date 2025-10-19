//! Utilities for tracking delimiters while parsing composite prefix forms.

use crate::{Span, SyntaxKind};

use super::pratt::Pratt;

impl<I> Pratt<'_, I>
where
    I: Iterator<Item = (SyntaxKind, Span)> + Clone,
{
    pub(super) fn is_at_top_level(
        paren_depth: usize,
        brace_depth: usize,
        bracket_depth: usize,
    ) -> bool {
        paren_depth == 0 && brace_depth == 0 && bracket_depth == 0
    }

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

    /// Handles a closing parenthesis while collecting a for-loop pattern.
    ///
    /// Returns the updated depth and span end when the parenthesis matches,
    /// or `None` if the parser emits a diagnostic for an unmatched token or a
    /// missing `in` keyword.
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
    /// Returns `true` when every delimiter type is balanced; otherwise emits a
    /// diagnostic anchored at the most recent span (or EOF) and returns
    /// `false`.
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

    /// Extracts and trims the binding pattern text from a `for` loop header.
    ///
    /// Returns the trimmed binding and its span when present; otherwise emits
    /// diagnostics anchored at the recorded span (or the `in` keyword when no
    /// span exists) and returns `None`.
    pub(super) fn extract_pattern_text(
        &mut self,
        start: Option<usize>,
        end: Option<usize>,
        in_span: Span,
    ) -> Option<(String, Span)> {
        let (Some(s), Some(e)) = (start, end) else {
            self.ts.push_error(
                in_span.clone(),
                "expected binding before 'in' in for-loop header",
            );
            return None;
        };

        let span = s..e;
        let text = self.ts.slice(&span);
        let trimmed = text.trim();
        if trimmed.is_empty() {
            self.ts.push_error(
                span.clone(),
                "expected binding before 'in' in for-loop header",
            );
            return None;
        }

        Some((trimmed.to_string(), span))
    }
}
