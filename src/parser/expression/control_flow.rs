//! Parsing of control-flow expressions (`if`, `match`, `for`).

use crate::parser::ast::{Expr, MatchArm};
use crate::{Span, SyntaxKind};

use super::pratt::Pratt;

/// Tracks delimiter depth and pattern span during pattern collection.
struct DelimiterState {
    paren_depth: usize,
    brace_depth: usize,
    bracket_depth: usize,
    start: Option<usize>,
    end: Option<usize>,
}

impl DelimiterState {
    fn new() -> Self {
        Self {
            paren_depth: 0,
            brace_depth: 0,
            bracket_depth: 0,
            start: None,
            end: None,
        }
    }

    fn is_at_top_level(&self) -> bool {
        type EmptyIter = std::iter::Empty<(SyntaxKind, Span)>;
        Pratt::<EmptyIter>::is_at_top_level(self.paren_depth, self.brace_depth, self.bracket_depth)
    }
}

impl<I> Pratt<'_, I>
where
    I: Iterator<Item = (SyntaxKind, Span)> + Clone,
{
    pub(super) fn parse_if_expression(&mut self) -> Option<Expr> {
        let condition = self.parse_if_condition()?;
        let then_branch =
            self.parse_if_clause("expected expression for 'then' branch of 'if'", None)?;
        let else_branch = if matches!(self.ts.peek_kind(), Some(SyntaxKind::K_ELSE)) {
            let else_span = self
                .ts
                .next_tok()
                .map_or_else(|| self.ts.eof_span(), |(_, span)| span);
            self.parse_if_clause(
                "expected expression for 'else' branch of 'if'",
                Some(else_span),
            )?
        } else {
            Expr::Tuple(Vec::new())
        };
        Some(Expr::IfElse {
            condition: Box::new(condition),
            then_branch: Box::new(then_branch),
            else_branch: Box::new(else_branch),
        })
    }

    fn parse_if_condition(&mut self) -> Option<Expr> {
        self.with_struct_literal_activation(|this| {
            this.parse_if_clause("expected condition expression after 'if'", None)
        })
    }

    fn parse_if_clause(&mut self, expectation: &str, fallback: Option<Span>) -> Option<Expr> {
        if matches!(self.ts.peek_kind(), Some(SyntaxKind::K_ELSE)) {
            let span = self.ts.peek_span().unwrap_or_else(|| self.ts.eof_span());
            self.ts.push_error(span, expectation.to_string());
            return None;
        }
        let error_count = self.ts.error_count();
        let expr = self.parse_expr(0);
        if expr.is_none() && self.ts.error_count() == error_count {
            let span = self
                .ts
                .peek_span()
                .or(fallback)
                .unwrap_or_else(|| self.ts.eof_span());
            self.ts.push_error(span, expectation.to_string());
        }
        expr
    }

    pub(super) fn parse_match_expression(&mut self) -> Option<Expr> {
        if !self.ts.expect(SyntaxKind::T_LPAREN) {
            return None;
        }

        let scrutinee = self.with_struct_literals_suspended(|this| this.parse_expr(0))?;

        if !self.ts.expect(SyntaxKind::T_RPAREN) {
            return None;
        }

        if !self.ts.expect(SyntaxKind::T_LBRACE) {
            return None;
        }

        let Some(arms) = self.parse_match_arms() else {
            let _ = self.ts.expect(SyntaxKind::T_RBRACE);
            return None;
        };

        if !self.ts.expect(SyntaxKind::T_RBRACE) {
            return None;
        }

        Some(Expr::Match {
            scrutinee: Box::new(scrutinee),
            arms,
        })
    }

    fn parse_match_arms(&mut self) -> Option<Vec<MatchArm>> {
        if matches!(self.ts.peek_kind(), Some(SyntaxKind::T_RBRACE)) {
            let span = self.ts.peek_span().unwrap_or_else(|| self.ts.eof_span());
            self.ts.push_error(span, "expected at least one match arm");
            return None;
        }

        let mut arms = Vec::new();
        loop {
            let arm = self.parse_single_match_arm()?;
            arms.push(arm);

            if !self.should_continue_parsing_arms()? {
                break;
            }
        }

        Some(arms)
    }

    fn parse_single_match_arm(&mut self) -> Option<MatchArm> {
        let (pattern, _) = self.collect_match_pattern()?;
        if !self.ts.expect(SyntaxKind::T_ARROW) {
            return None;
        }
        let body = self.with_struct_literals_suspended(|this| this.parse_expr(0))?;
        Some(MatchArm { pattern, body })
    }

    fn should_continue_parsing_arms(&mut self) -> Option<bool> {
        match self.ts.peek_kind() {
            Some(SyntaxKind::T_COMMA) => {
                self.ts.next_tok();
                if matches!(self.ts.peek_kind(), Some(SyntaxKind::T_RBRACE)) {
                    return Some(false);
                }
                Some(true)
            }
            Some(SyntaxKind::T_RBRACE) => Some(false),
            None => {
                self.ts
                    .push_error(self.ts.eof_span(), "expected ',' or '}' after match arm");
                None
            }
            _ => {
                let span = self.ts.peek_span().unwrap_or_else(|| self.ts.eof_span());
                self.ts
                    .push_error(span, "expected ',' or '}' after match arm");
                None
            }
        }
    }

    fn collect_match_pattern(&mut self) -> Option<(String, Span)> {
        let mut state = DelimiterState::new();
        let mut last_span: Option<Span> = None;

        loop {
            let Some(kind) = self.ts.peek_kind() else {
                self.ts
                    .push_error(self.ts.eof_span(), "expected '->' in match arm");
                return None;
            };

            if kind == SyntaxKind::T_ARROW && state.is_at_top_level() {
                break;
            }

            if state.is_at_top_level() && matches!(kind, SyntaxKind::T_RBRACE | SyntaxKind::T_COMMA)
            {
                let span = self.ts.peek_span().unwrap_or_else(|| self.ts.eof_span());
                self.ts.push_error(span, "expected '->' in match arm");
                return None;
            }

            let Some((kind, span)) = self.ts.next_tok() else {
                self.ts.push_error(
                    self.ts.eof_span(),
                    "unexpected end of input in match pattern",
                );
                return None;
            };
            last_span = Some(span.clone());

            self.process_delimiter_token(kind, &span, &mut state)?;
        }

        if !self.validate_delimiter_balance(
            (state.paren_depth, state.brace_depth, state.bracket_depth),
            last_span.as_ref(),
        ) {
            return None;
        }

        let arrow_span = self.ts.peek_span().unwrap_or_else(|| self.ts.eof_span());
        self.validate_pattern_text(state.start, state.end, arrow_span)
    }

    fn process_delimiter_token(
        &mut self,
        kind: SyntaxKind,
        span: &Span,
        state: &mut DelimiterState,
    ) -> Option<()> {
        match kind {
            SyntaxKind::T_LPAREN => {
                Self::handle_open_delimiter(
                    &mut state.start,
                    &mut state.end,
                    &mut state.paren_depth,
                    span,
                );
            }
            SyntaxKind::T_RPAREN => {
                let new_end = self.handle_close_delimiter(
                    span,
                    &mut state.paren_depth,
                    "unmatched closing parenthesis in match pattern",
                )?;
                state.end = Some(new_end);
            }
            SyntaxKind::T_LBRACE => {
                Self::handle_open_delimiter(
                    &mut state.start,
                    &mut state.end,
                    &mut state.brace_depth,
                    span,
                );
            }
            SyntaxKind::T_RBRACE => {
                let new_end = self.handle_close_delimiter(
                    span,
                    &mut state.brace_depth,
                    "unmatched closing brace in match pattern",
                )?;
                state.end = Some(new_end);
            }
            SyntaxKind::T_LBRACKET => {
                Self::handle_open_delimiter(
                    &mut state.start,
                    &mut state.end,
                    &mut state.bracket_depth,
                    span,
                );
            }
            SyntaxKind::T_RBRACKET => {
                let new_end = self.handle_close_delimiter(
                    span,
                    &mut state.bracket_depth,
                    "unmatched closing bracket in match pattern",
                )?;
                state.end = Some(new_end);
            }
            _ => {
                state.start.get_or_insert(span.start);
                state.end = Some(span.end);
            }
        }

        Some(())
    }

    fn validate_pattern_text(
        &mut self,
        start: Option<usize>,
        end: Option<usize>,
        arrow_span: Span,
    ) -> Option<(String, Span)> {
        let Some(s) = start else {
            self.ts.push_error(
                arrow_span.clone(),
                "expected pattern before '->' in match arm",
            );
            return None;
        };
        let Some(e) = end else {
            self.ts.push_error(
                arrow_span.clone(),
                "expected pattern before '->' in match arm",
            );
            return None;
        };

        let span = s..e;
        let text = self.ts.slice(&span);
        let trimmed = text.trim();
        if trimmed.is_empty() {
            self.ts
                .push_error(span.clone(), "expected pattern before '->' in match arm");
            return None;
        }

        Some((trimmed.to_string(), span))
    }

    pub(super) fn parse_for_expression(&mut self) -> Option<Expr> {
        if !self.ts.expect(SyntaxKind::T_LPAREN) {
            return None;
        }

        let (pattern, pattern_span) = self.collect_for_pattern()?;
        if pattern.is_empty() {
            self.ts.push_error(
                pattern_span,
                "expected binding before 'in' in for-loop header",
            );
            return None;
        }

        let iterable = self.with_struct_literals_suspended(|this| this.parse_expr(0))?;

        let guard = if matches!(self.ts.peek_kind(), Some(SyntaxKind::K_IF)) {
            let (_, if_span) = self
                .ts
                .next_tok()
                .unwrap_or_else(|| (SyntaxKind::K_IF, self.ts.eof_span()));
            let guard_expr = self.with_struct_literal_activation(|this| {
                this.parse_if_clause(
                    "expected guard expression after 'if' in for-loop header",
                    Some(if_span.clone()),
                )
            })?;
            Some(Box::new(guard_expr))
        } else {
            None
        };

        if !self.ts.expect(SyntaxKind::T_RPAREN) {
            return None;
        }

        let body = self.with_struct_literals_suspended(|this| this.parse_expr(0))?;

        Some(Expr::ForLoop {
            pattern,
            iterable: Box::new(iterable),
            guard,
            body: Box::new(body),
        })
    }

    fn collect_for_pattern(&mut self) -> Option<(String, Span)> {
        let mut paren_depth = 0usize;
        let mut brace_depth = 0usize;
        let mut bracket_depth = 0usize;
        let mut start = None;
        let mut end = None;
        let mut last_span: Option<Span> = None;

        while let Some((kind, span)) = self.ts.next_tok() {
            last_span = Some(span.clone());
            match kind {
                SyntaxKind::K_IN
                    if Self::is_at_top_level(paren_depth, brace_depth, bracket_depth) =>
                {
                    return self.extract_pattern_text(start, end, span);
                }
                SyntaxKind::T_LPAREN => {
                    Self::handle_open_delimiter(&mut start, &mut end, &mut paren_depth, &span);
                }
                SyntaxKind::T_RPAREN => {
                    let (new_depth, new_end) = self.handle_close_paren(
                        &span,
                        paren_depth,
                        brace_depth > 0 || bracket_depth > 0,
                    )?;
                    paren_depth = new_depth;
                    end = Some(new_end);
                }
                SyntaxKind::T_LBRACE => {
                    Self::handle_open_delimiter(&mut start, &mut end, &mut brace_depth, &span);
                }
                SyntaxKind::T_RBRACE => {
                    end = Some(self.handle_close_delimiter(
                        &span,
                        &mut brace_depth,
                        "unmatched closing brace in for-loop pattern",
                    )?);
                }
                SyntaxKind::T_LBRACKET => {
                    Self::handle_open_delimiter(&mut start, &mut end, &mut bracket_depth, &span);
                }
                SyntaxKind::T_RBRACKET => {
                    end = Some(self.handle_close_delimiter(
                        &span,
                        &mut bracket_depth,
                        "unmatched closing bracket in for-loop pattern",
                    )?);
                }
                _ => {
                    start.get_or_insert(span.start);
                    end = Some(span.end);
                }
            }
        }

        if !self.validate_delimiter_balance(
            (paren_depth, brace_depth, bracket_depth),
            last_span.as_ref(),
        ) {
            return None;
        }

        self.ts
            .push_error(self.ts.eof_span(), "expected 'in' in for-loop header");
        None
    }
}
