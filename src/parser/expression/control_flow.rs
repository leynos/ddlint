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

/// Describes context-specific diagnostics for pattern collection.
struct PatternContext {
    missing_terminator_msg: &'static str,
    unmatched_paren_msg: &'static str,
    unmatched_brace_msg: &'static str,
    unmatched_bracket_msg: &'static str,
    unexpected_end_msg: &'static str,
    validate_on_eof: bool,
    use_for_paren: bool,
}

/// Encapsulates the collection strategy for pattern parsing.
struct PatternCollectionStrategy<Terminator, PreCheck, Finalise> {
    should_terminate: Terminator,
    pre_check: PreCheck,
    consume_terminator: bool,
    finalise: Finalise,
}

impl<Terminator, PreCheck, Finalise> PatternCollectionStrategy<Terminator, PreCheck, Finalise> {
    fn new(
        should_terminate: Terminator,
        pre_check: PreCheck,
        consume_terminator: bool,
        finalise: Finalise,
    ) -> Self {
        Self {
            should_terminate,
            pre_check,
            consume_terminator,
            finalise,
        }
    }
}

/// A token being processed during delimiter tracking.
struct DelimiterToken {
    kind: SyntaxKind,
    span: Span,
}

impl DelimiterToken {
    fn new(kind: SyntaxKind, span: Span) -> Self {
        Self { kind, span }
    }
}

/// Encapsulates termination handling for pattern collection.
struct TerminationHandler<Finalise> {
    kind: SyntaxKind,
    consume_terminator: bool,
    finalise: Finalise,
}

impl<Finalise> TerminationHandler<Finalise> {
    fn new(kind: SyntaxKind, consume_terminator: bool, finalise: Finalise) -> Self {
        Self {
            kind,
            consume_terminator,
            finalise,
        }
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

        let first_arm = self.parse_single_match_arm()?;
        let mut arms = Vec::with_capacity(1);
        arms.push(first_arm);

        if matches!(self.ts.peek_kind(), Some(SyntaxKind::T_RBRACE)) {
            return Some(arms);
        }

        loop {
            if !self.should_continue_parsing_arms()? {
                break;
            }

            let arm = self.parse_single_match_arm()?;
            arms.push(arm);
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

    fn should_terminate_at_arrow(&self, kind: SyntaxKind, state: &DelimiterState) -> bool {
        let _ = self;
        kind == SyntaxKind::T_ARROW && state.is_at_top_level()
    }

    fn check_for_invalid_pattern_terminator(
        &mut self,
        kind: SyntaxKind,
        state: &DelimiterState,
    ) -> Option<()> {
        if state.is_at_top_level() && matches!(kind, SyntaxKind::T_RBRACE | SyntaxKind::T_COMMA) {
            let span = self.ts.peek_span().unwrap_or_else(|| self.ts.eof_span());
            self.ts.push_error(span, "expected '->' in match arm");
            return None;
        }
        Some(())
    }

    fn handle_eof_in_pattern_collection(
        &mut self,
        state: &DelimiterState,
        last_span: Option<&Span>,
        context: &PatternContext,
    ) -> Option<(String, Span)> {
        if context.validate_on_eof
            && !self.validate_delimiter_balance(
                (state.paren_depth, state.brace_depth, state.bracket_depth),
                last_span,
            )
        {
            return None;
        }

        self.ts.push_error(
            self.ts.eof_span(),
            context.missing_terminator_msg.to_string(),
        );
        None
    }

    fn handle_pattern_termination<Finalise>(
        &mut self,
        handler: TerminationHandler<Finalise>,
        state: DelimiterState,
        last_span: Option<&Span>,
    ) -> Option<(String, Span)>
    where
        Finalise: FnOnce(&mut Self, DelimiterState, Span) -> Option<(String, Span)>,
    {
        let TerminationHandler {
            kind,
            consume_terminator,
            finalise,
        } = handler;

        if !self.validate_delimiter_balance(
            (state.paren_depth, state.brace_depth, state.bracket_depth),
            last_span,
        ) {
            return None;
        }

        let terminator_span = if consume_terminator {
            let (_, span) = self
                .ts
                .next_tok()
                .unwrap_or_else(|| (kind, self.ts.eof_span()));
            span
        } else {
            self.ts.peek_span().unwrap_or_else(|| self.ts.eof_span())
        };

        (finalise)(self, state, terminator_span)
    }

    fn collect_pattern_until<Terminator, PreCheck, Finalise>(
        &mut self,
        strategy: PatternCollectionStrategy<Terminator, PreCheck, Finalise>,
        context: &PatternContext,
    ) -> Option<(String, Span)>
    where
        Terminator: FnMut(&Self, SyntaxKind, &DelimiterState) -> bool,
        PreCheck: FnMut(&mut Self, SyntaxKind, &DelimiterState) -> Option<()>,
        Finalise: FnOnce(&mut Self, DelimiterState, Span) -> Option<(String, Span)>,
    {
        let PatternCollectionStrategy {
            mut should_terminate,
            mut pre_check,
            consume_terminator,
            finalise,
        } = strategy;
        let mut finalise_fn = Some(finalise);

        let mut state = DelimiterState::new();
        let mut last_span: Option<Span> = None;

        loop {
            let Some(kind) = self.ts.peek_kind() else {
                return self.handle_eof_in_pattern_collection(&state, last_span.as_ref(), context);
            };

            if should_terminate(&*self, kind, &state) {
                let Some(finalise) = finalise_fn.take() else {
                    unreachable!("pattern finaliser already consumed");
                };
                return self.handle_pattern_termination(
                    TerminationHandler::new(kind, consume_terminator, finalise),
                    state,
                    last_span.as_ref(),
                );
            }

            pre_check(self, kind, &state)?;

            let Some((kind, span)) = self.ts.next_tok() else {
                self.ts
                    .push_error(self.ts.eof_span(), context.unexpected_end_msg.to_string());
                return None;
            };
            last_span = Some(span.clone());

            let token = DelimiterToken::new(kind, span);
            self.process_delimiter_token(&token, &mut state, context)?;
        }
    }

    fn collect_match_pattern(&mut self) -> Option<(String, Span)> {
        let context = PatternContext {
            missing_terminator_msg: "expected '->' in match arm",
            unmatched_paren_msg: "unmatched closing parenthesis in match pattern",
            unmatched_brace_msg: "unmatched closing brace in match pattern",
            unmatched_bracket_msg: "unmatched closing bracket in match pattern",
            unexpected_end_msg: "unexpected end of input in match pattern",
            validate_on_eof: false,
            use_for_paren: false,
        };

        self.collect_pattern_until(
            PatternCollectionStrategy::new(
                Self::should_terminate_at_arrow,
                Self::check_for_invalid_pattern_terminator,
                false,
                |this: &mut Self, state: DelimiterState, arrow_span: Span| {
                    this.validate_pattern_text(state.start, state.end, arrow_span)
                },
            ),
            &context,
        )
    }

    fn process_opening_delimiter(kind: SyntaxKind, span: &Span, state: &mut DelimiterState) {
        let depth = match kind {
            SyntaxKind::T_LPAREN => &mut state.paren_depth,
            SyntaxKind::T_LBRACE => &mut state.brace_depth,
            SyntaxKind::T_LBRACKET => &mut state.bracket_depth,
            _ => unreachable!("process_opening_delimiter called with non-opening delimiter"),
        };

        Self::handle_open_delimiter(&mut state.start, &mut state.end, depth, span);
    }

    fn process_closing_delimiter(
        &mut self,
        kind: SyntaxKind,
        span: &Span,
        state: &mut DelimiterState,
        context: &PatternContext,
    ) -> Option<()> {
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

    fn process_delimiter_token(
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
                self.process_closing_delimiter(kind, span, state, context)?;
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
        let context = PatternContext {
            missing_terminator_msg: "expected 'in' in for-loop header",
            unmatched_paren_msg: "unmatched closing parenthesis in for-loop pattern",
            unmatched_brace_msg: "unmatched closing brace in for-loop pattern",
            unmatched_bracket_msg: "unmatched closing bracket in for-loop pattern",
            unexpected_end_msg: "unexpected end of input in for-loop pattern",
            validate_on_eof: true,
            use_for_paren: true,
        };

        self.collect_pattern_until(
            PatternCollectionStrategy::new(
                |_this: &Self, kind, state: &DelimiterState| {
                    kind == SyntaxKind::K_IN && state.is_at_top_level()
                },
                |_this: &mut Self, _kind: SyntaxKind, _state: &DelimiterState| Some(()),
                true,
                |this: &mut Self, state: DelimiterState, in_span: Span| {
                    this.extract_pattern_text(state.start, state.end, in_span)
                },
            ),
            &context,
        )
    }
}
