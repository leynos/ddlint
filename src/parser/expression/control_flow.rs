//! Parsing of control-flow expressions (`if`, `match`, `for`).

use crate::parser::ast::{Expr, MatchArm};
use crate::{Span, SyntaxKind};

use super::pattern_collection::{DelimiterState, PatternContext, TerminationHandler};
use super::pratt::Pratt;

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

    pub(super) fn should_terminate_at_arrow(
        &self,
        kind: SyntaxKind,
        state: &DelimiterState,
    ) -> bool {
        // Retain `self` so this helper can be passed as a method reference,
        // for example via `Self::should_terminate_at_arrow` in pattern
        // collection closures.
        let _ = self;
        kind == SyntaxKind::T_ARROW && state.is_at_top_level()
    }

    pub(super) fn check_for_invalid_pattern_terminator(
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

    pub(super) fn handle_eof_in_pattern_collection(
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

    pub(super) fn handle_pattern_termination<Finalise>(
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
}
