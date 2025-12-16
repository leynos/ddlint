//! Shared pattern-collection utilities for control-flow parsing.
//!
//! Provides delimiter depth tracking, configuration structs, and helpers used
//! by `match` arm and `for` header parsing. The routines here coordinate with
//! the delimiter-handling helpers to keep module responsibilities focused.

use crate::{Span, SyntaxKind};

use crate::parser::ast::Pattern;

use super::pratt::Pratt;

/// Tracks delimiter depth and pattern span during pattern collection.
pub(super) struct DelimiterState {
    pub(super) paren_depth: usize,
    pub(super) brace_depth: usize,
    pub(super) bracket_depth: usize,
    pub(super) start: Option<usize>,
    pub(super) end: Option<usize>,
}

impl DelimiterState {
    pub(super) fn new() -> Self {
        Self {
            paren_depth: 0,
            brace_depth: 0,
            bracket_depth: 0,
            start: None,
            end: None,
        }
    }

    pub(super) fn is_at_top_level(&self) -> bool {
        type EmptyIter = std::iter::Empty<(SyntaxKind, Span)>;
        Pratt::<EmptyIter>::is_at_top_level(self.paren_depth, self.brace_depth, self.bracket_depth)
    }
}

/// Describes context-specific diagnostics for pattern collection.
pub(super) struct PatternContext {
    pub(super) missing_terminator_msg: &'static str,
    pub(super) unmatched_paren_msg: &'static str,
    pub(super) unmatched_brace_msg: &'static str,
    pub(super) unmatched_bracket_msg: &'static str,
    pub(super) unexpected_end_msg: &'static str,
    pub(super) validate_on_eof: bool,
    pub(super) use_for_paren: bool,
}

impl PatternContext {
    fn new(
        pattern_type: &'static str,
        missing_terminator_msg: &'static str,
        validate_on_eof: bool,
        use_for_paren: bool,
    ) -> Self {
        let (unmatched_paren_msg, unmatched_brace_msg, unmatched_bracket_msg, unexpected_end_msg) =
            match pattern_type {
                "match pattern" => (
                    "unmatched closing parenthesis in match pattern",
                    "unmatched closing brace in match pattern",
                    "unmatched closing bracket in match pattern",
                    "unexpected end of input in match pattern",
                ),
                "for-loop pattern" => (
                    "unmatched closing parenthesis in for-loop pattern",
                    "unmatched closing brace in for-loop pattern",
                    "unmatched closing bracket in for-loop pattern",
                    "unexpected end of input in for-loop pattern",
                ),
                _ => unreachable!("unknown pattern context: {pattern_type}"),
            };

        Self {
            missing_terminator_msg,
            unmatched_paren_msg,
            unmatched_brace_msg,
            unmatched_bracket_msg,
            unexpected_end_msg,
            validate_on_eof,
            use_for_paren,
        }
    }

    pub(super) fn for_match_pattern() -> Self {
        Self::new("match pattern", "expected '->' in match arm", false, false)
    }

    pub(super) fn for_for_loop_pattern() -> Self {
        Self::new(
            "for-loop pattern",
            "expected 'in' in for-loop header",
            true,
            true,
        )
    }
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

/// Complete configuration for pattern collection including strategy and context.
struct PatternCollectionConfig<Terminator, PreCheck, Finalise> {
    strategy: PatternCollectionStrategy<Terminator, PreCheck, Finalise>,
    context: PatternContext,
}

impl<Terminator, PreCheck, Finalise> PatternCollectionConfig<Terminator, PreCheck, Finalise> {
    fn new(
        strategy: PatternCollectionStrategy<Terminator, PreCheck, Finalise>,
        context: PatternContext,
    ) -> Self {
        Self { strategy, context }
    }
}

/// A token being processed during delimiter tracking.
pub(super) struct DelimiterToken {
    pub(super) kind: SyntaxKind,
    pub(super) span: Span,
}

impl DelimiterToken {
    pub(super) fn new(kind: SyntaxKind, span: Span) -> Self {
        Self { kind, span }
    }
}

/// Encapsulates termination handling for pattern collection.
pub(super) struct TerminationHandler<Finalise> {
    pub(super) kind: SyntaxKind,
    pub(super) consume_terminator: bool,
    pub(super) finalise: Finalise,
}

impl<Finalise> TerminationHandler<Finalise> {
    pub(super) fn new(kind: SyntaxKind, consume_terminator: bool, finalise: Finalise) -> Self {
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
    pub(super) fn collect_match_pattern(&mut self) -> Option<(Pattern, Span)> {
        let (tokens, span) = self.collect_pattern_until(PatternCollectionConfig::new(
            PatternCollectionStrategy::new(
                Self::should_terminate_at_arrow,
                Self::check_for_invalid_pattern_terminator,
                false,
                |this: &mut Self, state: DelimiterState, arrow_span: Span| {
                    this.validate_match_pattern_span(state.start, state.end, arrow_span)
                },
            ),
            PatternContext::for_match_pattern(),
        ))?;

        self.parse_collected_pattern_tokens(tokens, span)
    }

    pub(super) fn collect_for_pattern(&mut self) -> Option<(Pattern, Span)> {
        let (tokens, span) = self.collect_pattern_until(PatternCollectionConfig::new(
            PatternCollectionStrategy::new(
                |_this: &Self, kind, state: &DelimiterState| {
                    kind == SyntaxKind::K_IN && state.is_at_top_level()
                },
                |_this: &mut Self, _kind: SyntaxKind, _state: &DelimiterState| Some(()),
                true,
                |this: &mut Self, state: DelimiterState, in_span: Span| {
                    this.validate_for_pattern_span(state.start, state.end, in_span)
                },
            ),
            PatternContext::for_for_loop_pattern(),
        ))?;

        self.parse_collected_pattern_tokens(tokens, span)
    }

    fn parse_collected_pattern_tokens(
        &mut self,
        tokens: Vec<(SyntaxKind, Span)>,
        span: Span,
    ) -> Option<(Pattern, Span)> {
        let tokens = tokens.into_boxed_slice();
        match crate::parser::pattern::parse_pattern_tokens(&tokens, self.ts.src()) {
            Ok(pattern) => Some((pattern, span)),
            Err(errs) => {
                self.ts.extend_errors(errs);
                None
            }
        }
    }

    pub(super) fn validate_match_pattern_span(
        &mut self,
        start: Option<usize>,
        end: Option<usize>,
        arrow_span: Span,
    ) -> Option<Span> {
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

        Some(s..e)
    }

    pub(super) fn validate_for_pattern_span(
        &mut self,
        start: Option<usize>,
        end: Option<usize>,
        in_span: Span,
    ) -> Option<Span> {
        let (Some(s), Some(e)) = (start, end) else {
            self.ts.push_error(
                in_span.clone(),
                "expected binding before 'in' in for-loop header",
            );
            return None;
        };

        Some(s..e)
    }

    fn collect_pattern_until<Terminator, PreCheck, Finalise>(
        &mut self,
        config: PatternCollectionConfig<Terminator, PreCheck, Finalise>,
    ) -> Option<(Vec<(SyntaxKind, Span)>, Span)>
    where
        Terminator: FnMut(&Self, SyntaxKind, &DelimiterState) -> bool,
        PreCheck: FnMut(&mut Self, SyntaxKind, &DelimiterState) -> Option<()>,
        Finalise: FnOnce(&mut Self, DelimiterState, Span) -> Option<Span>,
    {
        let PatternCollectionConfig { strategy, context } = config;
        let PatternCollectionStrategy {
            mut should_terminate,
            mut pre_check,
            consume_terminator,
            finalise,
        } = strategy;
        let mut finalise_fn = Some(finalise);

        let mut state = DelimiterState::new();
        let mut last_span: Option<Span> = None;
        let mut collected: Vec<(SyntaxKind, Span)> = Vec::new();

        loop {
            let Some(kind) = self.ts.peek_kind() else {
                let _ = self.handle_eof_in_pattern_collection(&state, last_span.as_ref(), &context);
                return None;
            };

            if should_terminate(&*self, kind, &state) {
                #[expect(
                    clippy::unreachable,
                    reason = "pattern finaliser consumed exactly once"
                )]
                let Some(finalise) = finalise_fn.take() else {
                    unreachable!("pattern finaliser already consumed");
                };
                let span = self.handle_pattern_termination(
                    TerminationHandler::new(kind, consume_terminator, finalise),
                    state,
                    last_span.as_ref(),
                )?;
                return Some((collected, span));
            }

            pre_check(self, kind, &state)?;

            let Some((kind, span)) = self.ts.next_tok() else {
                self.ts
                    .push_error(self.ts.eof_span(), context.unexpected_end_msg.to_string());
                return None;
            };
            last_span = Some(span.clone());
            collected.push((kind, span.clone()));

            let token = DelimiterToken::new(kind, span);
            self.process_delimiter_token(&token, &mut state, &context)?;
        }
    }
}
