//! Shared pattern-collection utilities for control-flow parsing.
//!
//! Provides delimiter depth tracking, configuration structs, and helpers used
//! by `match` arm and `for` header parsing. The routines here coordinate with
//! the delimiter-handling helpers to keep module responsibilities focused.

use super::pratt::Pratt;
use crate::{Span, SyntaxKind, parser::ast::Pattern};

/// Tracks delimiter depth and pattern span during pattern collection.
pub(super) struct DelimiterState {
    pub(super) paren_depth: usize,
    pub(super) brace_depth: usize,
    pub(super) bracket_depth: usize,
    pub(super) start: Option<usize>,
    pub(super) end: Option<usize>,
}

impl DelimiterState {
    pub(super) const fn new() -> Self {
        Self {
            paren_depth: 0,
            brace_depth: 0,
            bracket_depth: 0,
            start: None,
            end: None,
        }
    }

    pub(super) const fn is_at_top_level(&self) -> bool {
        type EmptyIter = std::iter::Empty<(SyntaxKind, Span)>;
        Pratt::<EmptyIter>::is_at_top_level(self.paren_depth, self.brace_depth, self.bracket_depth)
    }
}

/// The construct whose pattern is being collected.
#[derive(Clone, Copy)]
enum PatternSite {
    MatchArm,
    ForLoop,
}

impl PatternSite {
    const fn missing_terminator_msg(self) -> &'static str {
        match self {
            Self::MatchArm => "expected '->' in match arm",
            Self::ForLoop => "expected 'in' in for-loop header",
        }
    }

    const fn unmatched_delimiter_msg(self, delimiter: DelimiterKind) -> &'static str {
        match (self, delimiter) {
            (Self::MatchArm, DelimiterKind::Paren) => {
                "unmatched closing parenthesis in match pattern"
            }
            (Self::MatchArm, DelimiterKind::Brace) => "unmatched closing brace in match pattern",
            (Self::MatchArm, DelimiterKind::Bracket) => {
                "unmatched closing bracket in match pattern"
            }
            (Self::ForLoop, DelimiterKind::Paren) => {
                "unmatched closing parenthesis in for-loop pattern"
            }
            (Self::ForLoop, DelimiterKind::Brace) => "unmatched closing brace in for-loop pattern",
            (Self::ForLoop, DelimiterKind::Bracket) => {
                "unmatched closing bracket in for-loop pattern"
            }
        }
    }

    const fn unexpected_end_msg(self) -> &'static str {
        match self {
            Self::MatchArm => "unexpected end of input in match pattern",
            Self::ForLoop => "unexpected end of input in for-loop pattern",
        }
    }
}

/// A closing delimiter that may appear inside a pattern.
#[derive(Clone, Copy)]
pub(super) enum DelimiterKind {
    Paren,
    Brace,
    Bracket,
}

/// Describes context-specific diagnostics and behaviour for pattern collection.
pub(super) struct PatternContext {
    site: PatternSite,
}

impl PatternContext {
    pub(super) const fn for_match_pattern() -> Self {
        Self {
            site: PatternSite::MatchArm,
        }
    }

    pub(super) const fn for_for_loop_pattern() -> Self {
        Self {
            site: PatternSite::ForLoop,
        }
    }

    pub(super) const fn validate_on_eof(&self) -> bool { matches!(self.site, PatternSite::ForLoop) }

    pub(super) const fn use_for_paren(&self) -> bool { matches!(self.site, PatternSite::ForLoop) }

    pub(super) const fn missing_terminator_msg(&self) -> &'static str {
        self.site.missing_terminator_msg()
    }

    pub(super) const fn unmatched_delimiter_msg(&self, delimiter: DelimiterKind) -> &'static str {
        self.site.unmatched_delimiter_msg(delimiter)
    }

    pub(super) const fn unexpected_end_msg(&self) -> &'static str { self.site.unexpected_end_msg() }
}

/// Encapsulates the collection strategy for pattern parsing.
struct PatternCollectionStrategy<Terminator, PreCheck, Finalise> {
    should_terminate: Terminator,
    pre_check: PreCheck,
    consume_terminator: bool,
    finalise: Finalise,
}

impl<Terminator, PreCheck, Finalise> PatternCollectionStrategy<Terminator, PreCheck, Finalise> {
    const fn new(
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
    const fn new(
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
    pub(super) const fn new(kind: SyntaxKind, span: Span) -> Self { Self { kind, span } }
}

/// Encapsulates termination handling for pattern collection.
pub(super) struct TerminationHandler<Finalise> {
    pub(super) kind: SyntaxKind,
    pub(super) consume_terminator: bool,
    pub(super) finalise: Finalise,
}

impl<Finalise> TerminationHandler<Finalise> {
    pub(super) const fn new(
        kind: SyntaxKind,
        consume_terminator: bool,
        finalise: Finalise,
    ) -> Self {
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
        let parsed = crate::parser::pattern::parse_pattern_tokens(&tokens, self.ts.src());
        drop(tokens);

        match parsed {
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
            finalise: finalise_terminator,
        } = strategy;
        let mut state = DelimiterState::new();
        let mut last_span: Option<Span> = None;
        let mut collected: Vec<(SyntaxKind, Span)> = Vec::new();

        let terminator_kind = loop {
            let Some(kind) = self.ts.peek_kind() else {
                let _ = self.handle_eof_in_pattern_collection(&state, last_span.as_ref(), &context);
                return None;
            };

            if should_terminate(&*self, kind, &state) {
                break kind;
            }

            pre_check(self, kind, &state)?;

            let Some((token_kind, span)) = self.ts.next_tok() else {
                self.ts
                    .push_error(self.ts.eof_span(), context.unexpected_end_msg().to_owned());
                return None;
            };
            last_span = Some(span.clone());
            collected.push((token_kind, span.clone()));

            let token = DelimiterToken::new(token_kind, span);
            self.process_delimiter_token(&token, &mut state, &context)?;
        };

        let span = self.handle_pattern_termination(
            TerminationHandler::new(terminator_kind, consume_terminator, finalise_terminator),
            state,
            last_span.as_ref(),
        )?;
        Some((collected, span))
    }
}
