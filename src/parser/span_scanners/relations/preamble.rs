//! Relation role and kind preamble parsing.

use chumsky::{Error, error::Simple};

use crate::{Span, SyntaxKind};

use super::ScanResult;
use super::cursor::skip_trivia;

const D_REL_001: &str =
    "D-REL-001: relation role keyword (input/output) must precede the kind keyword";
const D_REL_002: &str = "D-REL-002: at most one role keyword (input, output) is permitted";
const D_REL_003: &str =
    "D-REL-003: at most one kind keyword (relation, stream, multiset) is permitted";

/// Relation role annotation parsed from a relation preamble keyword.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum RelationRole {
    /// Input relation role introduced by the `input` keyword.
    Input,
    /// Output relation role introduced by the `output` keyword.
    Output,
}

/// Parsed relation preamble state.
///
/// # Fields
///
/// - `role` records an optional `input` or `output` preamble keyword.
/// - `has_kind` records whether `relation`, `stream`, or `multiset` was
///   present before the relation name.
#[derive(Debug)]
pub(super) struct Preamble {
    /// Optional `input` or `output` relation role.
    pub(super) role: Option<RelationRole>,
    /// Whether a relation kind keyword was present.
    pub(super) has_kind: bool,
}

/// Consume role and kind keywords from a relation declaration preamble.
///
/// The cursor advances through an optional role keyword followed by an
/// optional kind keyword while enforcing D-REL-001, D-REL-002, and D-REL-003.
/// It stops at the relation name or at the first token that cannot belong to a
/// preamble.
///
/// # Examples
///
/// ```rust,ignore
/// parse_preamble(tokens("input relation R(x: u32)"), &mut 0)?;
/// // => Preamble { role: Some(Input), has_kind: true }
///
/// parse_preamble(tokens("stream R(x: u32)"), &mut 0)?;
/// // => Preamble { role: None, has_kind: true }
///
/// parse_preamble(tokens("R(x: u32)"), &mut 0)?;
/// // => Preamble { role: None, has_kind: false }
/// ```
///
/// # Errors
///
/// Returns D-REL-001 when a role follows a leading kind, D-REL-002 when more
/// than one role keyword is present, D-REL-003 when more than one kind keyword
/// is present, or a structured parser error when the current token cannot
/// begin a relation declaration.
pub(super) fn parse_preamble(
    tokens: &[(SyntaxKind, Span)],
    cursor: &mut usize,
) -> ScanResult<Preamble> {
    let mut state = PreambleState::default();
    state.consume_first(tokens, cursor)?;
    state.consume_remaining(tokens, cursor)?;
    Ok(state.finish())
}

#[derive(Debug, Default)]
struct PreambleState {
    role: Option<RelationRole>,
    has_kind: bool,
    saw_kind_first: bool,
}

impl PreambleState {
    fn consume_first(
        &mut self,
        tokens: &[(SyntaxKind, Span)],
        cursor: &mut usize,
    ) -> ScanResult<()> {
        let Some((kind, span)) = tokens.get(*cursor) else {
            return Ok(());
        };
        match preamble_part(*kind) {
            PreamblePart::Role(role) => {
                self.role = Some(role);
                advance_preamble(tokens, cursor);
                Ok(())
            }
            PreamblePart::Kind => {
                self.has_kind = true;
                self.saw_kind_first = true;
                advance_preamble(tokens, cursor);
                Ok(())
            }
            PreamblePart::Name => Ok(()),
            PreamblePart::Other(found) => Err(Box::new(Simple::expected_input_found(
                span.clone(),
                [Some(SyntaxKind::T_IDENT)],
                Some(found),
            ))),
        }
    }

    fn consume_remaining(
        &mut self,
        tokens: &[(SyntaxKind, Span)],
        cursor: &mut usize,
    ) -> ScanResult<()> {
        while let Some((kind, span)) = tokens.get(*cursor) {
            match preamble_part(*kind) {
                PreamblePart::Role(role) => self.consume_role(role, span, tokens, cursor)?,
                PreamblePart::Kind => self.consume_kind(span, tokens, cursor)?,
                PreamblePart::Name | PreamblePart::Other(_) => break,
            }
        }
        Ok(())
    }

    fn consume_role(
        &mut self,
        role: RelationRole,
        span: &Span,
        tokens: &[(SyntaxKind, Span)],
        cursor: &mut usize,
    ) -> ScanResult<()> {
        if self.saw_kind_first {
            return Err(Box::new(custom_error(span, D_REL_001)));
        }
        if self.role.is_some() {
            return Err(Box::new(custom_error(span, D_REL_002)));
        }
        self.role = Some(role);
        advance_preamble(tokens, cursor);
        Ok(())
    }

    fn consume_kind(
        &mut self,
        span: &Span,
        tokens: &[(SyntaxKind, Span)],
        cursor: &mut usize,
    ) -> ScanResult<()> {
        if self.has_kind {
            return Err(Box::new(custom_error(span, D_REL_003)));
        }
        self.has_kind = true;
        advance_preamble(tokens, cursor);
        Ok(())
    }

    fn finish(self) -> Preamble {
        Preamble {
            role: self.role,
            has_kind: self.has_kind,
        }
    }
}

fn custom_error(span: &Span, message: &'static str) -> Simple<SyntaxKind> {
    Simple::custom(span.clone(), message)
}

enum PreamblePart {
    Role(RelationRole),
    Kind,
    Name,
    Other(SyntaxKind),
}

fn preamble_part(kind: SyntaxKind) -> PreamblePart {
    match kind {
        SyntaxKind::K_INPUT => PreamblePart::Role(RelationRole::Input),
        SyntaxKind::K_OUTPUT => PreamblePart::Role(RelationRole::Output),
        SyntaxKind::K_RELATION | SyntaxKind::K_STREAM | SyntaxKind::K_MULTISET => {
            PreamblePart::Kind
        }
        SyntaxKind::T_IDENT => PreamblePart::Name,
        other => PreamblePart::Other(other),
    }
}

fn advance_preamble(tokens: &[(SyntaxKind, Span)], cursor: &mut usize) {
    *cursor += 1;
    skip_trivia(tokens, cursor);
}
