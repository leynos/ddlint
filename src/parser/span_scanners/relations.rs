//! Scanner for relation declarations.
//!
//! Parses relation declarations before CST construction, including role/kind
//! preambles, record and bracket body forms, ref markers, and primary-key
//! clause recovery.

mod cursor;
mod preamble;

use chumsky::error::Simple;

use crate::{Span, SyntaxKind};

use cursor::{
    adjust_stack, consume_kind, consume_text, current_span, is_trivia, parse_balanced_block,
    skip_inline_trivia, skip_trivia, skip_trivia_end, token_text, token_text_at,
};
use preamble::{RelationRole, parse_preamble};

use super::utils::State;

pub(super) type ScanResult<T> = Result<T, Box<Simple<SyntaxKind>>>;

const D_REL_004: &str = "D-REL-004: bracket-form relations cannot declare a primary key clause";
const D_REL_005: &str =
    "D-REL-005: bracket-form relations require a single element type between '[' and ']'";
const D_REL_006: &str = "D-REL-006: primary key clauses are only valid on input relations";
pub(super) const D_REL_007: &str = "D-REL-007: unexpected or malformed primary key clause";
const D_REL_008: &str = "D-REL-008: bracket-wrapped primary key clauses are not supported; remove the surrounding '['/']";

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum BodyForm {
    Record,
    Bracket,
}

#[derive(Debug)]
struct ParsedRelation {
    end: usize,
    recovery_end: usize,
    error: Option<Simple<SyntaxKind>>,
}

/// Extract relation declaration spans from the token stream.
///
/// * `tokens` - Slice of `(SyntaxKind, Span)` pairs representing the tokenised source.
/// * `src` - Source string used for span slicing and diagnostic context.
///
/// Returns `(Vec<Span>, Vec<Simple<SyntaxKind>>)` containing collected spans and any parse errors.
pub(crate) fn collect_relation_spans(
    tokens: &[(SyntaxKind, Span)],
    src: &str,
) -> (Vec<Span>, Vec<Simple<SyntaxKind>>) {
    let mut st: State<'_> = State::new(tokens, src, Vec::new());
    let mut context = RelationScanContext::default();
    let mut observed = 0usize;

    while let Some(&(kind, ref span_ref)) = st.stream.peek() {
        let span = span_ref.clone();
        if is_relation_candidate(kind) {
            handle_candidate(&mut st, span, context.is_top_level_line_start());
        } else {
            st.stream.advance();
        }
        observe_scanned_tokens(&mut context, tokens, src, &mut observed, st.stream.cursor());
    }

    st.into_parts()
}

fn handle_candidate(st: &mut State<'_>, span: Span, is_top_level_line_start: bool) {
    if !is_top_level_line_start {
        st.stream.advance();
        return;
    }

    let start = span.start;
    match parse_relation_decl(st.stream.tokens(), st.stream.src(), st.stream.cursor()) {
        Ok(Some(parsed)) => {
            st.spans.push(start..parsed.end);
            if let Some(error) = parsed.error {
                st.extra.push(error);
            }
            st.stream.skip_until(parsed.recovery_end);
        }
        Ok(None) => st.stream.advance(),
        Err(error) => {
            st.extra.push(*error);
            st.skip_line();
        }
    }
}

fn parse_relation_decl(
    tokens: &[(SyntaxKind, Span)],
    src: &str,
    mut cursor: usize,
) -> ScanResult<Option<ParsedRelation>> {
    let preamble = parse_preamble(tokens, &mut cursor)?;
    skip_trivia(tokens, &mut cursor);

    if matches!(tokens.get(cursor), Some((SyntaxKind::T_AMP, _))) {
        cursor += 1;
        skip_trivia(tokens, &mut cursor);
    }

    consume_kind(tokens, src, &mut cursor, SyntaxKind::T_IDENT)?;
    skip_trivia(tokens, &mut cursor);

    let Some((body_open, _)) = tokens.get(cursor) else {
        return Ok(None);
    };
    let body = match body_open {
        SyntaxKind::T_LPAREN => BodyForm::Record,
        SyntaxKind::T_LBRACKET => BodyForm::Bracket,
        _ => return Ok(None),
    };
    let body_end = parse_body(tokens, src, &mut cursor, body)?;

    let has_preamble = preamble.role.is_some() || preamble.has_kind;
    if is_bare_rule_or_fact(tokens, src, &mut cursor, has_preamble) {
        return Ok(None);
    }

    parse_relation_suffix(tokens, src, cursor, preamble.role, body, body_end).map(Some)
}

fn parse_body(
    tokens: &[(SyntaxKind, Span)],
    src: &str,
    cursor: &mut usize,
    body: BodyForm,
) -> ScanResult<usize> {
    let close = match body {
        BodyForm::Record => SyntaxKind::T_RPAREN,
        BodyForm::Bracket => SyntaxKind::T_RBRACKET,
    };
    let mut has_element = body == BodyForm::Record;
    let end = parse_balanced_block(tokens, src, cursor, close, &mut has_element)?;
    if body == BodyForm::Bracket && !has_element {
        return Err(Box::new(custom_error(
            &current_span(tokens, src, *cursor),
            D_REL_005,
        )));
    }
    skip_trivia(tokens, cursor);
    Ok(end)
}

fn parse_relation_suffix(
    tokens: &[(SyntaxKind, Span)],
    src: &str,
    mut cursor: usize,
    role: Option<RelationRole>,
    body: BodyForm,
    body_end: usize,
) -> ScanResult<ParsedRelation> {
    let mut trailing_end = skip_trivia_end(tokens, &mut cursor, body_end);

    if starts_bracket_wrapped_primary_key(tokens, src, cursor) {
        let mut has_content = true;
        let recovery_end = parse_balanced_block(
            tokens,
            src,
            &mut cursor,
            SyntaxKind::T_RBRACKET,
            &mut has_content,
        )?;
        return Ok(recoverable_relation(body_end, recovery_end, D_REL_008));
    }

    if !starts_primary_key(tokens, src, cursor) {
        return Ok(ParsedRelation {
            end: trailing_end,
            recovery_end: trailing_end,
            error: None,
        });
    }

    if body == BodyForm::Bracket {
        let recovery_end = parse_primary_key_clause(tokens, src, &mut cursor)?;
        return Ok(recoverable_relation(body_end, recovery_end, D_REL_004));
    }

    if role != Some(RelationRole::Input) {
        let recovery_end = parse_primary_key_clause(tokens, src, &mut cursor)?;
        return Ok(recoverable_relation(body_end, recovery_end, D_REL_006));
    }

    let mut end = parse_primary_key_clause(tokens, src, &mut cursor)?;
    trailing_end = skip_trivia_end(tokens, &mut cursor, end);
    end = trailing_end;
    Ok(ParsedRelation {
        end,
        recovery_end: end,
        error: None,
    })
}

fn parse_primary_key_clause(
    tokens: &[(SyntaxKind, Span)],
    src: &str,
    cursor: &mut usize,
) -> ScanResult<usize> {
    consume_text(tokens, src, cursor, "primary")?;
    skip_trivia(tokens, cursor);
    consume_text(tokens, src, cursor, "key")?;
    skip_trivia(tokens, cursor);
    let mut has_column = false;
    let mut end = parse_balanced_block(tokens, src, cursor, SyntaxKind::T_RPAREN, &mut has_column)?;
    if !has_column {
        return Err(Box::new(custom_error(
            &current_span(tokens, src, *cursor),
            D_REL_007,
        )));
    }
    skip_inline_trivia(tokens, src, cursor);
    if next_token_is_same_line_expression(tokens, src, *cursor) {
        end = consume_opaque_primary_key_expr(tokens, src, cursor)?;
    }
    Ok(end)
}

fn consume_opaque_primary_key_expr(
    tokens: &[(SyntaxKind, Span)],
    src: &str,
    cursor: &mut usize,
) -> ScanResult<usize> {
    let mut stack = Vec::new();
    let mut end = current_span(tokens, src, *cursor).start;

    while let Some((kind, span)) = tokens.get(*cursor) {
        if stack.is_empty() && token_text(src, span).is_some_and(|text| text.contains('\n')) {
            break;
        }
        adjust_stack(*kind, &mut stack);
        end = span.end;
        *cursor += 1;
    }

    if stack.is_empty() {
        Ok(end)
    } else {
        Err(Box::new(Simple::custom(
            current_span(tokens, src, *cursor),
            D_REL_007,
        )))
    }
}

fn is_bare_rule_or_fact(
    tokens: &[(SyntaxKind, Span)],
    src: &str,
    cursor: &mut usize,
    has_preamble: bool,
) -> bool {
    if has_preamble {
        return false;
    }

    let mut next = *cursor;
    let saw_newline = skip_trivia_and_check_newline(tokens, src, &mut next);
    if tokens.get(next).is_none() || starts_primary_key(tokens, src, next) {
        *cursor = next;
        return false;
    }
    if saw_newline {
        return false;
    }

    *cursor = next;
    true
}

fn skip_trivia_and_check_newline(
    tokens: &[(SyntaxKind, Span)],
    src: &str,
    cursor: &mut usize,
) -> bool {
    let mut saw_newline = false;
    while let Some((kind, span)) = tokens.get(*cursor) {
        if !is_trivia(*kind) {
            break;
        }
        saw_newline |= token_text(src, span).is_some_and(|text| text.contains('\n'));
        *cursor += 1;
    }
    saw_newline
}

#[derive(Default)]
struct RelationScanContext {
    delimiters: Vec<SyntaxKind>,
    previous_non_trivia: Option<SyntaxKind>,
    saw_newline_since_non_trivia: bool,
}

impl RelationScanContext {
    fn is_top_level_line_start(&self) -> bool {
        self.delimiters.is_empty()
            && self.previous_non_trivia.is_none_or(|previous| {
                self.saw_newline_since_non_trivia
                    && !matches!(
                        previous,
                        SyntaxKind::K_ON
                            | SyntaxKind::T_COLON
                            | SyntaxKind::T_COMMA
                            | SyntaxKind::T_IMPLIES
                    )
            })
    }

    fn observe(&mut self, kind: SyntaxKind, span: &Span, src: &str) {
        if is_trivia(kind) {
            self.saw_newline_since_non_trivia |=
                token_text(src, span).is_some_and(|text| text.contains('\n'));
            return;
        }

        adjust_stack(kind, &mut self.delimiters);
        self.previous_non_trivia = Some(kind);
        self.saw_newline_since_non_trivia = false;
    }
}

fn is_relation_candidate(kind: SyntaxKind) -> bool {
    matches!(
        kind,
        SyntaxKind::K_INPUT
            | SyntaxKind::K_OUTPUT
            | SyntaxKind::K_RELATION
            | SyntaxKind::K_STREAM
            | SyntaxKind::K_MULTISET
            | SyntaxKind::T_AMP
            | SyntaxKind::T_IDENT
    )
}

fn observe_scanned_tokens(
    context: &mut RelationScanContext,
    tokens: &[(SyntaxKind, Span)],
    src: &str,
    observed: &mut usize,
    end: usize,
) {
    while *observed < end {
        let Some((kind, span)) = tokens.get(*observed) else {
            break;
        };
        context.observe(*kind, span, src);
        *observed += 1;
    }
}

fn starts_primary_key(tokens: &[(SyntaxKind, Span)], src: &str, cursor: usize) -> bool {
    token_text_at(tokens, src, cursor) == Some("primary")
}

fn starts_bracket_wrapped_primary_key(
    tokens: &[(SyntaxKind, Span)],
    src: &str,
    cursor: usize,
) -> bool {
    if !matches!(tokens.get(cursor), Some((SyntaxKind::T_LBRACKET, _))) {
        return false;
    }
    let mut next = cursor + 1;
    skip_trivia(tokens, &mut next);
    token_text_at(tokens, src, next) == Some("primary")
}

fn next_token_is_same_line_expression(
    tokens: &[(SyntaxKind, Span)],
    src: &str,
    cursor: usize,
) -> bool {
    matches!(
        tokens.get(cursor),
        Some((kind, span))
            if !matches!(kind, SyntaxKind::T_IMPLIES | SyntaxKind::T_DOT)
                && token_text(src, span).is_some_and(|text| !text.contains('\n'))
    )
}

fn recoverable_relation(
    span_start: usize,
    recovery_end: usize,
    message: &'static str,
) -> ParsedRelation {
    ParsedRelation {
        end: span_start,
        recovery_end,
        error: Some(Simple::custom(span_start..recovery_end, message)),
    }
}

fn custom_error(span: &Span, message: &'static str) -> Simple<SyntaxKind> {
    Simple::custom(span.clone(), message)
}
