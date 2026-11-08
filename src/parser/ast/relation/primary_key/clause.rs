//! CST readers behind typed primary-key access.
//!
//! A spec-form key is stored in the relation CST as plain tokens: `primary`,
//! `key`, a parenthesised binder list, and an optional trailing expression.
//! These helpers locate those tokens and slice the source text that the public
//! [`super::PrimaryKey`] view exposes. They stay private so the parser keeps a
//! single, small public surface.

use rowan::{SyntaxElement, SyntaxNode};

use super::{
    super::{RelationParseErrors, body_inspection_error, inspect, render_parse_error},
    PrimaryKey,
    PrimaryKeyBinder,
    PrimaryKeyExpression,
};
use crate::{
    DdlogLanguage,
    Span,
    SyntaxKind,
    parser::{
        ast::{
            parse_utils::{extract_delimited, is_trivia, primary_key_clause},
            rule::text_range_to_span,
            skip_whitespace_and_comments,
        },
        diagnostics::DiagnosticCode,
    },
    tokenize_with_trivia,
};

/// Diagnostic reported for an uninterpretable binder list.
const D_REL_007: &str = DiagnosticCode::RelationMalformedPrimaryKey.message_text();

/// Read the primary-key clause of a record-body relation declaration.
///
/// Returns `Ok(None)` for a keyless declaration and for bracket bodies, which
/// cannot carry a key. A present-but-malformed clause yields an error, as does
/// a relation whose body cannot be inspected at all.
pub(super) fn clause_of(
    syntax: &SyntaxNode<DdlogLanguage>,
) -> Result<Option<PrimaryKey>, RelationParseErrors> {
    let inspection = inspect::inspect_body(syntax);
    body_inspection_error(&inspection)?;
    if matches!(inspection, inspect::BodyInspection::Bracket(_)) {
        return Ok(None);
    }

    let mut iter = syntax.children_with_tokens().peekable();
    skip_to_end_of_columns(&mut iter)?;
    let elements: Vec<_> = iter.collect();
    if elements.is_empty() {
        return Ok(None);
    }
    interpret_suffix(&elements)
}

/// Interpret the CST elements that follow a record body's closing `)`.
///
/// Returns `Ok(None)` when the suffix does not begin with a `primary` keyword,
/// which is the keyless case. A suffix that does begin with `primary` must
/// carry an interpretable `primary key (...)` list, otherwise an error is
/// returned. The optional expression is captured as source text without
/// deciding whether it is a well-formed expression; callers that need the
/// expression tree use [`PrimaryKeyExpression::parse`].
fn interpret_suffix(
    elements: &[SyntaxElement<DdlogLanguage>],
) -> Result<Option<PrimaryKey>, RelationParseErrors> {
    let Some(rest_start) = elements.first().map(element_start) else {
        return Ok(None);
    };
    let rest = concat_text(elements);
    let tokens = tokenize_with_trivia(&rest);
    // No `primary` keyword after the columns means there is no clause; a
    // trailing newline or comment must not be misread as a malformed one.
    if first_non_trivia_text(&tokens, &rest) != Some("primary") {
        return Ok(None);
    }

    let relative_binders = parse_binder_list(&rest)?;
    let Some((close_index, close_span)) = binder_group_close(elements) else {
        return Err(vec![D_REL_007.to_owned()]);
    };
    let expression = expression_after(elements, close_index);
    let primary_start = first_non_trivia_element(elements).map_or(rest_start, element_start);
    let span_end = expression
        .as_ref()
        .map_or(close_span.end, |expr| expr.span.end);
    let binders = relative_binders
        .into_iter()
        .map(|(name, span)| PrimaryKeyBinder {
            name,
            // Binder spans are relative to the suffix text, so they need the
            // suffix's absolute start before they can address the source.
            span: rest_start + span.start..rest_start + span.end,
        })
        .collect();

    Ok(Some(PrimaryKey {
        binders,
        span: primary_start..span_end,
        expression,
    }))
}

/// Skip over the column declaration list and stop on the closing `)`.
///
/// Returns an error when the parenthesised column list is absent or unclosed,
/// so the primary-key suffix cannot be located.
fn skip_to_end_of_columns<I>(iter: &mut std::iter::Peekable<I>) -> Result<(), RelationParseErrors>
where
    I: Iterator<Item = SyntaxElement<DdlogLanguage>>,
{
    // Consume the first parenthesised list and discard content.
    skip_whitespace_and_comments(iter);
    extract_delimited(iter, SyntaxKind::T_LPAREN, SyntaxKind::T_RPAREN)
        .map(|_| ())
        .map_err(|err| vec![err.to_string()])
}

/// Parse the `primary key (...)` prefix of the clause text.
///
/// The returned spans are relative to `rest`, the concatenated suffix text.
fn parse_binder_list(rest: &str) -> Result<Vec<(String, Span)>, RelationParseErrors> {
    use chumsky::Parser as _;

    let tokens = tokenize_with_trivia(rest);
    let stream = chumsky::Stream::from_iter(0..rest.len(), tokens.into_iter());
    let (parsed, errors) = primary_key_clause(rest).parse_recovery(stream);
    if !errors.is_empty() {
        return Err(errors.iter().map(render_parse_error).collect());
    }
    parsed.ok_or_else(|| vec![D_REL_007.to_owned()])
}

/// Locate the binder list's closing `)` and return its index and span.
///
/// The first balanced parenthesised group in the suffix is the binder list,
/// because the clause must begin with `primary key (`.
fn binder_group_close(elements: &[SyntaxElement<DdlogLanguage>]) -> Option<(usize, Span)> {
    let mut depth = 0usize;
    let mut saw_open = false;
    for (index, element) in elements.iter().enumerate() {
        match element.kind() {
            SyntaxKind::T_LPAREN => {
                depth = depth.checked_add(1)?;
                saw_open = true;
            }
            SyntaxKind::T_RPAREN => {
                depth = depth.checked_sub(1)?;
                if saw_open && depth == 0 {
                    return Some((index, text_range_to_span(element.text_range())));
                }
            }
            _ => {}
        }
    }
    None
}

/// Return the non-trivia hull of the elements after `close_index`.
///
/// Leading and trailing trivia are excluded; interior trivia is preserved.
fn expression_after(
    elements: &[SyntaxElement<DdlogLanguage>],
    close_index: usize,
) -> Option<PrimaryKeyExpression> {
    let tail = elements.get(close_index.checked_add(1)?..)?;
    let first = tail.iter().position(|element| !is_trivia(element))?;
    let last = tail.iter().rposition(|element| !is_trivia(element))?;
    let hull = tail.get(first..=last)?;
    let text = concat_text(hull);
    let start = hull.first().map(element_start)?;
    let end = hull
        .last()
        .map(|element| text_range_to_span(element.text_range()).end)?;
    Some(PrimaryKeyExpression {
        text,
        span: start..end,
    })
}

/// Concatenate the source text of `elements`.
fn concat_text(elements: &[SyntaxElement<DdlogLanguage>]) -> String {
    elements.iter().map(ToString::to_string).collect()
}

/// Return the first non-trivia element, if any.
fn first_non_trivia_element(
    elements: &[SyntaxElement<DdlogLanguage>],
) -> Option<&SyntaxElement<DdlogLanguage>> {
    elements.iter().find(|element| !is_trivia(element))
}

/// Return the source start offset for an element.
fn element_start(element: &SyntaxElement<DdlogLanguage>) -> usize {
    text_range_to_span(element.text_range()).start
}

/// Return the source text of the first non-trivia token, if any.
fn first_non_trivia_text<'a>(tokens: &[(SyntaxKind, Span)], src: &'a str) -> Option<&'a str> {
    tokens
        .iter()
        .find(|(kind, _)| !matches!(kind, SyntaxKind::T_WHITESPACE | SyntaxKind::T_COMMENT))
        .and_then(|(_, span)| src.get(span.clone()))
}
