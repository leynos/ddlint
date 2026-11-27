//! Human-readable rendering of final parser diagnostics.
//!
//! The observer and the command-line front end both present the same message,
//! so the rendering lives beside the stable diagnostic identifiers rather than
//! in either consumer.

use chumsky::error::{Simple, SimpleReason};

use crate::{SyntaxKind, parser::lexer_helpers::token_display};

/// Render a final parse error as a human-readable message.
///
/// Custom diagnostics keep the text the parser supplied. Structural failures
/// are described from the token kinds Chumsky recorded, with punctuation
/// quoted and internal kinds named in words. The span is not part of the
/// message: callers report the error's span separately.
///
/// # Examples
///
/// ```rust
/// use ddlint::{parse, parse_error_message};
///
/// let parsed = parse("R(x) :- return x.");
/// let error = parsed
///     .errors()
///     .first()
///     .expect("the bare return is rejected");
/// assert_eq!(
///     parse_error_message(error),
///     "'return' is not permitted in a rule body; use it only in a function or closure body",
/// );
/// ```
#[must_use]
pub fn parse_error_message(error: &Simple<SyntaxKind>) -> String {
    match error.reason() {
        SimpleReason::Custom(message) => message.clone(),
        SimpleReason::Unexpected => expected_found_message(error),
        SimpleReason::Unclosed { delimiter, .. } => {
            format!("unclosed {}", display_kind(*delimiter))
        }
    }
}

/// Describe an unexpected token, listing the sorted alternatives when known.
fn expected_found_message(error: &Simple<SyntaxKind>) -> String {
    let found = error
        .found()
        .map_or_else(|| "end of input".to_owned(), |kind| display_kind(*kind));
    let mut expected: Vec<String> = error
        .expected()
        .map(|kind| {
            kind.as_ref().map_or_else(
                || "end of input".to_owned(),
                |expected_kind| display_kind(*expected_kind),
            )
        })
        .collect();
    expected.sort();

    if expected.is_empty() {
        format!("unexpected {found}")
    } else {
        format!("expected {} but found {found}", expected.join(" or "))
    }
}

/// Render a token kind for a diagnostic message.
fn display_kind(kind: SyntaxKind) -> String {
    match token_display(kind) {
        "" => format!("{kind:?}"),
        symbol => format!("`{symbol}`"),
    }
}

/// Return the half-open byte range a final parse error covers.
///
/// Front ends report this alongside [`parse_error_message`]. Keeping the
/// accessor here means a consumer needs no direct `chumsky` dependency to
/// render a located diagnostic.
///
/// # Examples
///
/// ```rust
/// use ddlint::{parse, parse_error_span};
///
/// let parsed = parse("R(x) :- return x.");
/// let error = parsed
///     .errors()
///     .first()
///     .expect("the bare return is rejected");
/// assert_eq!(parse_error_span(error), 8..14);
/// ```
#[must_use]
pub fn parse_error_span(error: &Simple<SyntaxKind>) -> crate::Span { error.span().clone() }
