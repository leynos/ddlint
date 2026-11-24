//! Tests for the span-shifting helpers.
//!
//! Lazy primary-key expression parsing reports errors relative to the captured
//! text, then shifts them into source coordinates. These cases pin the shifting
//! that the `AXIOM-CHUMSKY` assumption in the verification plan relies on.

use chumsky::error::{Simple, SimpleReason};

use super::shift_errors;
use crate::SyntaxKind;

/// Shifting an unclosed-delimiter error moves its reported span and the inner
/// opening-delimiter span.
///
/// A shift that moved only the outer span would report the opening delimiter at
/// a relative offset, so the diagnostic would point at unrelated source text.
#[test]
fn shifts_the_inner_span_of_an_unclosed_delimiter() {
    let error = <Simple<SyntaxKind> as chumsky::Error<SyntaxKind>>::unclosed_delimiter(
        2..3,
        SyntaxKind::T_LPAREN,
        7..7,
        SyntaxKind::T_RPAREN,
        None,
    );

    let shifted = shift_errors(vec![error], 10);
    let shifted_error = shifted.first().expect("shifted error");

    assert_eq!(shifted_error.span(), 17..17);
    match shifted_error.reason() {
        SimpleReason::Unclosed { span, delimiter } => {
            assert_eq!(span, &(12..13));
            assert_eq!(delimiter, &SyntaxKind::T_LPAREN);
        }
        other => panic!("expected an unclosed-delimiter reason, got {other:?}"),
    }
}

/// Shifting a custom message error moves its span and preserves the message.
#[test]
fn shifts_a_custom_error_span() {
    let error = Simple::<SyntaxKind>::custom(3..5, "D-REL-007");

    let shifted = shift_errors(vec![error], 40);
    let shifted_error = shifted.first().expect("shifted error");

    assert_eq!(shifted_error.span(), 43..45);
    match shifted_error.reason() {
        SimpleReason::Custom(message) => assert_eq!(message, "D-REL-007"),
        other => panic!("expected a custom reason, got {other:?}"),
    }
}
