//! Line-boundary helpers for the rule declaration scanner.
//!
//! Determines whether a token begins a new logical line so the `rules`
//! scanner only treats line-initial tokens as rule or `for` statement starts.

use crate::{Span, SyntaxKind};

use super::super::utils::State;

/// Return `true` if `span` begins a new logical line in the source.
///
/// Multiple rules on one physical line are recognised by treating a preceding
/// `.` as a line start boundary. Otherwise, only trivia containing a newline
/// counts; inline whitespace or comments keep the current line “open” so
/// continuations are not misclassified.
pub(super) fn is_at_line_start(st: &State<'_>, span: &Span) -> bool {
    if st.stream.cursor() == 0 {
        return true;
    }

    line_start_boundary(
        st.stream.tokens(),
        st.stream.src(),
        st.stream.cursor(),
        span.start,
    )
}

fn line_start_boundary(
    tokens: &[(SyntaxKind, Span)],
    src: &str,
    mut cursor: usize,
    span_start: usize,
) -> bool {
    while cursor > 0 {
        cursor -= 1;
        let Some((kind, prev_span)) = tokens.get(cursor) else {
            break;
        };
        if matches!(kind, SyntaxKind::T_WHITESPACE | SyntaxKind::T_COMMENT) {
            if range_contains_newline(src, prev_span.clone()) {
                return true;
            }
            continue;
        }

        if slice_contains_newline(src, prev_span.end, span_start) {
            return true;
        }
        if *kind == SyntaxKind::T_DOT {
            return true;
        }
        return false;
    }

    true
}

fn range_contains_newline(src: &str, range: std::ops::Range<usize>) -> bool {
    src.get(range).is_some_and(|text| text.contains('\n'))
}

fn slice_contains_newline(src: &str, start: usize, end: usize) -> bool {
    range_contains_newline(src, start..end)
}
