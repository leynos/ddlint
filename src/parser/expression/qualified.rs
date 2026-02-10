//! Qualification helpers for distinguishing calls from unresolved applications.
//!
//! These predicates enforce the parser-side rule that only fully qualified
//! names with a lowercase terminal segment are parsed as [`Expr::Call`].

use crate::parser::ast::Expr;

pub(super) fn is_qualified_function_callee(callee: &Expr) -> bool {
    match callee {
        Expr::Variable(name) => is_qualified_function_name(name),
        _ => false,
    }
}

fn is_qualified_function_name(name: &str) -> bool {
    let mut count = 0usize;
    let mut final_segment = None;
    for segment in name.split("::") {
        if !is_identifier_segment(segment) {
            return false;
        }
        count += 1;
        final_segment = Some(segment);
    }

    let Some(last) = final_segment else {
        return false;
    };

    count >= 2 && is_lowercase_identifier_segment(last)
}

fn is_identifier_segment(segment: &str) -> bool {
    let mut chars = segment.chars();
    let Some(first) = chars.next() else {
        return false;
    };
    (first == '_' || first.is_ascii_alphabetic())
        && chars.all(|ch| ch == '_' || ch.is_ascii_alphanumeric())
}

fn is_lowercase_identifier_segment(segment: &str) -> bool {
    let mut chars = segment.chars();
    let Some(first) = chars.next() else {
        return false;
    };
    (first == '_' || first.is_ascii_lowercase())
        && chars.all(|ch| ch == '_' || ch.is_ascii_alphanumeric())
}
