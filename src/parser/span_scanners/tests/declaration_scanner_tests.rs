//! Regression tests for the single-keyword declaration scanners.
//!
//! These pin the observable behaviour of `collect_import_spans` and
//! `collect_apply_spans` before the shared collector helper is introduced:
//! which spans survive, where they start and end, and whether a malformed
//! declaration reports a diagnostic. Both scanners recover line by line, so a
//! malformed declaration must not swallow the declaration that follows it.

use chumsky::error::{Simple, SimpleReason};
use rstest::rstest;

use super::super::{collect_apply_spans, collect_import_spans};
use crate::{Span, SyntaxKind, test_util::tokenize};

/// Expected outcome for one scanner run.
struct ScanCase<'a> {
    /// Source text handed to the scanner.
    src: &'a str,
    /// Spans the scanner must report, in source order.
    expected_spans: &'a [Span],
    /// Spans of the diagnostics the scanner must report, in order.
    expected_diagnostics: &'a [Span],
}

impl<'a> ScanCase<'a> {
    const fn new(
        src: &'a str,
        expected_spans: &'a [Span],
        expected_diagnostics: &'a [Span],
    ) -> Self {
        Self {
            src,
            expected_spans,
            expected_diagnostics,
        }
    }
}

/// Wrap one expected span without writing a one-element array literal.
const fn single_span(span: Span) -> [Span; 1] { [span] }

/// Assert the collected spans and the ordered diagnostic spans.
///
/// Pinning each diagnostic's span, rather than only its presence, catches a
/// recovery path that reports the right count against the wrong tokens.
#[track_caller]
fn assert_collection(case: &ScanCase<'_>, spans: &[Span], errors: &[Simple<SyntaxKind>]) {
    assert_eq!(
        spans, case.expected_spans,
        "span mismatch for {:?}: collected {spans:?}",
        case.src
    );
    let diagnostic_spans: Vec<Span> = errors.iter().map(Simple::span).collect();
    assert_eq!(
        diagnostic_spans, case.expected_diagnostics,
        "diagnostic span mismatch for {:?}: {errors:?}",
        case.src
    );
    assert!(
        errors.iter().all(|error| matches!(
            error.reason(),
            SimpleReason::Unexpected | SimpleReason::Custom(_)
        )),
        "expected syntax diagnostics for {:?}: {errors:?}",
        case.src
    );
}

#[rstest]
#[case("", vec![], vec![])]
#[case("import foo\n", vec![0..11], vec![])]
#[case("import foo::bar\n", vec![0..16], vec![])]
#[case("import foo as f\n", vec![0..16], vec![])]
#[case("import foo", vec![0..10], vec![])]
#[case("import foo\nimport bar\n", vec![0..11, 11..22], vec![])]
#[case("import foo import bar\n", vec![0..11, 11..22], vec![])]
fn collect_import_spans_preserves_span_ranges(
    #[case] src: &str,
    #[case] expected: Vec<Span>,
    #[case] expected_diagnostics: Vec<Span>,
) {
    let tokens = tokenize(src);
    let (spans, errors) = collect_import_spans(&tokens, src);
    let case = ScanCase::new(src, &expected, &expected_diagnostics);
    assert_collection(&case, &spans, &errors);
}

#[test]
fn collect_import_spans_reports_malformed_head_and_recovers() {
    let src = "import\nimport bar\n";
    let expected = single_span(7..18);
    // The parser has consumed the keyword and its padding, so it reports the
    // next statement's keyword as the unexpected token.
    let diagnostics = single_span(7..13);
    let tokens = tokenize(src);
    let (spans, errors) = collect_import_spans(&tokens, src);
    let case = ScanCase::new(src, &expected, &diagnostics);
    assert_collection(&case, &spans, &errors);
}

#[rstest]
#[case("apply empty() -> ()\n", vec![0..20], vec![])]
#[case("apply normalise(User) -> (Normalised)\n", vec![0..38], vec![])]
#[case("apply normalise(User) -> (Normalised)", vec![0..37], vec![])]
#[case("apply a(x) -> (y)   \n", vec![0..21], vec![])]
#[case(
    "apply a(x) -> (y)\napply b(p) -> (q)\n",
    vec![0..18, 18..36],
    vec![]
)]
#[case(
    "apply a(x) -> (y) apply b(p) -> (q)\n",
    vec![0..18, 18..36],
    vec![]
)]
fn collect_apply_spans_preserves_span_ranges(
    #[case] src: &str,
    #[case] expected: Vec<Span>,
    #[case] expected_diagnostics: Vec<Span>,
) {
    let tokens = tokenize(src);
    let (spans, errors) = collect_apply_spans(&tokens, src);
    let case = ScanCase::new(src, &expected, &expected_diagnostics);
    assert_collection(&case, &spans, &errors);
}

#[rstest]
#[case("apply t(x) (y)\n", 11..12)]
#[case("apply t(x) -> y\n", 14..15)]
#[case("apply t x -> (y)\n", 8..9)]
fn collect_apply_spans_rejects_malformed_declarations(
    #[case] src: &str,
    #[case] diagnostic_span: Span,
) {
    let diagnostics = single_span(diagnostic_span);
    let tokens = tokenize(src);
    let (spans, errors) = collect_apply_spans(&tokens, src);
    let case = ScanCase::new(src, &[], &diagnostics);
    assert_collection(&case, &spans, &errors);
}

#[test]
fn collect_apply_spans_recovers_after_malformed_declaration() {
    let src = "apply t(x) (y)\napply a(x) -> (y)\n";
    let expected = single_span(15..33);
    let diagnostics = single_span(11..12);
    let tokens = tokenize(src);
    let (spans, errors) = collect_apply_spans(&tokens, src);
    let case = ScanCase::new(src, &expected, &diagnostics);
    assert_collection(&case, &spans, &errors);
}

#[test]
fn collect_apply_spans_accepts_empty_lists_without_trailing_newline() {
    let src = "apply empty() -> ()";
    let expected = single_span(0..19);
    let tokens = tokenize(src);
    let (spans, errors) = collect_apply_spans(&tokens, src);
    let case = ScanCase::new(src, &expected, &[]);
    assert_collection(&case, &spans, &errors);
}
