//! Regression tests for the single-keyword declaration scanners.
//!
//! These pin the observable behaviour of `collect_import_spans` and
//! `collect_apply_spans` before the shared collector helper is introduced:
//! which spans survive, where they start and end, and whether a malformed
//! declaration reports a diagnostic. Both scanners recover line by line, so a
//! malformed declaration must not swallow the declaration that follows it.

use chumsky::error::Simple;
use rstest::rstest;

use super::super::{collect_apply_spans, collect_import_spans};
use crate::{Span, SyntaxKind, test_util::tokenize};

/// Expected outcome for one scanner run.
struct ScanCase<'a> {
    /// Source text handed to the scanner.
    src: &'a str,
    /// Spans the scanner must report, in source order.
    expected_spans: &'a [Span],
    /// Whether the scanner must report at least one diagnostic.
    expects_errors: bool,
}

impl<'a> ScanCase<'a> {
    const fn new(src: &'a str, expected_spans: &'a [Span], expects_errors: bool) -> Self {
        Self {
            src,
            expected_spans,
            expects_errors,
        }
    }
}

/// Wrap one expected span without writing a one-element array literal.
const fn single_span(span: Span) -> [Span; 1] { [span] }

/// Assert the collected spans and whether any diagnostic was reported.
#[track_caller]
fn assert_collection(case: &ScanCase<'_>, spans: &[Span], errors: &[Simple<SyntaxKind>]) {
    assert_eq!(
        spans, case.expected_spans,
        "span mismatch for {:?}: collected {spans:?}",
        case.src
    );
    assert_eq!(
        errors.is_empty(),
        !case.expects_errors,
        "diagnostic presence mismatch for {:?}: {errors:?}",
        case.src
    );
}

#[rstest]
#[case("", vec![], false)]
#[case("import foo\n", vec![0..11], false)]
#[case("import foo::bar\n", vec![0..16], false)]
#[case("import foo as f\n", vec![0..16], false)]
#[case("import foo", vec![0..10], false)]
#[case("import foo\nimport bar\n", vec![0..11, 11..22], false)]
#[case("import foo import bar\n", vec![0..11, 11..22], false)]
fn collect_import_spans_preserves_span_ranges(
    #[case] src: &str,
    #[case] expected: Vec<Span>,
    #[case] expects_errors: bool,
) {
    let tokens = tokenize(src);
    let (spans, errors) = collect_import_spans(&tokens, src);
    let case = ScanCase::new(src, &expected, expects_errors);
    assert_collection(&case, &spans, &errors);
}

#[test]
fn collect_import_spans_reports_malformed_head_and_recovers() {
    let src = "import\nimport bar\n";
    let expected = single_span(7..18);
    let tokens = tokenize(src);
    let (spans, errors) = collect_import_spans(&tokens, src);
    let case = ScanCase::new(src, &expected, true);
    assert_collection(&case, &spans, &errors);
}

#[rstest]
#[case("apply empty() -> ()\n", vec![0..20], false)]
#[case("apply normalise(User) -> (Normalised)\n", vec![0..38], false)]
#[case("apply normalise(User) -> (Normalised)", vec![0..37], false)]
#[case("apply a(x) -> (y)   \n", vec![0..21], false)]
#[case(
    "apply a(x) -> (y)\napply b(p) -> (q)\n",
    vec![0..18, 18..36],
    false
)]
#[case(
    "apply a(x) -> (y) apply b(p) -> (q)\n",
    vec![0..18, 18..36],
    false
)]
fn collect_apply_spans_preserves_span_ranges(
    #[case] src: &str,
    #[case] expected: Vec<Span>,
    #[case] expects_errors: bool,
) {
    let tokens = tokenize(src);
    let (spans, errors) = collect_apply_spans(&tokens, src);
    let case = ScanCase::new(src, &expected, expects_errors);
    assert_collection(&case, &spans, &errors);
}

#[rstest]
#[case("apply t(x) (y)\n")]
#[case("apply t(x) -> y\n")]
#[case("apply t x -> (y)\n")]
fn collect_apply_spans_rejects_malformed_declarations(#[case] src: &str) {
    let tokens = tokenize(src);
    let (spans, errors) = collect_apply_spans(&tokens, src);
    let case = ScanCase::new(src, &[], true);
    assert_collection(&case, &spans, &errors);
}

#[test]
fn collect_apply_spans_recovers_after_malformed_declaration() {
    let src = "apply t(x) (y)\napply a(x) -> (y)\n";
    let expected = single_span(15..33);
    let tokens = tokenize(src);
    let (spans, errors) = collect_apply_spans(&tokens, src);
    let case = ScanCase::new(src, &expected, true);
    assert_collection(&case, &spans, &errors);
}

#[test]
fn collect_apply_spans_accepts_empty_lists_without_trailing_newline() {
    let src = "apply empty() -> ()";
    let expected = single_span(0..19);
    let tokens = tokenize(src);
    let (spans, errors) = collect_apply_spans(&tokens, src);
    let case = ScanCase::new(src, &expected, false);
    assert_collection(&case, &spans, &errors);
}
