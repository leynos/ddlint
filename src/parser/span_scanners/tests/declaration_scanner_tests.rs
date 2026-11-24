//! Regression tests for the single-keyword declaration scanners.
//!
//! These pin the observable behaviour of `collect_import_spans` and
//! `collect_apply_spans` before the shared collector helper is introduced:
//! which spans survive, where they start and end, and which diagnostics are
//! reported in which order. Both scanners recover line by line, so a malformed
//! declaration must not swallow the declaration that follows it.

use chumsky::error::{Simple, SimpleReason};
use rstest::rstest;

use super::super::{collect_apply_spans, collect_import_spans};
use crate::{Span, SyntaxKind, test_util::tokenize};

/// A scanner under test, with the collector signature both scanners share.
type SpanScanner = fn(&[(SyntaxKind, Span)], &str) -> (Vec<Span>, Vec<Simple<SyntaxKind>>);

/// Scan `src` and compare the statement spans and ordered diagnostic spans.
///
/// Pinning each diagnostic's span, rather than only its presence, catches a
/// recovery path that reports the right count against the wrong tokens.
#[track_caller]
fn assert_scanned(
    scanner: SpanScanner,
    src: &str,
    expected_spans: &[Span],
    expected_diagnostics: &[Span],
) {
    let tokens = tokenize(src);
    let (spans, errors) = scanner(&tokens, src);
    assert_eq!(
        spans, expected_spans,
        "span mismatch for {src:?}: collected {spans:?}"
    );
    let diagnostic_spans: Vec<Span> = errors.iter().map(Simple::span).collect();
    assert_eq!(
        diagnostic_spans, expected_diagnostics,
        "diagnostic span mismatch for {src:?}: {errors:?}"
    );
    assert!(
        errors.iter().all(|error| matches!(
            error.reason(),
            SimpleReason::Unexpected | SimpleReason::Custom(_)
        )),
        "expected syntax diagnostics for {src:?}: {errors:?}"
    );
}

#[rstest]
#[case(collect_import_spans, "", vec![], vec![])]
#[case(collect_import_spans, "import foo\n", vec![0..11], vec![])]
#[case(collect_import_spans, "import foo::bar\n", vec![0..16], vec![])]
#[case(collect_import_spans, "import foo as f\n", vec![0..16], vec![])]
#[case(collect_import_spans, "import foo", vec![0..10], vec![])]
#[case(
    collect_import_spans,
    "import foo\nimport bar\n",
    vec![0..11, 11..22],
    vec![]
)]
#[case(
    collect_import_spans,
    "import foo import bar\n",
    vec![0..11, 11..22],
    vec![]
)]
#[case(collect_apply_spans, "apply empty() -> ()\n", vec![0..20], vec![])]
#[case(
    collect_apply_spans,
    "apply normalise(User) -> (Normalised)\n",
    vec![0..38],
    vec![]
)]
#[case(
    collect_apply_spans,
    "apply normalise(User) -> (Normalised)",
    vec![0..37],
    vec![]
)]
#[case(collect_apply_spans, "apply a(x) -> (y)   \n", vec![0..21], vec![])]
#[case(
    collect_apply_spans,
    "apply a(x) -> (y)\napply b(p) -> (q)\n",
    vec![0..18, 18..36],
    vec![]
)]
#[case(
    collect_apply_spans,
    "apply a(x) -> (y) apply b(p) -> (q)\n",
    vec![0..18, 18..36],
    vec![]
)]
fn collect_spans_preserves_statement_ranges(
    #[case] scanner: SpanScanner,
    #[case] src: &str,
    #[case] expected_spans: Vec<Span>,
    #[case] expected_diagnostics: Vec<Span>,
) {
    assert_scanned(scanner, src, &expected_spans, &expected_diagnostics);
}

#[rstest]
// The parser has consumed the keyword and its padding, so it reports the next
// declaration keyword as the unexpected token.
#[case(
    collect_import_spans,
    "import\nimport bar\n",
    vec![7..18],
    vec![7..13]
)]
#[case(collect_apply_spans, "apply t(x) (y)\n", vec![], vec![11..12])]
#[case(collect_apply_spans, "apply t(x) -> y\n", vec![], vec![14..15])]
#[case(collect_apply_spans, "apply t x -> (y)\n", vec![], vec![8..9])]
#[case(
    collect_apply_spans,
    "apply t(x) (y)\napply a(x) -> (y)\n",
    vec![15..33],
    vec![11..12]
)]
fn collect_spans_recovers_from_malformed_declarations(
    #[case] scanner: SpanScanner,
    #[case] src: &str,
    #[case] expected_spans: Vec<Span>,
    #[case] expected_diagnostics: Vec<Span>,
) {
    assert_scanned(scanner, src, &expected_spans, &expected_diagnostics);
}
