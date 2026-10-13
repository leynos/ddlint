//! Compile-fail tests for parser API compatibility boundaries.

#[test]
fn parser_api_compatibility_contracts_are_checked_for_consumers() {
    let cases = trybuild::TestCases::new();
    cases.compile_fail("tests/ui/non_exhaustive_diagnostic_category.rs");
    cases.compile_fail("tests/ui/non_exhaustive_diagnostic_code.rs");
    cases.compile_fail("tests/ui/non_exhaustive_diagnostic_severity.rs");

    #[cfg(not(feature = "observability"))]
    cases.compile_fail("tests/ui/telemetry_observer_without_feature.rs");
}
