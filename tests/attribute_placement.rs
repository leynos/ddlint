//! Behavioural tests for attribute placement validation.
//!
//! Verifies that the full `ddlint::parse` pipeline rejects attributes on
//! items where the specification forbids them, and accepts attributes on
//! permitted targets.

use ddlint::parse;
use ddlint::test_util::assert_no_parse_errors;
use rstest::rstest;

#[rstest]
#[case("#[cold]\ntypedef T = u32")]
#[case("#[inline]\nfunction f() {}")]
#[case("#[hot]\ninput relation R(x: u32)")]
#[case("#[hot]\noutput relation R(x: u32)")]
#[case("#[hot]\nrelation R(x: u32)")]
#[case("#[hot]\nstream relation R(x: u32)")]
#[case("#[hot]\nmultiset relation R(x: u32)")]
#[case("#[cold]\nextern function f()")]
#[case("#[cold]\nextern type Handle")]
#[case("#[a]\n#[b]\ntypedef T = u32")]
fn valid_attribute_placement(#[case] src: &str) {
    let parsed = parse(src);
    assert_no_parse_errors(parsed.errors());
}

/// Spec §12: attributes are only permitted on typedef, function, and
/// relation declarations.
#[rstest]
#[case("#[cold]\nindex Ix(a: T) on A(a)")]
#[case("#[cold]\napply T(R) -> (S)")]
#[case("#[cold]\nimport foo")]
#[case("#[cold]\nextern transformer t(x: A): B")]
#[case("#[cold]\nextern typedef T = u32")]
fn invalid_attribute_placement(#[case] src: &str) {
    let parsed = parse(src);
    let has_attr_error = parsed
        .errors()
        .iter()
        .any(|e| format!("{e:?}").contains("attribute"));
    assert!(
        has_attr_error,
        "expected attribute placement error for: {src}\n  errors: {:?}",
        parsed.errors()
    );
}
