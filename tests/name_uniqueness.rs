//! Behavioural tests for name uniqueness validation.
//!
//! Verifies that the full `ddlint::parse` pipeline rejects programs with
//! duplicate top-level definitions and permits function arity overloading.

use ddlint::parse;
use ddlint::test_util::assert_no_parse_errors;
use rstest::rstest;

#[test]
fn mixed_program_no_duplicates() {
    let src = concat!(
        "typedef A = u32\n",
        "typedef B = string\n",
        "input relation R(x: u32)\n",
        "output relation S(y: string)\n",
        "index IR on R(x)\n",
        "index IS on S(y)\n",
        "extern transformer t1(a: A): B\n",
        "import foo\n",
        "import bar\n",
        "function f(x: u32) {}\n",
        "function g(x: u32) {}\n",
    );
    let parsed = parse(src);
    assert_no_parse_errors(parsed.errors());
}

#[rstest]
#[case("typedef A = u32\ntypedef A = string", "duplicate", "A")]
#[case(
    "input relation R(x: u32)\ntypedef Z = u32\noutput relation R(y: string)\n",
    "duplicate",
    "R"
)]
#[case("index I on R(x)\nindex I on S(y)", "duplicate", "I")]
#[case(
    "extern transformer t(a: A): B\nextern transformer t(c: C): D",
    "duplicate",
    "t"
)]
#[case("import foo\nimport foo", "duplicate", "foo")]
#[case("function f(x: u32) {}\nfunction f(y: u32) {}", "duplicate", "f")]
fn duplicate_detected(#[case] src: &str, #[case] expected_word: &str, #[case] expected_name: &str) {
    let parsed = parse(src);
    let has_match = parsed.errors().iter().any(|e| {
        let msg = format!("{e:?}");
        msg.contains(expected_word) && msg.contains(expected_name)
    });
    assert!(
        has_match,
        "expected error containing '{expected_word}' and '{expected_name}' for: {src}\n  errors: {:?}",
        parsed.errors()
    );
}

#[test]
fn function_arity_overloading_permitted() {
    let src = concat!("function f(x: u32) {}\n", "function f(x: u32, y: u32) {}\n",);
    let parsed = parse(src);
    let dup_errors: Vec<_> = parsed
        .errors()
        .iter()
        .filter(|e| format!("{e:?}").contains("duplicate"))
        .collect();
    assert!(
        dup_errors.is_empty(),
        "arity overloading should be allowed: {dup_errors:?}"
    );
}
