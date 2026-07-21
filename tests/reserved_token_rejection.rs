//! Behavioural tests for reserved-token rejection.
//!
//! These cases exercise the public `parse()` entrypoint so parser-policy
//! diagnostics remain visible to downstream linter and semantic-model callers.

use ddlint::linter::{CstRuleStore, RuleConfig, Runner};
use ddlint::test_util::{assert_custom_parse_error_contains, assert_no_parse_errors};
use ddlint::{SyntaxKind, parse};
use rstest::rstest;

const RESERVED_TYPEDEF_ERROR: &str = "`typedef` is a legacy DDlog keyword; use `type` instead";
const RESERVED_SPACESHIP_ERROR: &str =
    "`<=>` was reserved upstream but has no semantics in DDlog; remove it";
const RESERVED_BARE_HASH_ERROR: &str =
    "`#` is reserved; only `#[...]` attribute syntax is accepted";
const RESERVED_BIGINT_ERROR: &str =
    "`bigint` is a legacy type name; use a sized integer such as `i64` or `u64`";
const RESERVED_BIT_ERROR: &str =
    "`bit` is a legacy type name; use an unsigned sized integer such as `u32`";
const RESERVED_DOUBLE_ERROR: &str = "`double` is a legacy type name; use `f64`";
const RESERVED_FLOAT_ERROR: &str = "`float` is a legacy type name; use `f32`";
const RESERVED_SIGNED_ERROR: &str =
    "`signed` is a legacy type name; use a signed sized integer such as `i32`";

#[rstest]
#[case::typedef("typedef Foo = u32\n", RESERVED_TYPEDEF_ERROR)]
#[case::spaceship("Output(x) :- Source(x), x <=> 1.\n", RESERVED_SPACESHIP_ERROR)]
#[case::bare_hash("# foo\n", RESERVED_BARE_HASH_ERROR)]
#[case::bigint("type Foo = bigint;\n", RESERVED_BIGINT_ERROR)]
#[case::bit("type Foo = bit<32>;\n", RESERVED_BIT_ERROR)]
#[case::double("type Foo = double;\n", RESERVED_DOUBLE_ERROR)]
#[case::float("type Foo = float;\n", RESERVED_FLOAT_ERROR)]
#[case::signed("type Foo = signed<32>;\n", RESERVED_SIGNED_ERROR)]
fn reserved_tokens_emit_public_parse_errors(#[case] source: &str, #[case] expected: &str) {
    let parsed = parse(source);

    assert_custom_parse_error_contains(parsed.errors(), expected);
}

#[test]
fn import_alias_keyword_still_parses_cleanly() {
    let parsed = parse("import foo::bar as baz\n");

    assert_no_parse_errors(parsed.errors());
    let imports = parsed.root().imports();
    assert_eq!(imports.len(), 1);
    assert_eq!(
        imports.first().map(ddlint::ast::Import::path).as_deref(),
        Some("foo::bar")
    );
    assert_eq!(
        imports
            .first()
            .and_then(ddlint::ast::Import::alias)
            .as_deref(),
        Some("baz")
    );
}

#[test]
fn attribute_hash_is_preserved_and_bare_hash_is_rejected() {
    let attributed = parse("#[cold]\ntype Foo = u32\n");
    assert_no_parse_errors(attributed.errors());
    assert_eq!(
        attributed
            .root()
            .syntax()
            .descendants()
            .filter(|node| node.kind() == SyntaxKind::N_ATTRIBUTE)
            .count(),
        1
    );
    assert_eq!(attributed.root().type_defs().len(), 1);

    let bare = parse("# cold\n");
    assert_custom_parse_error_contains(bare.errors(), RESERVED_BARE_HASH_ERROR);
}

#[test]
fn typedef_rejection_reaches_linter_callers_before_rules_run() {
    let source = "typedef Foo = u32\n";
    let parsed = parse(source);

    assert!(parsed.root().type_defs().is_empty());
    assert_custom_parse_error_contains(parsed.errors(), RESERVED_TYPEDEF_ERROR);

    let store = CstRuleStore::new();
    let diagnostics = Runner::new(&store, source, &parsed, RuleConfig::new()).run();
    assert!(
        diagnostics.is_empty(),
        "empty linter store should not mask parse errors with diagnostics: {diagnostics:?}",
    );
}
