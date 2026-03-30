//! Unit tests for semantic-model construction.

use crate::parse;
use rstest::{fixture, rstest};

use super::{
    DeclarationKind, Resolution, ScopeKind, SymbolOrigin, UseKind, UseOrigin, build,
    build_from_root,
};

fn parse_ok(source: &str) -> crate::Parsed {
    let parsed = parse(source);
    assert!(
        parsed.errors().is_empty(),
        "semantic test source should parse cleanly: {:?}",
        parsed.errors()
    );
    parsed
}

fn filter_named_by_kind<'a, T, K: PartialEq>(
    items: &'a [T],
    name: &str,
    kind: &K,
    name_fn: impl Fn(&T) -> &str,
    kind_fn: impl Fn(&T) -> K,
) -> Vec<&'a T> {
    items
        .iter()
        .filter(|item| name_fn(item) == name && &kind_fn(item) == kind)
        .collect()
}

fn uses_named<'a>(
    model: &'a super::SemanticModel,
    name: &str,
    kind: UseKind,
) -> Vec<&'a super::UseSite> {
    filter_named_by_kind(
        model.uses(),
        name,
        &kind,
        |u| u.name(),
        super::model::UseSite::kind,
    )
}

fn symbols_named<'a>(
    model: &'a super::SemanticModel,
    name: &str,
    kind: DeclarationKind,
) -> Vec<&'a super::Symbol> {
    filter_named_by_kind(
        model.symbols(),
        name,
        &kind,
        |s| s.name(),
        super::model::Symbol::kind,
    )
}

fn relation_symbol_id(model: &super::SemanticModel, name: &str) -> Option<crate::sema::SymbolId> {
    model
        .relation_symbols()
        .find_map(|(symbol_id, symbol)| (symbol.name() == name).then_some(symbol_id))
}

#[fixture]
fn parsed_case(#[default("")] source: &str) -> crate::Parsed {
    parse_ok(source)
}

#[fixture]
fn semantic_model(#[default("")] source: &str) -> super::SemanticModel {
    let parsed = parse_ok(source);
    build(&parsed)
}

#[rstest]
#[case("UserId", DeclarationKind::Type)]
#[case("project", DeclarationKind::Function)]
#[case("User", DeclarationKind::Relation)]
fn collects_program_scope_declarations(
    #[with(concat!(
        "typedef UserId = u32\n",
        "function project(id: UserId): UserId {}\n",
        "input relation User(id: UserId)"
    ))]
    semantic_model: super::SemanticModel,
    #[case] symbol_name: &str,
    #[case] symbol_kind: DeclarationKind,
) {
    let scopes = semantic_model.scopes();

    assert_eq!(scopes.len(), 1);
    assert_eq!(
        scopes.first().map(crate::sema::Scope::kind),
        Some(ScopeKind::Program)
    );
    assert_eq!(
        symbols_named(&semantic_model, symbol_name, symbol_kind).len(),
        1
    );
}

#[rstest]
fn head_bindings_are_visible_from_rule_start(
    #[with("Output(x) :- Source(x).")] semantic_model: super::SemanticModel,
) {
    let x_bindings = symbols_named(&semantic_model, "x", DeclarationKind::RuleBinding);
    let x_uses = uses_named(&semantic_model, "x", UseKind::Variable);

    assert_eq!(x_bindings.len(), 1);
    assert_eq!(
        x_bindings.first().map(|binding| binding.origin()),
        Some(SymbolOrigin::RuleHead)
    );
    assert_eq!(
        x_bindings
            .first()
            .map(|binding| binding.visible_from_rule_order()),
        Some(0)
    );
    assert!(
        x_uses
            .iter()
            .any(|use_site| matches!(use_site.resolution(), Resolution::Resolved(_))),
        "body use of x should resolve to the head binding",
    );
}

#[rstest]
fn relation_use_origins_distinguish_heads_from_reads(
    #[with(concat!(
        "Sink(x) :- ",
        "Source(x), ",
        "for (y in Items(x)) Check(y), ",
        "for (z in Items(x) if Check(z)) Check(z)."
    ))]
    semantic_model: super::SemanticModel,
) {
    let sink_uses = uses_named(&semantic_model, "Sink", UseKind::Relation);
    let source_uses = uses_named(&semantic_model, "Source", UseKind::Relation);
    let items_uses = uses_named(&semantic_model, "Items", UseKind::Relation);
    let check_uses = uses_named(&semantic_model, "Check", UseKind::Relation);

    let sink_origins: Vec<_> = sink_uses.iter().map(|use_site| use_site.origin()).collect();
    let source_origins: Vec<_> = source_uses
        .iter()
        .map(|use_site| use_site.origin())
        .collect();
    let items_origins: Vec<_> = items_uses
        .iter()
        .map(|use_site| use_site.origin())
        .collect();
    let check_origins: Vec<_> = check_uses
        .iter()
        .map(|use_site| use_site.origin())
        .collect();

    // `Sink` only appears in rule heads.
    assert!(!sink_origins.is_empty(), "expected at least one `Sink` use");
    assert!(
        sink_origins
            .iter()
            .all(|origin| matches!(origin, UseOrigin::RelationHead)),
        "expected all `Sink` uses to be rule heads, got: {sink_origins:?}",
    );

    // `Source` only appears as a body read.
    assert!(
        !source_origins.is_empty(),
        "expected at least one `Source` use"
    );
    assert!(
        source_origins
            .iter()
            .all(|origin| matches!(origin, UseOrigin::RelationBody)),
        "expected all `Source` uses to be body reads, got: {source_origins:?}",
    );

    // `Items` only appears as the iterable of a `for`.
    assert!(
        !items_origins.is_empty(),
        "expected at least one `Items` use"
    );
    assert!(
        items_origins
            .iter()
            .all(|origin| matches!(origin, UseOrigin::ForIterable)),
        "expected all `Items` uses to be `for` iterables, got: {items_origins:?}",
    );

    // `Check` appears both in the `for` body and in the `for` guard.
    assert!(
        check_origins
            .iter()
            .any(|origin| matches!(origin, UseOrigin::RelationBody)),
        "expected at least one body use for `Check`, got: {check_origins:?}",
    );
    assert!(
        check_origins
            .iter()
            .any(|origin| matches!(origin, UseOrigin::ForGuard)),
        "expected at least one guard use for `Check`, got: {check_origins:?}",
    );
}

#[rstest]
#[expect(
    clippy::expect_used,
    reason = "tests should fail with concise lookup messages"
)]
fn assignment_binding_becomes_visible_after_its_literal(
    #[with("Output(x) :- Before(x), var x = Seed(x), After(x).")]
    semantic_model: super::SemanticModel,
) {
    let x_bindings = symbols_named(&semantic_model, "x", DeclarationKind::RuleBinding);
    let maybe_after_use = uses_named(&semantic_model, "x", UseKind::Variable)
        .into_iter()
        .find(|use_site| use_site.source_order() == 2);
    let after_use = maybe_after_use.expect("expected use in the third literal");

    assert_eq!(x_bindings.len(), 2);
    assert!(
        x_bindings
            .iter()
            .any(|binding| binding.origin() == SymbolOrigin::AssignmentPattern),
        "assignment should introduce a new binding",
    );
    let resolved_symbol = semantic_model
        .resolved_symbol(after_use)
        .expect("expected resolved assignment binding");
    assert_eq!(resolved_symbol.origin(), SymbolOrigin::AssignmentPattern);
}

#[rstest]
fn wildcard_variable_uses_are_ignored(
    #[with("Output(_) :- Source(_).")] parsed_case: crate::Parsed,
) {
    let semantic_model = build_from_root(parsed_case.root());
    let wildcard_uses = uses_named(&semantic_model, "_", UseKind::Variable);

    assert!(
        wildcard_uses
            .iter()
            .all(|use_site| use_site.resolution() == Resolution::Ignored),
        "wildcard uses should be ignored",
    );
}

#[rstest]
#[expect(
    clippy::expect_used,
    reason = "tests should fail with concise lookup messages"
)]
fn for_loop_bindings_do_not_escape_loop_scope(
    #[with("Output(x) :- Source(x), for (y in Items(x)) Inner(y), After(y).")]
    semantic_model: super::SemanticModel,
) {
    let maybe_inner_use = uses_named(&semantic_model, "y", UseKind::Variable)
        .into_iter()
        .find(|use_site| use_site.source_order() == 1);
    let inner_use = maybe_inner_use.expect("expected inner y use");
    let maybe_after_use = uses_named(&semantic_model, "y", UseKind::Variable)
        .into_iter()
        .find(|use_site| use_site.source_order() == 2);
    let after_use = maybe_after_use.expect("expected post-loop y use");

    assert!(
        semantic_model
            .scopes()
            .iter()
            .any(|scope| scope.kind() == ScopeKind::ForLoop)
    );
    assert!(matches!(inner_use.resolution(), Resolution::Resolved(_)));
    assert_eq!(after_use.resolution(), Resolution::Unresolved);
}

#[rstest]
fn top_level_for_semantic_rules_participate_in_analysis(
    #[with("for (x in Source(x)) Output(0).")] semantic_model: super::SemanticModel,
) {
    let output_uses = uses_named(&semantic_model, "Output", UseKind::Relation);
    let source_uses = uses_named(&semantic_model, "Source", UseKind::Relation);
    let x_uses = uses_named(&semantic_model, "x", UseKind::Variable);

    assert!(
        semantic_model
            .scopes()
            .iter()
            .any(|scope| scope.kind() == ScopeKind::Rule),
        "semantic rule should produce a rule scope",
    );
    assert!(
        !output_uses.is_empty(),
        "semantic rule head should record relation use",
    );
    assert_eq!(
        output_uses.first().map(|use_site| use_site.origin()),
        Some(UseOrigin::RelationHead)
    );
    assert_eq!(
        source_uses
            .first()
            .map(|use_site| use_site.origin().is_relation_read()),
        Some(true)
    );
    assert!(
        x_uses
            .iter()
            .all(|use_site| matches!(use_site.resolution(), Resolution::Resolved(_))),
        "top-level `for` bindings should participate in semantic resolution",
    );
}

#[rstest]
#[expect(
    clippy::expect_used,
    reason = "tests should fail with concise lookup messages"
)]
fn relation_use_prefers_relation_over_function_with_same_name(
    #[with(concat!(
        "input relation Foo(x: u32)\n",
        "function Foo(x: u32): u32 { x }\n",
        "relation Bar(x: u32)\n",
        "Bar(x) :- Foo(x).\n",
    ))]
    semantic_model: super::SemanticModel,
) {
    let foo_relation = symbols_named(&semantic_model, "Foo", DeclarationKind::Relation)
        .into_iter()
        .next()
        .expect("expected relation declaration named Foo");
    let foo_functions = symbols_named(&semantic_model, "Foo", DeclarationKind::Function);
    let foo_relation_uses = uses_named(&semantic_model, "Foo", UseKind::Relation);

    assert_eq!(foo_functions.len(), 1, "expected exactly one function Foo");
    assert!(
        !foo_relation_uses.is_empty(),
        "expected at least one relation-position use of Foo"
    );

    for use_site in foo_relation_uses {
        let resolved_symbol = semantic_model
            .resolved_symbol(use_site)
            .expect("relation-position use of Foo should resolve");
        assert_eq!(resolved_symbol.kind(), DeclarationKind::Relation);
        assert_eq!(resolved_symbol, foo_relation);
    }
}

#[rstest]
#[expect(
    clippy::expect_used,
    reason = "tests should fail with concise lookup messages"
)]
fn relation_read_helpers_ignore_head_only_and_unresolved_uses(
    #[with(concat!(
        "input relation Source(x: u32)\n",
        "relation Sink(x: u32)\n",
        "relation HeadOnly(x: u32)\n",
        "relation NeverRead(x: u32)\n",
        "Sink(x) :- Source(x), Missing(x).\n",
        "HeadOnly(x) :- Source(x).\n",
    ))]
    semantic_model: super::SemanticModel,
) {
    let source_id = relation_symbol_id(&semantic_model, "Source").expect("missing Source");
    let sink_id = relation_symbol_id(&semantic_model, "Sink").expect("missing Sink");
    let head_only_id = relation_symbol_id(&semantic_model, "HeadOnly").expect("missing HeadOnly");
    let never_read_id =
        relation_symbol_id(&semantic_model, "NeverRead").expect("missing NeverRead");

    assert!(semantic_model.has_resolved_relation_read(source_id));
    assert!(!semantic_model.has_resolved_relation_read(sink_id));
    assert!(!semantic_model.has_resolved_relation_read(head_only_id));
    assert!(!semantic_model.has_resolved_relation_read(never_read_id));

    let source_span = semantic_model
        .symbol(source_id)
        .expect("missing Source symbol")
        .span()
        .clone();
    assert_eq!(
        semantic_model.relation_symbol_at_span(&source_span),
        Some(source_id)
    );
}

mod unused_variable;
