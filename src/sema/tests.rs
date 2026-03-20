//! Unit tests for semantic-model construction.

use crate::parse;
use rstest::{fixture, rstest};

use super::{
    DeclarationKind, Resolution, ScopeKind, SymbolOrigin, UseKind, build, build_from_root,
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
fn assignment_binding_becomes_visible_after_its_literal(
    #[with("Output(x) :- Before(x), var x = Seed(x), After(x).")]
    semantic_model: super::SemanticModel,
) {
    let x_bindings = symbols_named(&semantic_model, "x", DeclarationKind::RuleBinding);
    let maybe_after_use = uses_named(&semantic_model, "x", UseKind::Variable)
        .into_iter()
        .find(|use_site| use_site.source_order() == 2);
    let Some(after_use) = maybe_after_use else {
        panic!("expected use in the third literal");
    };

    assert_eq!(x_bindings.len(), 2);
    assert!(
        x_bindings
            .iter()
            .any(|binding| binding.origin() == SymbolOrigin::AssignmentPattern),
        "assignment should introduce a new binding",
    );
    match after_use.resolution() {
        Resolution::Resolved(symbol_id) => {
            assert_eq!(
                semantic_model.symbol(symbol_id).origin(),
                SymbolOrigin::AssignmentPattern
            );
        }
        other => panic!("expected resolved assignment binding, got {other:?}"),
    }
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
fn for_loop_bindings_do_not_escape_loop_scope(
    #[with("Output(x) :- Source(x), for (y in Items(x)) Inner(y), After(y).")]
    semantic_model: super::SemanticModel,
) {
    let maybe_inner_use = uses_named(&semantic_model, "y", UseKind::Variable)
        .into_iter()
        .find(|use_site| use_site.source_order() == 1);
    let Some(inner_use) = maybe_inner_use else {
        panic!("expected inner y use");
    };
    let maybe_after_use = uses_named(&semantic_model, "y", UseKind::Variable)
        .into_iter()
        .find(|use_site| use_site.source_order() == 2);
    let Some(after_use) = maybe_after_use else {
        panic!("expected post-loop y use");
    };

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
    let x_uses = uses_named(&semantic_model, "x", UseKind::Variable);

    assert!(
        semantic_model
            .scopes()
            .iter()
            .any(|scope| scope.kind() == ScopeKind::Rule),
        "semantic rule should produce a rule scope",
    );
    assert!(
        !uses_named(&semantic_model, "Output", UseKind::Relation).is_empty(),
        "semantic rule head should record relation use",
    );
    assert!(
        x_uses
            .iter()
            .all(|use_site| matches!(use_site.resolution(), Resolution::Resolved(_))),
        "top-level `for` bindings should participate in semantic resolution",
    );
}

#[rstest]
fn relation_use_prefers_relation_over_function_with_same_name(
    #[with(concat!(
        "input relation Foo(x: u32)\n",
        "function Foo(x: u32): u32 { x }\n",
        "relation Bar(x: u32)\n",
        "Bar(x) :- Foo(x).\n",
    ))]
    semantic_model: super::SemanticModel,
) {
    let Some(foo_relation) = symbols_named(&semantic_model, "Foo", DeclarationKind::Relation)
        .into_iter()
        .next()
    else {
        panic!("expected relation declaration named Foo");
    };
    let foo_functions = symbols_named(&semantic_model, "Foo", DeclarationKind::Function);
    let foo_relation_uses = uses_named(&semantic_model, "Foo", UseKind::Relation);

    assert_eq!(foo_functions.len(), 1, "expected exactly one function Foo");
    assert!(
        !foo_relation_uses.is_empty(),
        "expected at least one relation-position use of Foo"
    );

    for use_site in foo_relation_uses {
        let Some(resolved_symbol) = semantic_model.resolved_symbol(use_site) else {
            panic!("relation-position use of Foo should resolve");
        };
        assert_eq!(resolved_symbol.kind(), DeclarationKind::Relation);
        assert_eq!(resolved_symbol, foo_relation);
    }
}
