//! Unit tests for semantic-model construction.

use crate::parse;

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

fn uses_named<'a>(
    model: &'a super::SemanticModel,
    name: &str,
    kind: UseKind,
) -> Vec<&'a super::UseSite> {
    model
        .uses()
        .iter()
        .filter(|use_site| use_site.name() == name && use_site.kind() == kind)
        .collect()
}

fn symbols_named<'a>(
    model: &'a super::SemanticModel,
    name: &str,
    kind: DeclarationKind,
) -> Vec<&'a super::Symbol> {
    model
        .symbols()
        .iter()
        .filter(|symbol| symbol.name() == name && symbol.kind() == kind)
        .collect()
}

#[test]
fn collects_program_scope_declarations() {
    let parsed = parse_ok(concat!(
        "typedef UserId = u32\n",
        "function project(id: UserId): UserId {}\n",
        "input relation User(id: UserId)"
    ));
    let model = build(&parsed);

    assert_eq!(model.scopes().len(), 1);
    assert_eq!(
        model.scopes().first().map(crate::sema::Scope::kind),
        Some(ScopeKind::Program)
    );
    assert_eq!(
        symbols_named(&model, "UserId", DeclarationKind::Type).len(),
        1
    );
    assert_eq!(
        symbols_named(&model, "project", DeclarationKind::Function).len(),
        1
    );
    assert_eq!(
        symbols_named(&model, "User", DeclarationKind::Relation).len(),
        1
    );
}

#[test]
fn head_bindings_are_visible_from_rule_start() {
    let parsed = parse_ok("Output(x) :- Source(x).");
    let model = build(&parsed);
    let x_bindings = symbols_named(&model, "x", DeclarationKind::RuleBinding);
    let x_uses = uses_named(&model, "x", UseKind::Variable);

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

#[test]
fn assignment_binding_becomes_visible_after_its_literal() {
    let parsed = parse_ok("Output(x) :- Before(x), var x = Seed(x), After(x).");
    let model = build(&parsed);
    let x_bindings = symbols_named(&model, "x", DeclarationKind::RuleBinding);
    let maybe_after_use = uses_named(&model, "x", UseKind::Variable)
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
                model.symbol(symbol_id).origin(),
                SymbolOrigin::AssignmentPattern
            );
        }
        other => panic!("expected resolved assignment binding, got {other:?}"),
    }
}

#[test]
fn wildcard_variable_uses_are_ignored() {
    let parsed = parse_ok("Output(_) :- Source(_).");
    let model = build_from_root(parsed.root());
    let wildcard_uses = uses_named(&model, "_", UseKind::Variable);

    assert!(
        wildcard_uses
            .iter()
            .all(|use_site| use_site.resolution() == Resolution::Ignored),
        "wildcard uses should be ignored",
    );
}

#[test]
fn for_loop_bindings_do_not_escape_loop_scope() {
    let parsed = parse_ok("Output(x) :- Source(x), for (y in Items(x)) Inner(y), After(y).");
    let model = build(&parsed);
    let maybe_inner_use = uses_named(&model, "y", UseKind::Variable)
        .into_iter()
        .find(|use_site| use_site.source_order() == 1);
    let Some(inner_use) = maybe_inner_use else {
        panic!("expected inner y use");
    };
    let maybe_after_use = uses_named(&model, "y", UseKind::Variable)
        .into_iter()
        .find(|use_site| use_site.source_order() == 2);
    let Some(after_use) = maybe_after_use else {
        panic!("expected post-loop y use");
    };

    assert!(
        model
            .scopes()
            .iter()
            .any(|scope| scope.kind() == ScopeKind::ForLoop)
    );
    assert!(matches!(inner_use.resolution(), Resolution::Resolved(_)));
    assert_eq!(after_use.resolution(), Resolution::Unresolved);
}

#[test]
fn top_level_for_semantic_rules_participate_in_analysis() {
    let parsed = parse_ok("for (x in Source(x)) Output(x).");
    let model = build(&parsed);

    assert!(
        model
            .scopes()
            .iter()
            .any(|scope| scope.kind() == ScopeKind::Rule),
        "semantic rule should produce a rule scope",
    );
    assert!(
        !uses_named(&model, "Output", UseKind::Relation).is_empty(),
        "semantic rule head should record relation use",
    );
}
