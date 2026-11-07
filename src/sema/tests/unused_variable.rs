//! Semantic helper tests for `unused-variable` support.

use rstest::rstest;

use super::{DeclarationKind, SymbolOrigin, build, parse_ok, symbols_named};
use crate::sema::SymbolId;

fn rule_binding_symbol_id(model: &super::super::SemanticModel, name: &str) -> SymbolId {
    // Closure parameters are rule bindings too, but the unused-variable rule
    // only reports on warning-eligible bindings, so select that one.
    let bindings: Vec<_> = symbols_named(model, name, DeclarationKind::RuleBinding)
        .into_iter()
        .filter(|symbol| symbol.origin().is_warning_eligible_rule_binding())
        .collect();
    assert_eq!(
        bindings.len(),
        1,
        "expected exactly one warning-eligible rule binding named `{name}`",
    );

    let maybe_symbol_id = model
        .rule_binding_symbols()
        .find_map(|(symbol_id, symbol)| (symbol.name() == name).then_some(symbol_id));
    let Some(symbol_id) = maybe_symbol_id else {
        panic!("missing rule-binding symbol id");
    };

    symbol_id
}

#[rstest]
#[case("Output(head_x) :- Source(head_x).", "head_x", true)]
#[case("Output(head_x) :- Source(_).", "head_x", false)]
// Wrapper expressions must still record the uses of their children.
#[case("Output(head_x) :- Source((head_x)).", "head_x", true)]
#[case("Output(head_x) :- Source(-head_x).", "head_x", true)]
#[case("Output(head_x) :- Source(head_x.field).", "head_x", true)]
#[case("Output(head_x) :- Source(head_x.0).", "head_x", true)]
// A diff marker must bind to a postfix atom, so the wrapper carries a field
// access whose child is the variable.
#[case("Output(head_x) :- Source(head_x'.field).", "head_x", true)]
#[case("Output(head_x) :- Source(head_x-<5>).", "head_x", true)]
// A closure parameter shadows the rule-head binding of the same name, so the
// head binding stays unused even though the closure body mentions it.
#[case("Output(head_x) :- Source(|head_x| head_x).", "head_x", false)]
// A capture in the closure body still resolves to the enclosing binding.
#[case("Output(head_x) :- Source(|ignored| head_x).", "head_x", true)]
// `return` is only legal in function or closure bodies, never in a rule body,
// so the wrapper is exercised through a closure while `head_x` stays a rule
// binding.
#[case("Output(head_x) :- Source(|ignored| return head_x).", "head_x", true)]
#[case(
    "Output(result) :- Source(result), var assigned_x = Seed(result), Use(assigned_x).",
    "assigned_x",
    true
)]
#[case(
    "Output(result) :- Source(result), var assigned_x = Seed(result), Use(result).",
    "assigned_x",
    false
)]
#[case(
    "Output(result) :- Source(result), for (item_y in Items(result)) Inner(item_y).",
    "item_y",
    true
)]
#[case(
    "Output(result) :- Source(result), for (item_y in Items(result)) Inner(result).",
    "item_y",
    false
)]
// `item_y` is referenced after the `for` body, where the loop binding is no
// longer visible, so the use stays unresolved.
#[case(
    "Output(result) :- Source(result), for (item_y in Items(result)) Inner(result), After(item_y).",
    "item_y",
    false
)]
fn helper_reports_only_resolved_variable_uses(
    #[case] source: &str,
    #[case] binding_name: &str,
    #[case] expected_used: bool,
) {
    let parsed = parse_ok(source);
    let semantic_model = build(&parsed);
    let symbol_id = rule_binding_symbol_id(&semantic_model, binding_name);

    assert_eq!(
        semantic_model.has_resolved_variable_use(symbol_id),
        expected_used
    );
}

#[rstest]
fn wildcard_positions_do_not_create_warning_eligible_bindings() {
    let parsed = parse_ok("Output(_) :- Source(_).");
    let semantic_model = build(&parsed);

    assert!(
        semantic_model
            .rule_binding_symbols()
            .all(|(_, symbol)| symbol.name() != "_"),
        "wildcard positions should not be recorded as rule bindings",
    );
}

#[rstest]
fn synthetic_semantic_rule_head_bindings_are_not_warning_eligible() {
    let parsed = parse_ok("for (item_y in Source(item_y)) Output(item_y).");
    let semantic_model = build(&parsed);
    let item_y_binding_origins: Vec<_> = semantic_model
        .rule_binding_symbols()
        .filter(|(_, symbol)| symbol.name() == "item_y")
        .map(|(_, symbol)| symbol.origin())
        .collect();

    assert_eq!(item_y_binding_origins, vec![SymbolOrigin::ForPattern]);
}
