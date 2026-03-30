//! Semantic helper tests for `unused-variable` support.

use rstest::rstest;

use super::{DeclarationKind, build, parse_ok, symbols_named};
use crate::sema::SymbolId;

fn rule_binding_symbol_id(model: &super::super::SemanticModel, name: &str) -> SymbolId {
    let bindings = symbols_named(model, name, DeclarationKind::RuleBinding);
    assert_eq!(
        bindings.len(),
        1,
        "expected exactly one rule binding named `{name}`",
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
