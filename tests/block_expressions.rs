//! End-to-end coverage for block expressions and collection literals.
//!
//! One program exercises the public parser, the semantic model, and the
//! shipped `unused-relation` lint rule across the boundaries that consume the
//! new AST shape. A second source pins the top-level `for` diagnostic that
//! still treats a braced body as an unsupported head until roadmap 3.3.5.

use ddlint::{
    linter::{
        CstRuleStore,
        LintDiagnostic,
        RuleConfig,
        Runner,
        rules::correctness::UnusedRelationRule,
    },
    parse,
    parser::ast::{Expr, RuleAssignment, RuleBodyTerm},
    sema::{self, SymbolOrigin, UseKind},
    test_util::{ErrorPattern, find_matching_error},
};
use rstest::{fixture, rstest};

/// Program whose braced `if` branches, statement-bearing block, and bracket
/// map literal cross every boundary under test.
const PROGRAM: &str = concat!(
    "input relation Items(x: u32)\n",
    "input relation Checked(x: u32)\n",
    "input relation Wrapped(x: u32)\n",
    "input relation Witness(x: u32)\n",
    "output relation Out(x: u32)\n",
    "Out(y) :- Items(x), var y = if x > 1 { x } else { 0 }, ",
    "var s = { x; x + 1 }, Checked(y), var m = [x -> 1], { Wrapped(x) }.\n",
);

/// Diagnostic substring for a top-level `for` whose body is not atom-like.
const UNSUPPORTED_FOR_HEAD: &str = "top-level `for` body must end in an atom-like expression";

/// Parse [`PROGRAM`], returning its diagnostics when it does not parse.
///
/// The fixture is fallible rather than panicking so each test reports the
/// diagnostics of the program it depends on at the point of use.
#[fixture]
fn parsed_program() -> Result<ddlint::Parsed, String> {
    let parsed = parse(PROGRAM);
    let errors = parsed.errors();
    if errors.is_empty() {
        Ok(parsed)
    } else {
        Err(format!("block program should parse cleanly: {errors:?}"))
    }
}

/// Report a failed expectation from a test that returns `Result`.
///
/// These tests consume a fallible fixture, so they return `Result` instead of
/// asserting; `ensure` keeps the failure message while propagating it as an
/// error rather than a panic.
fn ensure(condition: bool, message: impl FnOnce() -> String) -> Result<(), String> {
    if condition { Ok(()) } else { Err(message()) }
}

/// Run the shipped `unused-relation` rule over `PROGRAM`.
fn unused_relation_diagnostics(parsed: &ddlint::Parsed) -> Vec<LintDiagnostic> {
    let mut store = CstRuleStore::new();
    store.register(Box::new(UnusedRelationRule));
    Runner::new(&store, PROGRAM, parsed, RuleConfig::new()).run()
}

#[rstest]
fn block_program_parses_without_errors(
    #[from(parsed_program)] parsed_res: Result<ddlint::Parsed, String>,
) -> Result<(), String> {
    let parsed = parsed_res?;
    ensure(parsed.root().rules().len() == 1, || {
        let found = parsed.root().rules().len();
        format!("the program declares one rule, found {found}")
    })
}

#[rstest]
fn semantic_model_records_each_braced_branch_use_of_x(
    #[from(parsed_program)] parsed_res: Result<ddlint::Parsed, String>,
) -> Result<(), String> {
    let parsed = parsed_res?;
    let model = sema::build(&parsed);
    let x_texts: Vec<&str> = model
        .uses()
        .iter()
        .filter(|use_site| use_site.name() == "x" && use_site.kind() == UseKind::Variable)
        .map(|use_site| {
            let span = use_site.span().clone();
            PROGRAM
                .get(span)
                .expect("a recorded use span must be inside the program")
        })
        .collect();

    // The atom argument, the condition, both block values, the statement and
    // value of `{ x; x + 1 }`, and the map key each record a use. Uses inside
    // one term share its span, so the repeated text is what proves the block
    // items were walked: visiting only `value` would drop the statement.
    ensure(
        x_texts
            == [
                "Items(x)",
                "var y = if x > 1 { x } else { 0 }",
                "var y = if x > 1 { x } else { 0 }",
                "var s = { x; x + 1 }",
                "var s = { x; x + 1 }",
                "var m = [x -> 1]",
                "{ Wrapped(x) }",
            ],
        || format!("each block item and the bracket map key must contribute uses, got {x_texts:?}"),
    )?;

    // Identify the use by its source text: several `y` uses could share the
    // assignment's term span, and a name-only check cannot tell them apart.
    let y_use = model
        .uses()
        .iter()
        .find(|use_site| {
            use_site.name() == "y"
                && use_site.kind() == UseKind::Variable
                && PROGRAM
                    .get(use_site.span().clone())
                    .is_some_and(|text| text == "Checked(y)")
        })
        .expect("`Checked(y)` should record a use of `y`");
    let y_symbol = model
        .resolved_symbol(y_use)
        .expect("the `y` binding should resolve");
    ensure(y_symbol.name() == "y", || {
        let name = y_symbol.name();
        format!("the recorded use should resolve to a `y` binding, got `{name}`")
    })?;
    // Interim rule 7: a top-level assignment declares its pattern for later
    // clauses, so `var y = ...` shadows the head binding until roadmap 3.3.5.
    ensure(y_symbol.origin() == SymbolOrigin::AssignmentPattern, || {
        let origin = y_symbol.origin();
        format!(
            "`Checked(y)` must resolve to the assignment binding that shadows the head, got \
             {origin:?}"
        )
    })
}

#[rstest]
fn map_literal_survives_as_a_rule_body_assignment(
    #[from(parsed_program)] parsed_res: Result<ddlint::Parsed, String>,
) -> Result<(), String> {
    let parsed = parsed_res?;
    let rule = parsed
        .root()
        .rules()
        .into_iter()
        .next()
        .expect("the program declares one rule");
    let terms = rule.body_terms().expect("rule body should parse");

    let values: Vec<&Expr> = terms
        .iter()
        .filter_map(|term| match term {
            RuleBodyTerm::Assignment(RuleAssignment { value, .. }) => Some(value),
            _ => None,
        })
        .collect();

    let if_value = values
        .iter()
        .find(|value| matches!(value, Expr::IfElse { .. }))
        .expect("an `if` assignment should remain in the body");
    let Expr::IfElse { then_branch, .. } = if_value else {
        return Err(format!("expected an `if` expression, got {if_value:?}"));
    };
    let Expr::Block(block) = then_branch.as_ref() else {
        let branch = then_branch.as_ref();
        return Err(format!("the then branch must be a block, got {branch:?}"));
    };
    ensure(
        block.value.as_deref() == Some(&Expr::Variable("x".to_owned())),
        || {
            let value = block.value.as_deref();
            format!("the braced branch must stay a block whose value is `x`, got {value:?}")
        },
    )?;

    let (key, value) = values
        .iter()
        .find_map(|value| match value {
            Expr::MapLit(entries) if entries.len() == 1 => entries.first(),
            _ => None,
        })
        .expect("`[x -> 1]` must be preserved as a one-entry map literal");
    ensure(key.to_sexpr() == "x", || {
        let rendered = key.to_sexpr();
        format!("the map key must stay `x`, got {rendered}")
    })?;
    ensure(value.to_sexpr() == "1", || {
        let rendered = value.to_sexpr();
        format!("the map value must stay `1`, got {rendered}")
    })
}

#[rstest]
fn unused_relation_reports_witness_and_not_checked(
    #[from(parsed_program)] parsed_res: Result<ddlint::Parsed, String>,
) -> Result<(), String> {
    let parsed = parsed_res?;
    let diagnostics = unused_relation_diagnostics(&parsed);
    let messages: Vec<_> = diagnostics.iter().map(LintDiagnostic::message).collect();

    ensure(
        messages.iter().any(|message| message.contains("`Witness`")),
        || format!("the never-read `Witness` relation must be reported, got {messages:?}"),
    )?;
    ensure(
        messages
            .iter()
            .all(|message| !message.contains("`Checked`")),
        || format!("`Checked` is read in the rule body and must not be reported, got {messages:?}"),
    )?;
    ensure(
        messages
            .iter()
            .all(|message| !message.contains("`Wrapped`")),
        || {
            format!(
                "`Wrapped` is read only through a block and must not be reported, got {messages:?}"
            )
        },
    )
}

#[test]
fn top_level_for_with_braced_body_stays_unsupported() {
    let parsed = parse("for (x in Items(x)) { Out(x) }.");
    let expected = ErrorPattern::from(UNSUPPORTED_FOR_HEAD);
    assert!(
        find_matching_error(parsed.errors(), &expected).is_some(),
        "a braced top-level `for` body must keep the unsupported-head diagnostic, got {:?}",
        parsed.errors()
    );
    assert!(
        parsed.semantic_rules().is_empty(),
        "an unsupported top-level `for` must not emit semantic rules"
    );
}
