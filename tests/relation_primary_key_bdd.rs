//! Behavioural coverage for typed primary-key access.
//!
//! Each scenario drives the public `ddlint::parse` entrypoint and asserts the
//! typed accessor's observable results, keeping the feature text as the
//! specification of expected behaviour.

use std::cell::{Ref, RefCell};

use chumsky::error::SimpleReason;
use ddlint::{
    Parsed,
    ast::{PrimaryKey, PrimaryKeyExpression, RelationParseErrors},
    parse,
};
use fixture_lints::allow_fixture_expansion_lints;
use rstest::fixture;
use rstest_bdd::StepResult;
use rstest_bdd_macros::{given, scenario, then, when};

/// Message used when a step expected an exposed primary-key clause.
const NO_KEY: &str = "no primary-key clause was exposed";

/// What one source inspection observed through the public API.
struct Inspection {
    /// The first relation's primary key, when the scenario inspected it.
    key: Option<PrimaryKey>,
    /// Scanner diagnostics, rendered as their custom messages.
    diagnostics: Vec<String>,
    /// Names of the relations the scanner accepted.
    relation_names: Vec<String>,
}

/// Shared state passed between primary-key steps.
#[derive(Default)]
struct PrimaryKeyWorld {
    /// Source written by the `Given` step.
    source: RefCell<String>,
    /// Result of the most recent inspection step.
    inspection: RefCell<Option<Inspection>>,
}

impl PrimaryKeyWorld {
    /// The inspection recorded by a `When` step.
    fn inspection(&self) -> StepResult<Ref<'_, Inspection>, String> {
        Ref::filter_map(self.inspection.borrow(), Option::as_ref)
            .map_err(|_| "the source has not been inspected".to_owned())
    }
}

/// Provides the empty world each scenario fills, letting steps share state
/// without globals.
#[allow_fixture_expansion_lints]
#[fixture]
fn world() -> PrimaryKeyWorld { PrimaryKeyWorld::default() }

/// The exposed clause, or an error naming the missing step.
fn exposed_key(inspection: &Inspection) -> StepResult<&PrimaryKey, String> {
    inspection.key.as_ref().ok_or_else(|| NO_KEY.to_owned())
}

/// The clause's expression, or an error when the key has none.
fn exposed_expression(inspection: &Inspection) -> StepResult<&PrimaryKeyExpression, String> {
    exposed_key(inspection)?
        .expression()
        .ok_or_else(|| "the key has no expression".to_owned())
}

/// Scanner diagnostics as their custom messages.
fn diagnostic_messages(parsed: &Parsed) -> Vec<String> {
    parsed
        .errors()
        .iter()
        .map(|error| match error.reason() {
            SimpleReason::Custom(message) => message.clone(),
            reason => format!("{reason:?}"),
        })
        .collect()
}

/// Names of the relations the scanner accepted.
fn relation_names(parsed: &Parsed) -> Vec<String> {
    parsed
        .root()
        .relations()
        .into_iter()
        .filter_map(|relation| relation.name())
        .collect()
}

/// Record what the scanner and accessor exposed for `parsed`.
fn record_inspection(world: &PrimaryKeyWorld, parsed: &Parsed, key: Option<PrimaryKey>) {
    let inspection = Inspection {
        key,
        diagnostics: diagnostic_messages(parsed),
        relation_names: relation_names(parsed),
    };
    *world.inspection.borrow_mut() = Some(inspection);
}

/// Records the source under test for the inspection step that follows.
#[given("the DDlog source {source:string}")]
fn given_source(world: &PrimaryKeyWorld, source: &str) {
    source.clone_into(&mut world.source.borrow_mut());
}

/// Drives the public accessor for the first relation and records what it
/// exposed, failing when the source has no relation or the query errors.
#[when("the first relation's primary key is inspected")]
fn inspect_primary_key(world: &PrimaryKeyWorld) -> StepResult<(), String> {
    let parsed = parse(&world.source.borrow());
    let relation = parsed
        .root()
        .relations()
        .into_iter()
        .next()
        .ok_or_else(|| "the source has no relation".to_owned())?;
    let key = relation
        .primary_key()
        .map_err(|errors: RelationParseErrors| format!("primary key query failed: {errors:?}"))?;
    record_inspection(world, &parsed, key);
    Ok(())
}

/// Records only the scanner's view, so scenarios can assert on relation names
/// and diagnostics without touching a key.
#[when("the relations are listed")]
fn list_relations(world: &PrimaryKeyWorld) {
    let parsed = parse(&world.source.borrow());
    record_inspection(world, &parsed, None);
}

/// Compare a rendered observation with the text the feature step expected.
///
/// `noun` names the observation in the failure message, so each step keeps a
/// precise diagnostic while sharing one comparison.
fn expect_text(noun: &str, expected: &str, actual: &str) -> StepResult<(), String> {
    if actual == expected {
        return Ok(());
    }
    Err(format!("expected {noun} {expected:?}, got {actual:?}"))
}

/// Asserts the accessor's comma-joined binders match the feature text.
#[then("the primary-key binders are {expected:string}")]
fn primary_key_binders(world: &PrimaryKeyWorld, expected: &str) -> StepResult<(), String> {
    let inspection = world.inspection()?;
    let actual = exposed_key(&inspection)?.binder_names().join(", ");
    expect_text("binders", expected, &actual)
}

/// Asserts the exact expression slice matches the feature text, pinning that
/// surrounding trivia is excluded.
#[then("the primary-key expression text is {expected:string}")]
fn primary_key_expression_text(world: &PrimaryKeyWorld, expected: &str) -> StepResult<(), String> {
    let inspection = world.inspection()?;
    let actual = exposed_expression(&inspection)?.text();
    expect_text("expression", expected, actual)
}

/// Pins that a list-form key exposes binders but no spec-form expression.
#[then("the primary key has no expression")]
fn primary_key_without_expression(world: &PrimaryKeyWorld) -> StepResult<(), String> {
    let inspection = world.inspection()?;
    if exposed_key(&inspection)?.expression().is_some() {
        return Err("expected the primary key to have no expression".to_owned());
    }
    Ok(())
}

/// Parses the exposed expression and compares its s-expression, proving the
/// slice is a complete, valid expression.
#[then("the primary-key expression renders as {expected:string}")]
fn primary_key_expression_renders(
    world: &PrimaryKeyWorld,
    expected: &str,
) -> StepResult<(), String> {
    let inspection = world.inspection()?;
    let parsed = exposed_expression(&inspection)?
        .parse()
        .map_err(|errors| format!("expression parse failed: {errors:?}"))?;
    let actual = parsed.to_sexpr();
    if actual != expected {
        return Err(format!(
            "expected s-expression {expected:?}, got {actual:?}"
        ));
    }
    Ok(())
}

/// Pins that every error from a malformed expression lies inside the
/// expression's own span, keeping diagnostics located.
#[then("parsing the primary-key expression fails within its span")]
fn expression_parse_fails_within_span(world: &PrimaryKeyWorld) -> StepResult<(), String> {
    let inspection = world.inspection()?;
    let expression = exposed_expression(&inspection)?;
    let span = expression.span();
    let Err(errors) = expression.parse() else {
        return Err("expected the expression to fail to parse".to_owned());
    };
    if errors.is_empty() {
        return Err("expected at least one expression error".to_owned());
    }
    for error in &errors {
        let error_span = error.span();
        if error_span.start < span.start || error_span.end > span.end {
            return Err(format!(
                "error span {error_span:?} lies outside the expression span {span:?}"
            ));
        }
    }
    Ok(())
}

/// Asserts the scanner reported nothing, separating accessor behaviour from
/// recovery noise.
#[then("the parse has no parser-level errors")]
fn parse_has_no_errors(world: &PrimaryKeyWorld) -> StepResult<(), String> {
    let inspection = world.inspection()?;
    if !inspection.diagnostics.is_empty() {
        return Err(format!(
            "unexpected diagnostics: {:?}",
            inspection.diagnostics
        ));
    }
    Ok(())
}

/// Asserts the relations the scanner accepted, which also reveals whether a
/// malformed declaration was dropped.
#[then("the relation names are {expected:string}")]
fn accepted_relation_names(world: &PrimaryKeyWorld, expected: &str) -> StepResult<(), String> {
    let inspection = world.inspection()?;
    let actual = inspection.relation_names.join(", ");
    expect_text("relation names", expected, &actual)
}

/// Pins the keyless baseline: the relation is accepted and exposes no clause
/// at all.
#[then("the relation has no primary-key clause")]
fn relation_has_no_clause(world: &PrimaryKeyWorld) -> StepResult<(), String> {
    let inspection = world.inspection()?;
    if inspection.key.is_some() {
        return Err("expected the relation to have no primary-key clause".to_owned());
    }
    Ok(())
}

/// Asserts a reported diagnostic contains the expected code, so a rejected
/// form fails for the intended reason.
#[then("the parse reports the diagnostic {code:string}")]
fn parse_reports_diagnostic(world: &PrimaryKeyWorld, code: &str) -> StepResult<(), String> {
    let inspection = world.inspection()?;
    let is_reported = inspection
        .diagnostics
        .iter()
        .any(|message| message.contains(code));
    if !is_reported {
        return Err(format!(
            "diagnostic {code:?} not found in {:?}",
            inspection.diagnostics
        ));
    }
    Ok(())
}

/// Assert that the scenario's `When` step recorded an inspection.
///
/// Takes the world by value so the scenario body consumes its fixture, then
/// moves the recorded inspection out of the `RefCell`.
fn assert_inspection_recorded(world: PrimaryKeyWorld) {
    let inspection = world.inspection.into_inner();
    assert!(
        inspection.is_some(),
        "scenario steps must record an inspection"
    );
}

/// Covers the spec-form key scenario: the accessor exposes a typed tuple
/// expression as well as its binders.
#[scenario(
    path = "tests/features/relation_primary_key.feature",
    name = "Spec-form key exposes a typed tuple expression"
)]
fn spec_form_key_exposes_typed_tuple_expression(world: PrimaryKeyWorld) {
    assert_inspection_recorded(world);
}

/// Covers the list-form key scenario: binders are exposed while the expression
/// accessor stays `None`.
#[scenario(
    path = "tests/features/relation_primary_key.feature",
    name = "List-form key has binders but no expression"
)]
fn list_form_key_has_binders_but_no_expression(world: PrimaryKeyWorld) {
    assert_inspection_recorded(world);
}

/// Pins that a trailing comment after the key is excluded from the exposed
/// expression slice.
#[scenario(
    path = "tests/features/relation_primary_key.feature",
    name = "Trailing comment is not part of the key expression"
)]
fn trailing_comment_is_not_part_of_the_expression(world: PrimaryKeyWorld) {
    assert_inspection_recorded(world);
}

/// Pins that an unparsable expression still exposes the binders and reports a
/// located error.
#[scenario(
    path = "tests/features/relation_primary_key.feature",
    name = "Malformed key expression keeps binders and reports a located error"
)]
fn malformed_expression_keeps_binders(world: PrimaryKeyWorld) { assert_inspection_recorded(world); }

/// Pins that a `;` after the key does not swallow the declaration that
/// follows.
#[scenario(
    path = "tests/features/relation_primary_key.feature",
    name = "Statement terminator after a key does not hide the next declaration"
)]
fn terminator_does_not_hide_next_declaration(world: PrimaryKeyWorld) {
    assert_inspection_recorded(world);
}

/// Pins that a key on an `output` relation is rejected and no clause is
/// exposed.
#[scenario(
    path = "tests/features/relation_primary_key.feature",
    name = "Key on an output relation is rejected and not exposed"
)]
fn output_relation_key_is_rejected(world: PrimaryKeyWorld) { assert_inspection_recorded(world); }

/// Covers the keyless scenario: the relation parses and exposes no
/// primary-key clause.
#[scenario(
    path = "tests/features/relation_primary_key.feature",
    name = "Relation without a key has no primary-key clause"
)]
fn relation_without_key_has_no_clause(world: PrimaryKeyWorld) { assert_inspection_recorded(world); }
