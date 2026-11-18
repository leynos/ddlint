//! Behavioural coverage for block expressions and collection literals.
//!
//! Each scenario drives the public expression parser and asserts the
//! observable AST shape or diagnostic, keeping the feature text as the
//! specification of expected behaviour.

use std::cell::{Ref, RefCell};

use chumsky::error::SimpleReason;
use ddlint::{
    SyntaxKind,
    parser::{
        ast::{BlockExpr, Expr},
        expression::parse_expression,
    },
    test_util::{block, call, if_expr, var},
};
use fixture_lints::allow_fixture_expansion_lints;
use rstest::fixture;
use rstest_bdd::StepResult;
use rstest_bdd_macros::{given, scenario, then, when};

/// Shared state passed between block-expression steps.
#[derive(Default)]
struct BlockWorld {
    /// Source written by the `Given` step.
    source: RefCell<String>,
    /// Parsed expression, when parsing succeeded.
    parsed: RefCell<Option<Expr>>,
    /// Diagnostic messages, when parsing was attempted.
    diagnostics: RefCell<Option<Vec<String>>>,
}

impl BlockWorld {
    /// The parsed expression recorded by the `When` step.
    fn parsed(&self) -> StepResult<Ref<'_, Expr>, String> {
        Ref::filter_map(self.parsed.borrow(), Option::as_ref)
            .map_err(|_| "the expression has not been parsed".to_owned())
    }

    /// The parsed block, or an error naming what was parsed instead.
    fn block(&self) -> StepResult<Ref<'_, BlockExpr>, String> {
        Ref::filter_map(self.parsed.borrow(), |parsed| {
            parsed.as_ref().and_then(|expr| match expr {
                Expr::Block(block) => Some(block),
                _ => None,
            })
        })
        .map_err(|parsed| format!("expected a block, got {parsed:?}"))
    }

    /// The diagnostic messages recorded by the `When` step.
    fn diagnostics(&self) -> StepResult<Ref<'_, Vec<String>>, String> {
        Ref::filter_map(self.diagnostics.borrow(), Option::as_ref)
            .map_err(|_| "the expression has not been parsed".to_owned())
    }
}

/// Provides the empty world each scenario fills, letting steps share state
/// without globals.
#[allow_fixture_expansion_lints]
#[fixture]
fn world() -> BlockWorld { BlockWorld::default() }

/// Render a diagnostic as its message, so steps compare text rather than the
/// diagnostic's debug form.
fn diagnostic_message(error: &chumsky::error::Simple<SyntaxKind>) -> String {
    match error.reason() {
        SimpleReason::Custom(message) => message.clone(),
        reason => format!("{reason:?}"),
    }
}

/// Build one expected item from the feature's data table.
///
/// The table names each expected item as source text. Building the AST here
/// rather than parsing that text keeps the expectation independent of the
/// parser under test: a regression that turns `go()` into a variable would
/// otherwise change both the parsed value and the expectation together.
fn expected_item(item: &str) -> StepResult<Expr, String> {
    match item {
        "a" => Ok(var("a")),
        "b" => Ok(var("b")),
        "c" => Ok(var("c")),
        "d" => Ok(var("d")),
        "go()" => Ok(call("go", Vec::new())),
        "if c { a } else { b }" => Ok(if_expr(
            var("c"),
            block(Vec::new(), Some(var("a"))),
            Some(block(Vec::new(), Some(var("b")))),
        )),
        other => Err(format!(
            "no independent expectation for item {other:?}; add a fixture"
        )),
    }
}

/// Records the source under test for the parse step that follows.
#[given("the expression source {source:string}")]
fn given_expression_source(world: &BlockWorld, source: &str) {
    source.clone_into(&mut world.source.borrow_mut());
}

/// Parses the recorded source, keeping whichever outcome the scenario asserts.
#[when("the expression is parsed")]
fn parse_the_expression(world: &BlockWorld) {
    let source = world.source.borrow().clone();
    let (parsed, diagnostics) = match parse_expression(&source) {
        Ok(expr) => (Some(expr), Vec::new()),
        Err(errors) => (None, errors.iter().map(diagnostic_message).collect()),
    };
    *world.parsed.borrow_mut() = parsed;
    *world.diagnostics.borrow_mut() = Some(diagnostics);
}

/// Asserts the block's statement list, comparing parsed items rather than
/// table text.
#[then("the result is a block with statements")]
fn block_statements(world: &BlockWorld, datatable: Vec<Vec<String>>) -> StepResult<(), String> {
    let block = world.block()?;
    let expected = datatable
        .iter()
        .skip(1) // The first row is the `item` header.
        .map(|row| {
            row.first().map_or_else(
                || Err("a statement row must name one item".to_owned()),
                |item| expected_item(item.as_str()),
            )
        })
        .collect::<Result<Vec<_>, _>>()?;
    if block.statements.as_slice() != expected.as_slice() {
        return Err(format!(
            "expected statements {expected:?}, got {:?}",
            block.statements
        ));
    }
    Ok(())
}

/// Asserts the block's final item by parsing the feature's expected text.
#[then("the block value is {expected:string}")]
fn block_value(world: &BlockWorld, expected: &str) -> StepResult<(), String> {
    let block = world.block()?;
    let wanted = expected_item(expected)?;
    match block.value.as_deref() {
        Some(value) if *value == wanted => Ok(()),
        Some(value) => Err(format!("expected block value {wanted:?}, got {value:?}")),
        None => Err(format!("expected block value {wanted:?}, got none")),
    }
}

/// Pins the empty block and trailing-separator cases, where `()` is the value.
#[then("the block has no value")]
fn block_has_no_value(world: &BlockWorld) -> StepResult<(), String> {
    let block = world.block()?;
    if block.value.is_some() {
        return Err(format!("expected no block value, got {:?}", block.value));
    }
    Ok(())
}

/// Pins `{}` as a block with no statements rather than a collection literal.
#[then("the result is a block with no statements")]
fn block_without_statements(world: &BlockWorld) -> StepResult<(), String> {
    let block = world.block()?;
    if !block.statements.is_empty() {
        return Err(format!(
            "expected no statements, got {:?}",
            block.statements
        ));
    }
    Ok(())
}

/// Compares the parsed shape with the form named by the scenario outline.
#[then("the result is a {form}")]
fn result_has_form(world: &BlockWorld, form: &str) -> StepResult<(), String> {
    let expr = world.parsed()?;
    let matched = match form {
        "vector literal" => matches!(&*expr, Expr::VecLit(_)),
        "map literal" => matches!(&*expr, Expr::MapLit(_)),
        "block" => matches!(&*expr, Expr::Block(_)),
        "parenthesized group" => matches!(&*expr, Expr::Group(_)),
        other => return Err(format!("unknown form {other:?}")),
    };
    if matched {
        Ok(())
    } else {
        Err(format!("expected a {form}, got {expr:?}"))
    }
}

/// Asserts that one reported message equals the quoted text exactly.
#[then("parsing fails with {message:string}")]
fn parsing_fails_with(world: &BlockWorld, message: &str) -> StepResult<(), String> {
    let diagnostics = world.diagnostics()?;
    if diagnostics.iter().any(|actual| actual == message) {
        Ok(())
    } else {
        Err(format!(
            "expected diagnostic {message:?}, got {diagnostics:?}"
        ))
    }
}

/// Pins that a braced branch of an `if` is a block, including when empty.
#[then("the result is an if expression whose then branch is an empty block")]
fn if_then_branch_is_empty_block(world: &BlockWorld) -> StepResult<(), String> {
    let expr = world.parsed()?;
    match &*expr {
        Expr::IfElse { then_branch, .. } => match then_branch.as_ref() {
            Expr::Block(block) if block.statements.is_empty() && block.value.is_none() => Ok(()),
            other => Err(format!("expected an empty then block, got {other:?}")),
        },
        other => Err(format!("expected an if expression, got {other:?}")),
    }
}

/// Pins that a braced `else` branch is a block whose value is its final item,
/// so the then-branch assertion cannot stand in for both branches.
#[then("the else branch is a block whose value is {expected:string}")]
fn if_else_branch_is_block_with_value(
    world: &BlockWorld,
    expected: &str,
) -> StepResult<(), String> {
    let expr = world.parsed()?;
    let wanted = expected_item(expected)?;
    let Expr::IfElse { else_branch, .. } = &*expr else {
        return Err(format!("expected an if expression, got {expr:?}"));
    };
    let Expr::Block(block) = else_branch.as_ref() else {
        return Err(format!("expected an else block, got {else_branch:?}"));
    };
    match block.value.as_deref() {
        Some(value) if *value == wanted => Ok(()),
        Some(value) => Err(format!(
            "expected else block value {wanted:?}, got {value:?}"
        )),
        None => Err(format!("expected else block value {wanted:?}, got none")),
    }
}

/// Assert that the scenario's `When` step recorded an outcome.
fn assert_scenario_ran(world: &BlockWorld) {
    assert!(
        world.diagnostics.borrow().is_some(),
        "scenario steps must parse the expression"
    );
}

/// Covers statement ordering: `{ a; b; c }` has two statements and a value.
#[scenario(
    path = "tests/features/block_expressions.feature",
    name = "Semicolons separate block statements in order"
)]
fn semicolons_separate_block_statements(world: BlockWorld) { assert_scenario_ran(&world); }

/// Covers the trailing separator: `{ a; }` has one statement and no value.
#[scenario(
    path = "tests/features/block_expressions.feature",
    name = "A trailing semicolon makes the block value unit"
)]
fn trailing_semicolon_makes_the_value_unit(world: BlockWorld) { assert_scenario_ran(&world); }

/// Covers `{}`: an empty block, not an empty collection.
#[scenario(
    path = "tests/features/block_expressions.feature",
    name = "Empty braces are an empty block"
)]
fn empty_braces_are_an_empty_block(world: BlockWorld) { assert_scenario_ran(&world); }

/// Covers separator scoping: an `if` tail inside braces does not swallow `;`.
#[scenario(
    path = "tests/features/block_expressions.feature",
    name = "An if expression inside a block does not swallow the separator"
)]
fn if_inside_block_keeps_the_separator(world: BlockWorld) { assert_scenario_ran(&world); }

/// Covers each delimiter's shape: brackets build collections, braces blocks.
#[scenario(
    path = "tests/features/block_expressions.feature",
    name = "Brackets build vectors and maps; braces build blocks"
)]
fn brackets_and_braces_build_different_shapes(source: String, form: String, world: BlockWorld) {
    assert!(!source.is_empty(), "each outline row supplies a source");
    assert!(!form.is_empty(), "each outline row supplies a form");
    assert_scenario_ran(&world);
}

/// Covers the malformed forms with their exact diagnostics.
#[scenario(
    path = "tests/features/block_expressions.feature",
    name = "Malformed braces and brackets are rejected"
)]
fn malformed_braces_and_brackets_are_rejected(source: String, message: String, world: BlockWorld) {
    assert!(!source.is_empty(), "each outline row supplies a source");
    assert!(!message.is_empty(), "each outline row supplies a message");
    assert_scenario_ran(&world);
}

/// Covers `if ready {} else { go() }`: the empty then branch is a block.
#[scenario(
    path = "tests/features/block_expressions.feature",
    name = "Braced if branches with an empty block parse"
)]
fn braced_if_branches_with_empty_block_parse(world: BlockWorld) { assert_scenario_ran(&world); }
