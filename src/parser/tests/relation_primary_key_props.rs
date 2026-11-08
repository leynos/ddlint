//! Property tests for typed primary-key access.
//!
//! These properties check the invariants recorded in the `ExecPlan` against
//! generated sources whose expected binders and expression text come from the
//! generator, not from the CST the accessor reads.

use std::{cell::RefCell, collections::HashSet};

use chumsky::error::SimpleReason;
use proptest::{prelude::*, test_runner::TestRunner};

use super::relation_primary_key_strategies::{
    GeneratedKey,
    IndependencePair,
    KeyClass,
    VALID_CLASSES,
    generated_key_strategy,
    independence_pair_strategy,
    invalid_key_strategy,
    terminator_key_strategy,
    valid_key_strategy,
};
use crate::{
    Parsed,
    parse,
    parser::{
        ast::{PrimaryKey, PrimaryKeyExpression},
        expression::parse_expression,
    },
};

/// Diagnostic messages of `parsed`, without spans.
///
/// `INV-TERM` compares a keyed declaration with its keyless twin, whose
/// scanner diagnostics sit at different offsets. Extracting the custom
/// message alone keeps the comparison about behaviour, not position.
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
fn relation_names(parsed: &Parsed) -> Vec<Option<String>> {
    parsed
        .root()
        .relations()
        .into_iter()
        .map(|relation| relation.name())
        .collect()
}

/// The primary key of the first relation in `parsed`.
fn primary_key_of(parsed: &Parsed) -> Result<PrimaryKey, TestCaseError> {
    let relation = parsed
        .root()
        .relations()
        .into_iter()
        .next()
        .ok_or_else(|| TestCaseError::fail("generated source produced no relation"))?;
    relation
        .primary_key()
        .map_err(|errors| TestCaseError::fail(format!("primary key query failed: {errors:?}")))?
        .ok_or_else(|| TestCaseError::fail("generated source produced no primary key"))
}

/// `INV-AGREE`: the accessor reports exactly the generated clause.
fn check_agreement(generated: &GeneratedKey) -> Result<(), TestCaseError> {
    let parsed = parse(&generated.source);
    prop_assert!(
        parsed.errors().is_empty(),
        "unexpected diagnostics for {:?}: {:?}",
        generated.source,
        diagnostic_messages(&parsed)
    );
    let key = primary_key_of(&parsed)?;
    let expected: Vec<&str> = generated.binders.iter().map(String::as_str).collect();
    prop_assert_eq!(key.binder_names(), expected);
    let text = key.expression().map(PrimaryKeyExpression::text);
    prop_assert_eq!(text, generated.expression.as_deref());
    for binder in key.binders() {
        prop_assert_eq!(generated.source.get(binder.span()), Some(binder.name()));
    }
    if let Some(expression) = key.expression() {
        prop_assert_eq!(
            generated.source.get(expression.span()),
            Some(expression.text())
        );
        prop_assert!(
            expression.span().start > 0,
            "expression must start after the relation prefix"
        );
    }
    Ok(())
}

/// `INV-INDEP`: binders do not depend on the expression's validity.
fn check_independence(pair: &IndependencePair) -> Result<(), TestCaseError> {
    let valid = primary_key_of(&parse(&pair.valid_source))?;
    let invalid = primary_key_of(&parse(&pair.invalid_source))?;
    let expected: Vec<&str> = pair.binders.iter().map(String::as_str).collect();
    prop_assert_eq!(valid.binder_names(), expected.clone());
    prop_assert_eq!(invalid.binder_names(), expected);
    let valid_expression = valid
        .expression()
        .ok_or_else(|| TestCaseError::fail("valid arm has no expression"))?;
    prop_assert!(valid_expression.parse().is_ok());
    let invalid_expression = invalid
        .expression()
        .ok_or_else(|| TestCaseError::fail("invalid arm has no expression"))?;
    prop_assert!(invalid_expression.parse().is_err());
    Ok(())
}

/// `INV-SHIFT`: expression errors move by exactly the expression's start.
fn check_shift(generated: &GeneratedKey) -> Result<(), TestCaseError> {
    let key = primary_key_of(&parse(&generated.source))?;
    let expression = key
        .expression()
        .ok_or_else(|| TestCaseError::fail("invalid mutation has no expression"))?;
    let absolute = expression
        .parse()
        .expect_err("invalid mutation must fail to parse");
    let relative = parse_expression(expression.text())
        .expect_err("invalid mutation must fail to parse directly");
    prop_assert_eq!(absolute.len(), relative.len());
    let offset = expression.span().start;
    prop_assert!(
        offset > 0,
        "expression must start after the relation prefix"
    );
    for (absolute_error, relative_error) in absolute.iter().zip(&relative) {
        let absolute_span = absolute_error.span();
        let relative_span = relative_error.span();
        prop_assert_eq!(absolute_span.start, relative_span.start + offset);
        prop_assert_eq!(absolute_span.end, relative_span.end + offset);
        prop_assert!(absolute_span.start >= expression.span().start);
        prop_assert!(absolute_span.end <= expression.span().end);
    }
    Ok(())
}

/// `INV-FIDELITY`: the lazy parse agrees with the direct parse.
fn check_fidelity(generated: &GeneratedKey) -> Result<(), TestCaseError> {
    let key = primary_key_of(&parse(&generated.source))?;
    let Some(expression) = key.expression() else {
        return Ok(());
    };
    let parsed_expression = expression
        .parse()
        .expect("generated valid expression must parse");
    let direct =
        parse_expression(expression.text()).expect("generated valid expression must parse");
    prop_assert_eq!(parsed_expression, direct);
    Ok(())
}

/// `INV-TERM`: a `;` after a key does not change what follows.
fn check_terminator(generated: &GeneratedKey) -> Result<(), TestCaseError> {
    let keyed = parse(&generated.source_with_suffix(";"));
    let keyless = parse(&format!("{};", generated.keyless_source));
    prop_assert_eq!(relation_names(&keyed), relation_names(&keyless));
    prop_assert_eq!(diagnostic_messages(&keyed), diagnostic_messages(&keyless));
    prop_assert!(!relation_names(&keyed).is_empty());
    Ok(())
}

/// Draw every class one strategy produces in a fixed-seed run.
///
/// `TestRunner::run` accepts only a shared-reference closure, so the class set
/// lives behind a `RefCell` rather than being captured mutably.
fn draw_classes(strategy: impl Strategy<Value = GeneratedKey>) -> HashSet<KeyClass> {
    let mut runner = TestRunner::deterministic();
    let classes = RefCell::new(HashSet::new());
    runner
        .run(&strategy, |generated| {
            classes.borrow_mut().insert(generated.class);
            Ok(())
        })
        .expect("class coverage run");
    classes.into_inner()
}

/// Every strategy must reach every class it promises.
#[test]
fn strategies_cover_every_class() {
    let valid = draw_classes(valid_key_strategy());
    for class in VALID_CLASSES {
        assert!(
            valid.contains(&class),
            "valid strategy missed {class:?}: {valid:?}"
        );
    }
    let invalid = draw_classes(invalid_key_strategy());
    assert_eq!(invalid, HashSet::from([KeyClass::InvalidMutation]));
    let combined = draw_classes(generated_key_strategy());
    assert_eq!(
        combined.len(),
        VALID_CLASSES.len() + 1,
        "combined strategy classes: {combined:?}"
    );
}

proptest! {
    #[test]
    fn accessor_agrees_with_generated_clause(generated in generated_key_strategy()) {
        check_agreement(&generated)?;
    }

    #[test]
    fn binders_are_independent_of_expression(pair in independence_pair_strategy()) {
        check_independence(&pair)?;
    }

    #[test]
    fn error_spans_are_shifted(generated in invalid_key_strategy()) {
        check_shift(&generated)?;
    }

    #[test]
    fn lazy_parse_matches_direct_parse(generated in valid_key_strategy()) {
        check_fidelity(&generated)?;
    }

    #[test]
    fn semicolon_preserves_structure(generated in terminator_key_strategy()) {
        check_terminator(&generated)?;
    }
}
