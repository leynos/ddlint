//! Property tests for block expressions.
//!
//! The generated sources exercise item ordering, the optional trailing
//! separator, and panic freedom over arbitrary brace-bearing token soup.
//! Generators and the expected-AST oracle live in [`strategy`].

use proptest::{prelude::*, strategy::ValueTree, test_runner::TestRunner};

use super::strategy;
use crate::{
    parser::{
        ast::{BlockExpr, Expr},
        expression::parse_expression,
    },
    test_util::{block, if_expr, var},
};

/// Generator classes the block properties must exercise.
///
/// [`generator_covers_every_class`] asserts that each of these appears often
/// enough in a deterministic sample, so a generator change cannot silently
/// stop testing separator handling, nesting, or a construct tail.
const CLASS_NAMES: [&str; 16] = [
    "empty block",
    "single value",
    "single statement",
    "multi value",
    "multi statement",
    "nested block",
    "map first",
    "if tail",
    "for tail",
    "return tail",
    "closure tail",
    "tuple item",
    "parenthesized sequence",
    "vector item",
    "call item",
    "binary item",
];

/// Return `true` when a generated item list can form a block.
///
/// `{ ; }` has no item to separate and is rejected by design, so the property
/// and its coverage check skip the same draws.
const fn is_representable(items: &[&str], ends_with_semicolon: bool) -> bool {
    !items.is_empty() || !ends_with_semicolon
}

/// Mark every class this generated block belongs to.
fn classes_of(items: &[&str], ends_with_semicolon: bool) -> [bool; CLASS_NAMES.len()] {
    let first_is = |source: &str| items.first() == Some(&source);
    // A construct tail is the final item with no trailing `;`, which is the
    // position where an item boundary could swallow the following separator.
    let is_tail = |source: &str| items.last() == Some(&source) && !ends_with_semicolon;
    [
        items.is_empty(),
        items.len() == 1 && !ends_with_semicolon,
        items.len() == 1 && ends_with_semicolon,
        items.len() >= 2 && !ends_with_semicolon,
        items.len() >= 2 && ends_with_semicolon,
        items.contains(&"{ a }"),
        first_is("[k -> v]"),
        is_tail("if c { a } else { b }"),
        is_tail("for (x in xs) f(x)"),
        is_tail("return x"),
        is_tail("|x| x"),
        items.contains(&"(a, b)"),
        items.contains(&"(a; b)"),
        items.contains(&"[a, b]"),
        items.contains(&"f()"),
        items.contains(&"x + 1"),
    ]
}

proptest! {
    #[test]
    fn generated_blocks_preserve_items_and_trailing_separator(
        items in prop::collection::vec(strategy::item_source(), 0..5),
        ends_with_semicolon in any::<bool>(),
    ) {
        // `{ ; }` has no item to separate and is rejected by design.
        prop_assume!(is_representable(&items, ends_with_semicolon));

        let src = strategy::render_block(&items, ends_with_semicolon);
        let oracle = items
            .iter()
            .map(|item| strategy::expected_item(item))
            .collect::<Option<Vec<_>>>();
        let Some(expected_items) = oracle else {
            return Err(TestCaseError::fail(format!("missing oracle for {items:?}")));
        };
        // Assemble the expected block here rather than through
        // `BlockExpr::from_items`, so a change to the split rule cannot move
        // both the parser and the oracle in the same step.
        let expected = if ends_with_semicolon {
            Expr::Block(BlockExpr {
                statements: expected_items,
                value: None,
            })
        } else if let Some((last, rest)) = expected_items.split_last() {
            Expr::Block(BlockExpr {
                statements: rest.to_vec(),
                value: Some(Box::new(last.clone())),
            })
        } else {
            Expr::Block(BlockExpr::default())
        };

        let parsed = parse_expression(&src)
            .map_err(|errs| TestCaseError::fail(format!("source {src:?} errors: {errs:?}")))?;

        prop_assert_eq!(parsed, expected, "source {}", src);
    }

    #[test]
    fn malformed_brace_sources_never_panic(src in strategy::brace_token_soup()) {
        // Panic freedom is the assertion: either a tree or diagnostics are
        // acceptable, but the parser must terminate with one of them.
        drop(parse_expression(&src));
    }
}

/// A construct followed by `;` is a statement, not a tail.
///
/// The `if tail` class only counts an `if` with no separator after it, so it
/// cannot establish this boundary: the `;` must end the `if` item and leave
/// the next item as the block's value.
#[test]
fn if_statement_followed_by_separator_leaves_the_next_item_as_value() {
    let parsed = parse_expression("{ if c { a } else { b }; x }")
        .expect("an `if` statement followed by `;` should parse");
    let Expr::Block(block_expr) = parsed else {
        panic!("expected a block, got {parsed:?}");
    };
    assert_eq!(
        block_expr.statements,
        vec![if_expr(
            var("c"),
            block(vec![], Some(var("a"))),
            Some(block(vec![], Some(var("b"))))
        )],
        "the `if` must be the block's only statement"
    );
    assert_eq!(
        block_expr.value.as_deref(),
        Some(&var("x")),
        "the item after the separator must be the block value"
    );
}

#[test]
fn generator_covers_every_class() {
    let strategy = (
        prop::collection::vec(strategy::item_source(), 0..5),
        any::<bool>(),
    );
    let mut runner = TestRunner::deterministic();
    let mut counts = [0usize; CLASS_NAMES.len()];
    for _ in 0..256 {
        let (items, ends_with_semicolon) = strategy
            .new_tree(&mut runner)
            .expect("the item strategy always builds a tree")
            .current();
        // Count only draws the property actually parses, so rejected draws
        // cannot satisfy a class threshold on their own.
        if !is_representable(&items, ends_with_semicolon) {
            continue;
        }
        let present = classes_of(&items, ends_with_semicolon);
        for (count, is_present) in counts.iter_mut().zip(present) {
            *count += usize::from(is_present);
        }
    }
    for (name, count) in CLASS_NAMES.iter().zip(counts) {
        assert!(
            count >= 5,
            "class {name:?} appeared {count} times in 256 draws; the generator must exercise \
             every class"
        );
    }
}
