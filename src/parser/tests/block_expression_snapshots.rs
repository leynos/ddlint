//! Inline snapshot of how block and collection inputs parse.
//!
//! `V-B1` and `V-B6` require that the block and collection surfaces keep one
//! stable shape: accepted inputs render one S-expression, rejected inputs
//! report one located diagnostic. Recording both in one snapshot makes an
//! accidental change to either visible as a diff, and the semantic assertions
//! below pin the rules the rendering alone cannot state.

use chumsky::error::SimpleReason;

use crate::{
    SyntaxKind,
    parser::{
        ast::{BlockExpr, Expr},
        expression::parse_expression,
    },
    test_util::{BLOCK_ITEM_SEPARATOR_ERROR, EMPTY_BLOCK_ITEM_ERROR, UNCLOSED_BLOCK_ERROR},
};

/// Sources this surface accepts, in the order the snapshot records them.
const ACCEPTED: &[&str] = &[
    "{}",
    "{ x }",
    "{ 1 }",
    "{ f() }",
    "{ (x, y) }",
    "{ 1 + 2 }",
    "{ a; b }",
    "{ a; b; }",
    "{ a; }",
    "{ { a } }",
    "{ {a}; b }",
    "{ a; x: T }",
    "{ return }",
    "{ x = 1; x }",
    "{ [a -> 1] }",
    "[1]",
    "[1, 2]",
    "[a -> 1]",
    "[a -> 1, b -> 2]",
    "[a -> [b -> 2]]",
    "[[k -> v] -> 1]",
    "[(a, b) -> c]",
    "if ready {} else { go() }",
];

/// Sources this surface rejects, with the diagnostic the snapshot records.
const REJECTED: &[&str] = &[
    "{ a, b }",
    "{ a; b, c }",
    "{ a; b c }",
    "{ ; }",
    "{ a;; b }",
    "{ a",
    "{ a;",
    "({ x)",
    "{a: 1, b: 2}",
    "{ var x = 1; x }",
    "[]",
    "[a, b -> c]",
    "[a -> 1, b]",
    "[a -> 1, b",
    "[a ->]",
    "[-> a]",
];

/// Render one source as `source => s-expression` or `source => message @ span`.
fn render(source: &str) -> String {
    match parse_expression(source) {
        Ok(expr) => format!("{source:?} => {}", expr.to_sexpr()),
        Err(errors) => {
            assert_eq!(
                errors.len(),
                1,
                "rejected source {source:?} should report exactly one diagnostic"
            );
            errors.first().map_or_else(
                || format!("{source:?} => no diagnostic"),
                |error| {
                    let message = match error.reason() {
                        SimpleReason::Custom(message) => message.clone(),
                        reason => format!("{reason:?}"),
                    };
                    let span = error.span();
                    format!("{source:?} => {message} @ {}..{}", span.start, span.end)
                },
            )
        }
    }
}

/// Parse an accepted source, failing the test with its diagnostics.
fn accepted(source: &str) -> Expr {
    match parse_expression(source) {
        Ok(expr) => expr,
        Err(errors) => panic!("accepted source {source:?} errors: {errors:?}"),
    }
}

/// The first diagnostic's message for a rejected source.
fn rejection_message(source: &str) -> String {
    let errors = parse_expression(source).expect_err("rejected source should not parse");
    match errors.first().map(Simple::reason) {
        Some(SimpleReason::Custom(message)) => message.clone(),
        other => panic!("expected a custom diagnostic for {source:?}, got {other:?}"),
    }
}

/// Alias the diagnostic type so `Simple::reason` reads clearly above.
type Simple = chumsky::error::Simple<SyntaxKind>;

/// Extract the block an accepted source must parse to.
fn accepted_block(source: &str) -> BlockExpr {
    match accepted(source) {
        Expr::Block(block) => block,
        other => panic!("{source:?} must be a block, got {other:?}"),
    }
}

#[test]
fn empty_braces_are_the_empty_block() {
    let block = accepted_block("{}");
    assert!(
        block.statements.is_empty() && block.value.is_none(),
        "empty braces must produce the empty block, got {block:?}"
    );
}

#[test]
fn trailing_separator_leaves_no_value() {
    let block = accepted_block("{ a; b; }");
    assert!(
        block.statements.len() == 2 && block.value.is_none(),
        "a trailing separator must leave no value, got {block:?}"
    );
    assert!(
        block.ends_with_semicolon(),
        "the separator before '}}' must be remembered"
    );
}

#[test]
fn ascription_is_the_block_value() {
    let block = accepted_block("{ x: T }");
    assert!(
        block.statements.is_empty() && block.value.is_some(),
        "an ascription is the block value, got {block:?}"
    );
}

#[test]
fn bracket_arrow_stays_a_map_literal() {
    assert!(
        matches!(accepted("[a -> 1]"), Expr::MapLit(entries) if entries.len() == 1),
        "brackets with an arrow must stay a map literal"
    );
}

#[test]
fn block_diagnostics_name_the_defect() {
    assert_eq!(
        rejection_message("{ a, b }"),
        BLOCK_ITEM_SEPARATOR_ERROR,
        "a comma must be reported as a missing block separator"
    );
    assert_eq!(
        rejection_message("{ a;; b }"),
        EMPTY_BLOCK_ITEM_ERROR,
        "a doubled separator must be reported as an empty item"
    );
    assert_eq!(
        rejection_message("{ a"),
        UNCLOSED_BLOCK_ERROR,
        "an unterminated block must be reported as unclosed"
    );
}

#[test]
fn block_and_collection_rendering_is_stable() {
    let rows: Vec<String> = ACCEPTED
        .iter()
        .chain(REJECTED)
        .map(|source| render(source))
        .collect();
    insta::assert_snapshot!(rows.join("\n"), @r#"
    "{}" => (block)
    "{ x }" => (block (value x))
    "{ 1 }" => (block (value 1))
    "{ f() }" => (block (value (call f)))
    "{ (x, y) }" => (block (value (tuple x y)))
    "{ 1 + 2 }" => (block (value (+ 1 2)))
    "{ a; b }" => (block (stmts a) (value b))
    "{ a; b; }" => (block (stmts a b))
    "{ a; }" => (block (stmts a))
    "{ { a } }" => (block (value (block (value a))))
    "{ {a}; b }" => (block (stmts (block (value a))) (value b))
    "{ a; x: T }" => (block (stmts a) (value (: x T)))
    "{ return }" => (block (value (return (tuple))))
    "{ x = 1; x }" => (block (stmts (= x 1)) (value x))
    "{ [a -> 1] }" => (block (value (map (entry a 1))))
    "[1]" => (vec 1)
    "[1, 2]" => (vec 1 2)
    "[a -> 1]" => (map (entry a 1))
    "[a -> 1, b -> 2]" => (map (entry a 1) (entry b 2))
    "[a -> [b -> 2]]" => (map (entry a (map (entry b 2))))
    "[[k -> v] -> 1]" => (map (entry (map (entry k v)) 1))
    "[(a, b) -> c]" => (map (entry (tuple a b) c))
    "if ready {} else { go() }" => (if ready (block) (block (value (call go))))
    "{ a, b }" => expected ';' or '}' after a block item @ 3..4
    "{ a; b, c }" => expected ';' or '}' after a block item @ 6..7
    "{ a; b c }" => expected ';' or '}' after a block item @ 7..8
    "{ ; }" => empty statement in block: remove the extra ';' @ 2..3
    "{ a;; b }" => empty statement in block: remove the extra ';' @ 4..5
    "{ a" => expected '}' to close block @ 3..3
    "{ a;" => expected '}' to close block @ 4..4
    "({ x)" => expected '}' to close block @ 4..5
    "{a: 1, b: 2}" => expected ';' or '}' after a block item @ 5..6
    "{ var x = 1; x }" => unexpected token: K_VAR @ 2..5
    "[]" => empty vector literals are not DDlog syntax; use vec_empty() instead @ 1..2
    "[a, b -> c]" => cannot mix vector elements and map entries in one literal @ 4..5
    "[a -> 1, b]" => cannot mix vector elements and map entries in one literal @ 9..10
    "[a -> 1, b" => expected '->' in map entry @ 9..10
    "[a ->]" => unexpected token: T_RBRACKET @ 5..6
    "[-> a]" => unexpected token: T_ARROW @ 1..3
    "#);
}
