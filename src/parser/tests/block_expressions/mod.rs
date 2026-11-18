//! Tests for brace-delimited block expressions.
//!
//! Braces always introduce a block, so these tests cover item ordering,
//! trailing separators, the empty block, nested delimiters, and the
//! diagnostics for malformed blocks. Property tests live in
//! [`props`].

use rstest::rstest;

use crate::{
    parser::{
        ast::{BinaryOp, BlockExpr, Expr},
        expression::parse_expression,
    },
    test_util::{
        BLOCK_ITEM_SEPARATOR_ERROR,
        EMPTY_BLOCK_ITEM_ERROR,
        UNCLOSED_BLOCK_ERROR,
        assert_parse_error,
        block,
        call,
        closure,
        for_loop,
        if_expr,
        lit_num,
        map_entry,
        map_lit,
        match_arm,
        match_expr,
        return_expr,
        tuple,
        var,
        vec_lit,
    },
};

mod props;
mod strategy;

/// Build a sequencing expression `lhs; rhs`.
fn seq(lhs: Expr, rhs: Expr) -> Expr {
    Expr::Binary {
        op: BinaryOp::Seq,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
    }
}

/// Build an assignment expression `lhs = rhs`.
fn assign(lhs: Expr, rhs: Expr) -> Expr {
    Expr::Binary {
        op: BinaryOp::Assign,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
    }
}

/// Build a type ascription `lhs: rhs`.
fn ascribe(lhs: Expr, rhs: Expr) -> Expr {
    Expr::Binary {
        op: BinaryOp::Ascribe,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
    }
}

#[rstest]
#[case::variable("{ x }", block(vec![], Some(var("x"))))]
#[case::number("{ 1 }", block(vec![], Some(lit_num("1"))))]
#[case::call("{ f() }", block(vec![], Some(call("f", vec![]))))]
#[case::tuple("{ (x, y) }", block(vec![], Some(tuple(vec![var("x"), var("y")]))))]
#[case::binary(
    "{ 1 + 2 }",
    block(
        vec![],
        Some(Expr::Binary {
            op: BinaryOp::Add,
            lhs: Box::new(lit_num("1")),
            rhs: Box::new(lit_num("2")),
        })
    )
)]
#[case::statements("{ a; b }", block(vec![var("a")], Some(var("b"))))]
#[case::trailing_semicolon("{ a; b; }", block(vec![var("a"), var("b")], None))]
#[case::single_statement("{ a; }", block(vec![var("a")], None))]
#[case::empty("{}", block(vec![], None))]
#[case::nested_block("{ { a } }", block(vec![], Some(block(vec![], Some(var("a"))))))]
#[case::nested_statement("{ {a}; b }", block(vec![block(vec![], Some(var("a")))], Some(var("b"))))]
#[case::parenthesized_sequence("{ (a; b) }", block(vec![], Some(Expr::Group(Box::new(seq(var("a"), var("b")))))))]
#[case::ascription("{ x: T }", block(vec![], Some(ascribe(var("x"), var("T")))))]
#[case::statement_then_ascription("{ a; x: T }", block(vec![var("a")], Some(ascribe(var("x"), var("T")))))]
#[case::return_without_value("{ return }", block(vec![], Some(return_expr(None))))]
#[case::assignment_sequence("{ x = 1; x }", block(vec![assign(var("x"), lit_num("1"))], Some(var("x"))))]
#[case::closure_then_value("{ |x| x; y }", block(vec![closure(["x"], var("x"))], Some(var("y"))))]
#[case::map_value(
    "{ [a -> 1] }",
    block(vec![], Some(map_lit(vec![map_entry(var("a"), lit_num("1"))])))
)]
#[case::match_then_value(
    "{ match (x) { _ -> 1 }; y }",
    block(
        vec![match_expr(var("x"), vec![match_arm("_", lit_num("1"))])],
        Some(var("y"))
    )
)]
fn parses_blocks(#[case] src: &str, #[case] expected: Expr) {
    let expr =
        parse_expression(src).unwrap_or_else(|errs| panic!("source {src:?} errors: {errs:?}"));
    assert_eq!(expr, expected, "source {src:?} parsed unexpectedly");
}

#[rstest]
#[case::if_else(
    "{ if c { a } else { b }; d }",
    block(
        vec![if_expr(
            var("c"),
            block(vec![], Some(var("a"))),
            Some(block(vec![], Some(var("b"))))
        )],
        Some(var("d"))
    )
)]
#[case::if_without_else(
    "{ if c { a }; d }",
    block(vec![if_expr(var("c"), block(vec![], Some(var("a"))), None)], Some(var("d")))
)]
#[case::else_if(
    "{ if a { x } else if b { y }; z }",
    block(
        vec![if_expr(
            var("a"),
            block(vec![], Some(var("x"))),
            Some(if_expr(var("b"), block(vec![], Some(var("y"))), None))
        )],
        Some(var("z"))
    )
)]
#[case::for_with_block_body(
    "{ for (x in xs) { f(x) }; y }",
    block(
        vec![for_loop(
            "x",
            var("xs"),
            None,
            block(vec![], Some(call("f", vec![var("x")])))
        )],
        Some(var("y"))
    )
)]
#[case::for_with_expression_body(
    "{ for (x in xs) f(x); g() }",
    block(
        vec![for_loop("x", var("xs"), None, call("f", vec![var("x")]))],
        Some(call("g", vec![]))
    )
)]
#[case::return_then_value(
    "{ return x; y }",
    block(vec![return_expr(Some(var("x")))], Some(var("y")))
)]
#[case::assignment_of_if(
    "{ a = if c { x } else { y }; z }",
    block(
        vec![assign(
            var("a"),
            if_expr(
                var("c"),
                block(vec![], Some(var("x"))),
                Some(block(vec![], Some(var("y"))))
            )
        )],
        Some(var("z"))
    )
)]
fn parse_construct_tails_as_separate_items(#[case] src: &str, #[case] expected: Expr) {
    let expr =
        parse_expression(src).unwrap_or_else(|errs| panic!("source {src:?} errors: {errs:?}"));
    assert_eq!(expr, expected, "source {src:?} swallowed its separator");
}

#[rstest]
#[case::parentheses("{ (a; b); c }", block(vec![Expr::Group(Box::new(seq(var("a"), var("b"))))], Some(var("c"))))]
#[case::call_arguments(
    "{ f(a; b) }",
    block(vec![], Some(call("f", vec![seq(var("a"), var("b"))])))
)]
#[case::vector_items(
    "{ [a; b] }",
    block(vec![], Some(vec_lit(vec![seq(var("a"), var("b"))])))
)]
#[case::match_arm_body(
    "{ match (x) { _ -> a; b } }",
    block(
        vec![],
        Some(match_expr(var("x"), vec![match_arm("_", seq(var("a"), var("b")))]))
    )
)]
fn nested_delimiters_still_allow_sequence(#[case] src: &str, #[case] expected: Expr) {
    let expr =
        parse_expression(src).unwrap_or_else(|errs| panic!("source {src:?} errors: {errs:?}"));
    assert_eq!(
        expr, expected,
        "source {src:?} lost sequencing inside a delimiter"
    );
}

#[rstest]
#[case::comma("{ a, b }", BLOCK_ITEM_SEPARATOR_ERROR, 3, 4)]
#[case::comma_after_statement("{ a; b, c }", BLOCK_ITEM_SEPARATOR_ERROR, 6, 7)]
#[case::missing_separator("{ a; b c }", BLOCK_ITEM_SEPARATOR_ERROR, 7, 8)]
#[case::lone_separator("{ ; }", EMPTY_BLOCK_ITEM_ERROR, 2, 3)]
#[case::double_separator("{ a;; b }", EMPTY_BLOCK_ITEM_ERROR, 4, 5)]
#[case::unclosed("{ a", UNCLOSED_BLOCK_ERROR, 3, 3)]
#[case::unclosed_after_separator("{ a;", UNCLOSED_BLOCK_ERROR, 4, 4)]
#[case::brace_map_shape("{a: 1, b: 2}", BLOCK_ITEM_SEPARATOR_ERROR, 5, 6)]
fn rejects_malformed_blocks(
    #[case] src: &str,
    #[case] message: &str,
    #[case] start: usize,
    #[case] end: usize,
) {
    let errors = parse_expression(src).expect_err("malformed block should be rejected");
    assert_parse_error(&errors, message, start, end);
}

#[test]
fn rejects_var_statement_until_roadmap_2_6_12() {
    let errors = parse_expression("{ var x = 1; x }").expect_err("`var` is not an expression yet");
    assert_parse_error(&errors, "unexpected token: K_VAR", 2, 5);
}

#[test]
fn block_expr_from_items_rejects_empty_trailing_separator() {
    assert!(
        BlockExpr::from_items(Vec::new(), true).is_none(),
        "an empty item list with a trailing ';' has no item to separate"
    );
    assert_eq!(
        BlockExpr::from_items(Vec::new(), false),
        Some(BlockExpr::default()),
        "empty braces without a separator are the empty block"
    );
}

#[rstest]
#[case::empty(block(vec![], None), vec![])]
#[case::value(block(vec![], Some(var("a"))), vec!["a"])]
#[case::statement(block(vec![var("a")], None), vec!["a"])]
#[case::both(block(vec![var("a")], Some(var("b"))), vec!["a", "b"])]
fn block_items_follow_source_order(#[case] expr: Expr, #[case] expected: Vec<&str>) {
    let Expr::Block(parsed) = expr else {
        panic!("fixture should be a block expression")
    };
    let render = |item: &Expr| match item {
        Expr::Variable(name) => name.clone(),
        other => panic!("fixture item should be a variable, got {other:?}"),
    };
    let borrowed: Vec<String> = parsed.items().map(render).collect();
    assert_eq!(
        borrowed, expected,
        "items() should yield statements then the value in source order"
    );
    let consumed: Vec<String> = parsed.into_items().map(|item| render(&item)).collect();
    assert_eq!(
        consumed, expected,
        "into_items() should yield statements then the value in source order"
    );
}

#[rstest]
#[case::empty(block(vec![], None), false)]
#[case::value(block(vec![], Some(var("a"))), false)]
#[case::statement(block(vec![var("a")], None), true)]
#[case::both(block(vec![var("a")], Some(var("b"))), false)]
fn block_ends_with_semicolon_only_for_a_unit_value(#[case] expr: Expr, #[case] expected: bool) {
    let Expr::Block(parsed) = expr else {
        panic!("fixture should be a block expression")
    };
    assert_eq!(
        parsed.ends_with_semicolon(),
        expected,
        "ends_with_semicolon() should be true exactly for a non-empty unit block"
    );
}

#[rstest]
#[case::empty(block(vec![], None), "(block)")]
#[case::value(block(vec![], Some(var("a"))), "(block (value a))")]
#[case::statement(block(vec![var("a")], None), "(block (stmts a))")]
#[case::both(block(vec![var("a")], Some(var("b"))), "(block (stmts a) (value b))")]
fn block_renders_as_sexpr(#[case] expr: Expr, #[case] expected: &str) {
    assert_eq!(expr.to_sexpr(), expected);
}
