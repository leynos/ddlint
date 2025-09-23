//! Tests for the Pratt expression parser.

use crate::parser::ast::{BinaryOp, Expr, UnaryOp};
use crate::parser::expression::parse_expression;
use crate::test_util::{
    assert_parse_error, assert_unclosed_delimiter_error, bit_slice, call, call_expr, closure,
    field, field_access, if_expr, lit_bool, lit_num, lit_str, method_call, struct_expr, tuple,
    tuple_index, var,
};
use rstest::rstest;

#[rstest]
#[case("1 + 2 * 3", Expr::Binary { op: BinaryOp::Add, lhs: Box::new(lit_num("1")), rhs: Box::new(Expr::Binary { op: BinaryOp::Mul, lhs: Box::new(lit_num("2")), rhs: Box::new(lit_num("3")) }) })]
#[case("8 - 4 - 2", Expr::Binary { op: BinaryOp::Sub, lhs: Box::new(Expr::Binary { op: BinaryOp::Sub, lhs: Box::new(lit_num("8")), rhs: Box::new(lit_num("4")) }), rhs: Box::new(lit_num("2")) })]
#[case("-5 + 2", Expr::Binary { op: BinaryOp::Add, lhs: Box::new(Expr::Unary { op: UnaryOp::Neg, expr: Box::new(lit_num("5")) }), rhs: Box::new(lit_num("2")) })]
#[case("-(5 + 2)", Expr::Unary { op: UnaryOp::Neg, expr: Box::new(Expr::Group(Box::new(Expr::Binary { op: BinaryOp::Add, lhs: Box::new(lit_num("5")), rhs: Box::new(lit_num("2")) }))) })]
#[case("x", var("x"))]
#[case("foo()", call("foo", vec![]))]
#[case("add(x, 1)", call("add", vec![var("x"), lit_num("1")]))]
#[case("(1, 2)", tuple(vec![lit_num("1"), lit_num("2")]))]
#[case("(1, 2, 3)", tuple(vec![lit_num("1"), lit_num("2"), lit_num("3")]))]
#[case("(1,)", tuple(vec![lit_num("1")]))]
#[case("()", tuple(vec![]))]
#[case("(1)", Expr::Group(Box::new(lit_num("1"))))]
#[case("Point { x: 1, y: 2 }", struct_expr("Point", vec![field("x", lit_num("1")), field("y", lit_num("2"))]))]
#[case("Point {}", struct_expr("Point", vec![]))]
#[case(
    "Point { x: 1, y: 2, }",
    struct_expr("Point", vec![field("x", lit_num("1")), field("y", lit_num("2"))]),
)]
#[case("|x, y| x + y", closure(vec!["x", "y"], Expr::Binary { op: BinaryOp::Add, lhs: Box::new(var("x")), rhs: Box::new(var("y")) }))]
#[case("|| 1", closure(std::iter::empty::<&str>(), lit_num("1")))]
#[case("|x,| x", closure(vec!["x"], var("x")))]
#[case("Point { pair: (1, 2) }", struct_expr("Point", vec![field("pair", tuple(vec![lit_num("1"), lit_num("2")]))]))]
#[case(
    "(|| 1, |x| x)",
    tuple(vec![
        closure(Vec::<&str>::new(), lit_num("1")),
        closure(vec!["x"], var("x")),
    ]),
)]
#[case("|x| Point { x: x }", closure(vec!["x"], struct_expr("Point", vec![field("x", var("x"))])))]
#[case("(f)(x)", call_expr(Expr::Group(Box::new(var("f"))), vec![var("x")]))]
#[case("foo.bar(x)", method_call(var("foo"), "bar", vec![var("x")]))]
#[case("foo.bar(x).baz", field_access(
    method_call(var("foo"), "bar", vec![var("x")]),
    "baz"
))]
#[case("foo.bar", field_access(var("foo"), "bar"))]
#[case("foo.bar.baz(x)", method_call(
    field_access(var("foo"), "bar"),
    "baz",
    vec![var("x")]
))]
#[case("foo.bar.baz().qux", field_access(
    method_call(field_access(var("foo"), "bar"), "baz", vec![]),
    "qux"
))]
#[case("foo.bar().baz(x)", method_call(
    method_call(var("foo"), "bar", vec![]),
    "baz",
    vec![var("x")]
))]
#[case("foo.bar.baz", field_access(field_access(var("foo"), "bar"), "baz"))]
#[case("foo.bar().baz.qux(x)", method_call(
    field_access(
        method_call(var("foo"), "bar", vec![]),
        "baz"
    ),
    "qux",
    vec![var("x")]
))]
#[case("e[1,0]", bit_slice(var("e"), lit_num("1"), lit_num("0")))]
#[case("t.0", tuple_index(var("t"), "0"))]
#[case("x: T", Expr::Binary { op: BinaryOp::Ascribe, lhs: Box::new(var("x")), rhs: Box::new(var("T")) })]
#[case("x as T", Expr::Binary { op: BinaryOp::Cast, lhs: Box::new(var("x")), rhs: Box::new(var("T")) })]
#[case("a = b = c", Expr::Binary { op: BinaryOp::Assign, lhs: Box::new(var("a")), rhs: Box::new(Expr::Binary { op: BinaryOp::Assign, lhs: Box::new(var("b")), rhs: Box::new(var("c")) }) })]
#[case("a = b; c", Expr::Binary { op: BinaryOp::Seq, lhs: Box::new(Expr::Binary { op: BinaryOp::Assign, lhs: Box::new(var("a")), rhs: Box::new(var("b")) }), rhs: Box::new(var("c")) })]
#[case("a and b => c or d", Expr::Binary { op: BinaryOp::Imply, lhs: Box::new(Expr::Binary { op: BinaryOp::And, lhs: Box::new(var("a")), rhs: Box::new(var("b")) }), rhs: Box::new(Expr::Binary { op: BinaryOp::Or, lhs: Box::new(var("c")), rhs: Box::new(var("d")) }) })]
#[case("a or b and c", Expr::Binary { op: BinaryOp::Or, lhs: Box::new(var("a")), rhs: Box::new(Expr::Binary { op: BinaryOp::And, lhs: Box::new(var("b")), rhs: Box::new(var("c")) }) })]
#[case("a and b = c", Expr::Binary { op: BinaryOp::Assign, lhs: Box::new(Expr::Binary { op: BinaryOp::And, lhs: Box::new(var("a")), rhs: Box::new(var("b")) }), rhs: Box::new(var("c")) })]
#[case("a or b = c", Expr::Binary { op: BinaryOp::Assign, lhs: Box::new(Expr::Binary { op: BinaryOp::Or, lhs: Box::new(var("a")), rhs: Box::new(var("b")) }), rhs: Box::new(var("c")) })]
#[case("a + b: T", Expr::Binary { op: BinaryOp::Ascribe, lhs: Box::new(Expr::Binary { op: BinaryOp::Add, lhs: Box::new(var("a")), rhs: Box::new(var("b")) }), rhs: Box::new(var("T")) })]
#[case("x: T + y", Expr::Binary { op: BinaryOp::Add, lhs: Box::new(Expr::Binary { op: BinaryOp::Ascribe, lhs: Box::new(var("x")), rhs: Box::new(var("T")) }), rhs: Box::new(var("y")) })]
#[case("x: T = y", Expr::Binary { op: BinaryOp::Assign, lhs: Box::new(Expr::Binary { op: BinaryOp::Ascribe, lhs: Box::new(var("x")), rhs: Box::new(var("T")) }), rhs: Box::new(var("y")) })]
#[case("x as T + y", Expr::Binary { op: BinaryOp::Add, lhs: Box::new(Expr::Binary { op: BinaryOp::Cast, lhs: Box::new(var("x")), rhs: Box::new(var("T")) }), rhs: Box::new(var("y")) })]
#[case("x + y as T", Expr::Binary { op: BinaryOp::Cast, lhs: Box::new(Expr::Binary { op: BinaryOp::Add, lhs: Box::new(var("x")), rhs: Box::new(var("y")) }), rhs: Box::new(var("T")) })]
#[case("x as T = y", Expr::Binary { op: BinaryOp::Assign, lhs: Box::new(Expr::Binary { op: BinaryOp::Cast, lhs: Box::new(var("x")), rhs: Box::new(var("T")) }), rhs: Box::new(var("y")) })]
#[case("not x and y", Expr::Binary { op: BinaryOp::And, lhs: Box::new(Expr::Unary { op: UnaryOp::Not, expr: Box::new(var("x")) }), rhs: Box::new(var("y")) })]
#[case("not (x and y)", Expr::Unary { op: UnaryOp::Not, expr: Box::new(Expr::Group(Box::new(Expr::Binary { op: BinaryOp::And, lhs: Box::new(var("x")), rhs: Box::new(var("y")) }))) })]
#[case("-(x as T)", Expr::Unary { op: UnaryOp::Neg, expr: Box::new(Expr::Group(Box::new(Expr::Binary { op: BinaryOp::Cast, lhs: Box::new(var("x")), rhs: Box::new(var("T")) }))) })]
#[case("-x as T", Expr::Binary { op: BinaryOp::Cast, lhs: Box::new(Expr::Unary { op: UnaryOp::Neg, expr: Box::new(var("x")) }), rhs: Box::new(var("T")) })]
#[case("a => b => c", Expr::Binary { op: BinaryOp::Imply, lhs: Box::new(var("a")), rhs: Box::new(Expr::Binary { op: BinaryOp::Imply, lhs: Box::new(var("b")), rhs: Box::new(var("c")) }) })]
#[case("a => b; c", Expr::Binary { op: BinaryOp::Seq, lhs: Box::new(Expr::Binary { op: BinaryOp::Imply, lhs: Box::new(var("a")), rhs: Box::new(var("b")) }), rhs: Box::new(var("c")) })]
#[case("(a and b) = c", Expr::Binary { op: BinaryOp::Assign, lhs: Box::new(Expr::Group(Box::new(Expr::Binary { op: BinaryOp::And, lhs: Box::new(var("a")), rhs: Box::new(var("b")) }))), rhs: Box::new(var("c")) })]
#[case("a and (b = c)", Expr::Binary { op: BinaryOp::And, lhs: Box::new(var("a")), rhs: Box::new(Expr::Group(Box::new(Expr::Binary { op: BinaryOp::Assign, lhs: Box::new(var("b")), rhs: Box::new(var("c")) }))) })]
#[case("(x: T) + y", Expr::Binary { op: BinaryOp::Add, lhs: Box::new(Expr::Group(Box::new(Expr::Binary { op: BinaryOp::Ascribe, lhs: Box::new(var("x")), rhs: Box::new(var("T")) }))), rhs: Box::new(var("y")) })]
#[case("a + (b: T)", Expr::Binary { op: BinaryOp::Add, lhs: Box::new(var("a")), rhs: Box::new(Expr::Group(Box::new(Expr::Binary { op: BinaryOp::Ascribe, lhs: Box::new(var("b")), rhs: Box::new(var("T")) }))) })]
#[case("a + (x as T)", Expr::Binary { op: BinaryOp::Add, lhs: Box::new(var("a")), rhs: Box::new(Expr::Group(Box::new(Expr::Binary { op: BinaryOp::Cast, lhs: Box::new(var("x")), rhs: Box::new(var("T")) }))) })]
#[case("(a => b); c", Expr::Binary { op: BinaryOp::Seq, lhs: Box::new(Expr::Group(Box::new(Expr::Binary { op: BinaryOp::Imply, lhs: Box::new(var("a")), rhs: Box::new(var("b")) }))), rhs: Box::new(var("c")) })]
#[case("a => (b; c)", Expr::Binary { op: BinaryOp::Imply, lhs: Box::new(var("a")), rhs: Box::new(Expr::Group(Box::new(Expr::Binary { op: BinaryOp::Seq, lhs: Box::new(var("b")), rhs: Box::new(var("c")) }))) })]
#[case(
    "if x { y } else { z }",
    if_expr(
        var("x"),
        Expr::Group(Box::new(var("y"))),
        Some(Expr::Group(Box::new(var("z")))),
    )
)]
#[case(
    "if (Point { x: 1 }) { y } else { z }",
    if_expr(
        Expr::Group(Box::new(struct_expr(
            "Point",
            vec![field("x", lit_num("1"))],
        ))),
        Expr::Group(Box::new(var("y"))),
        Some(Expr::Group(Box::new(var("z")))),
    )
)]
#[case(
    "if flag { Point { x: 1 } } else { z }",
    if_expr(
        var("flag"),
        Expr::Group(Box::new(struct_expr(
            "Point",
            vec![field("x", lit_num("1"))],
        ))),
        Some(Expr::Group(Box::new(var("z")))),
    )
)]
#[case(
    "if cond { Outer { inner: Inner { a: 1, b: 2 }, flag: true } } else { fallback }",
    if_expr(
        var("cond"),
        Expr::Group(Box::new(struct_expr(
            "Outer",
            vec![
                field(
                    "inner",
                    struct_expr(
                        "Inner",
                        vec![
                            field("a", lit_num("1")),
                            field("b", lit_num("2")),
                        ],
                    ),
                ),
                field("flag", lit_bool(true)),
            ],
        ))),
        Some(Expr::Group(Box::new(var("fallback")))),
    )
)]
#[case(
    "if ok { S { f: T { x: 1, y: U { z: 2 } }, g: 3 } } else { alt }",
    if_expr(
        var("ok"),
        Expr::Group(Box::new(struct_expr(
            "S",
            vec![
                field(
                    "f",
                    struct_expr(
                        "T",
                        vec![
                            field("x", lit_num("1")),
                            field(
                                "y",
                                struct_expr(
                                    "U",
                                    vec![field("z", lit_num("2"))],
                                ),
                            ),
                        ],
                    ),
                ),
                field("g", lit_num("3")),
            ],
        ))),
        Some(Expr::Group(Box::new(var("alt")))),
    )
)]
#[case(
    "if a and b { x } else { y }",
    if_expr(
        Expr::Binary {
            op: BinaryOp::And,
            lhs: Box::new(var("a")),
            rhs: Box::new(var("b")),
        },
        Expr::Group(Box::new(var("x"))),
        Some(Expr::Group(Box::new(var("y")))),
    )
)]
#[case("if flag value", if_expr(var("flag"), var("value"), None))]
#[case(
    "if cond { left } else if other { mid } else { right }",
    if_expr(
        var("cond"),
        Expr::Group(Box::new(var("left"))),
        Some(if_expr(
            var("other"),
            Expr::Group(Box::new(var("mid"))),
            Some(Expr::Group(Box::new(var("right")))),
        )),
    )
)]
fn parses_expressions(#[case] src: &str, #[case] expected: Expr) {
    let expr =
        parse_expression(src).unwrap_or_else(|errs| panic!("source {src:?} errors: {errs:?}"));
    assert_eq!(expr, expected);
}

#[rstest]
#[case("\"hi\"", lit_str("hi"))]
#[case("true", lit_bool(true))]
#[case("false", lit_bool(false))]
#[case("42", lit_num("42"))]
fn parses_literals(#[case] src: &str, #[case] expected: Expr) {
    let expr =
        parse_expression(src).unwrap_or_else(|errs| panic!("source {src:?} errors: {errs:?}"));
    assert_eq!(expr, expected);
}

#[test]
fn rejects_expression_exceeding_max_depth() {
    let depth = 257;
    let source = format!("{}0{}", "(".repeat(depth), ")".repeat(depth));
    let Err(errors) = parse_expression(&source) else {
        panic!("expected depth error");
    };
    assert_parse_error(&errors, "expression nesting too deep", depth - 1, depth);
}

#[test]
fn reports_struct_literal_disallowed_in_if_condition() {
    let Err(errors) = parse_expression("if Point { x: 1 } else { y }") else {
        panic!("expected parse error");
    };
    assert_parse_error(
        &errors,
        "struct literal syntax is not allowed in this context",
        3,
        8,
    );
}

#[rstest]
#[case::addition_missing_rhs("1 +", 1)]
#[case::unclosed_parenthesis("(1 + 2", 1)]
#[case::unexpected_question_mark("1 ? 2", 1)]
#[case::trailing_colon("x :", 1)]
#[case::incomplete_as_cast("x as", 1)]
#[case::assignment_missing_rhs("x =", 1)]
#[case::extraneous_semicolon("x ;", 1)]
#[case::unexpected_fat_arrow("x =>", 1)]
#[case::empty_input("", 1)]
#[case::bit_slice_missing_comma("e[1 0]", 1)]
#[case::bit_slice_missing_lhs("e[,0]", 1)]
#[case::bit_slice_missing_rhs("e[1,]", 1)]
#[case::bit_slice_extra_comma("e[1,,0]", 1)]
#[case::tuple_index_missing_digits("t.", 1)]
#[case::tuple_index_negative("t.-1", 1)]
#[case::tuple_index_plus("t.+1", 1)]
#[case::tuple_index_double_dot("t..0", 1)]
#[case::if_missing_then("if cond else value", 1)]
#[case::if_missing_else_expr("if cond value else", 1)]
#[case::if_missing_condition("if", 1)]
fn reports_errors(#[case] src: &str, #[case] min_errs: usize) {
    match parse_expression(src) {
        Ok(_) => panic!("expected parse error"),
        Err(errs) => assert!(errs.len() >= min_errs),
    }
}

fn reports_exactly_one_error(src: &str) {
    match parse_expression(src) {
        Ok(_) => panic!("expected parse error"),
        Err(errs) => assert_eq!(errs.len(), 1, "expected exactly one error, got {errs:?}"),
    }
}

#[test]
fn rejects_chained_type_ops_with_single_diag() {
    for src in ["x: T: U", "x as T as U", "x: T as U", "x as T: U"] {
        reports_exactly_one_error(src);
    }
}

#[rstest]
#[case::trailing_dot("foo.", "expected identifier or tuple index after '.'", 4, 4, false)]
#[case::bit_slice_missing_comma("e[1]", "expected comma", 3, 4, false)]
#[case::bit_slice_unclosed("e[1,0", "expected right bracket", 5, 5, true)]
fn postfix_expression_errors(
    #[case] src: &str,
    #[case] msg: &str,
    #[case] start: usize,
    #[case] end: usize,
    #[case] unclosed: bool,
) {
    let Err(errors) = parse_expression(src) else {
        panic!("expected error");
    };
    if unclosed {
        assert_unclosed_delimiter_error(&errors, "expected right bracket", start, end);
    } else {
        assert_parse_error(&errors, msg, start, end);
    }
}
