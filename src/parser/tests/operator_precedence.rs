//! Tests for operator precedence in the Pratt expression parser.

use crate::parser::ast::{BinaryOp, Expr, UnaryOp};
use crate::parser::expression::parse_expression;
use crate::test_util::{lit_num, var};
use rstest::rstest;

#[rstest]
#[case("1 + 2 * 3", Expr::Binary { op: BinaryOp::Add, lhs: Box::new(lit_num("1")), rhs: Box::new(Expr::Binary { op: BinaryOp::Mul, lhs: Box::new(lit_num("2")), rhs: Box::new(lit_num("3")) }) })]
#[case("8 - 4 - 2", Expr::Binary { op: BinaryOp::Sub, lhs: Box::new(Expr::Binary { op: BinaryOp::Sub, lhs: Box::new(lit_num("8")), rhs: Box::new(lit_num("4")) }), rhs: Box::new(lit_num("2")) })]
#[case("-5 + 2", Expr::Binary { op: BinaryOp::Add, lhs: Box::new(Expr::Unary { op: UnaryOp::Neg, expr: Box::new(lit_num("5")) }), rhs: Box::new(lit_num("2")) })]
#[case("-(5 + 2)", Expr::Unary { op: UnaryOp::Neg, expr: Box::new(Expr::Group(Box::new(Expr::Binary { op: BinaryOp::Add, lhs: Box::new(lit_num("5")), rhs: Box::new(lit_num("2")) }))) })]
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
// Concatenation operator (++) tests
#[case("a ++ b", Expr::Binary { op: BinaryOp::Concat, lhs: Box::new(var("a")), rhs: Box::new(var("b")) })]
#[case("a + b ++ c", Expr::Binary { op: BinaryOp::Concat, lhs: Box::new(Expr::Binary { op: BinaryOp::Add, lhs: Box::new(var("a")), rhs: Box::new(var("b")) }), rhs: Box::new(var("c")) })]
#[case("a ++ b + c", Expr::Binary { op: BinaryOp::Add, lhs: Box::new(Expr::Binary { op: BinaryOp::Concat, lhs: Box::new(var("a")), rhs: Box::new(var("b")) }), rhs: Box::new(var("c")) })]
#[case("a ++ b ++ c", Expr::Binary { op: BinaryOp::Concat, lhs: Box::new(Expr::Binary { op: BinaryOp::Concat, lhs: Box::new(var("a")), rhs: Box::new(var("b")) }), rhs: Box::new(var("c")) })]
// Concatenation precedence with shifts and bitwise operators
#[case("a ++ b << c", Expr::Binary { op: BinaryOp::Shl, lhs: Box::new(Expr::Binary { op: BinaryOp::Concat, lhs: Box::new(var("a")), rhs: Box::new(var("b")) }), rhs: Box::new(var("c")) })]
#[case("a << b ++ c", Expr::Binary { op: BinaryOp::Shl, lhs: Box::new(var("a")), rhs: Box::new(Expr::Binary { op: BinaryOp::Concat, lhs: Box::new(var("b")), rhs: Box::new(var("c")) }) })]
#[case("a ++ b & c", Expr::Binary { op: BinaryOp::BitAnd, lhs: Box::new(Expr::Binary { op: BinaryOp::Concat, lhs: Box::new(var("a")), rhs: Box::new(var("b")) }), rhs: Box::new(var("c")) })]
#[case("a ++ b ^ c", Expr::Binary { op: BinaryOp::BitXor, lhs: Box::new(Expr::Binary { op: BinaryOp::Concat, lhs: Box::new(var("a")), rhs: Box::new(var("b")) }), rhs: Box::new(var("c")) })]
#[case("a ++ b | c", Expr::Binary { op: BinaryOp::BitOr, lhs: Box::new(Expr::Binary { op: BinaryOp::Concat, lhs: Box::new(var("a")), rhs: Box::new(var("b")) }), rhs: Box::new(var("c")) })]
// Shift operator tests
#[case("a << b", Expr::Binary { op: BinaryOp::Shl, lhs: Box::new(var("a")), rhs: Box::new(var("b")) })]
#[case("a >> b", Expr::Binary { op: BinaryOp::Shr, lhs: Box::new(var("a")), rhs: Box::new(var("b")) })]
#[case("a << b >> c", Expr::Binary { op: BinaryOp::Shr, lhs: Box::new(Expr::Binary { op: BinaryOp::Shl, lhs: Box::new(var("a")), rhs: Box::new(var("b")) }), rhs: Box::new(var("c")) })]
#[case("a + b << c", Expr::Binary { op: BinaryOp::Shl, lhs: Box::new(Expr::Binary { op: BinaryOp::Add, lhs: Box::new(var("a")), rhs: Box::new(var("b")) }), rhs: Box::new(var("c")) })]
// Shift associativity tests (left-associative)
#[case("a << b << c", Expr::Binary { op: BinaryOp::Shl, lhs: Box::new(Expr::Binary { op: BinaryOp::Shl, lhs: Box::new(var("a")), rhs: Box::new(var("b")) }), rhs: Box::new(var("c")) })]
#[case("a >> b >> c", Expr::Binary { op: BinaryOp::Shr, lhs: Box::new(Expr::Binary { op: BinaryOp::Shr, lhs: Box::new(var("a")), rhs: Box::new(var("b")) }), rhs: Box::new(var("c")) })]
// Bitwise AND operator tests
#[case("a & b", Expr::Binary { op: BinaryOp::BitAnd, lhs: Box::new(var("a")), rhs: Box::new(var("b")) })]
#[case("a << b & c", Expr::Binary { op: BinaryOp::BitAnd, lhs: Box::new(Expr::Binary { op: BinaryOp::Shl, lhs: Box::new(var("a")), rhs: Box::new(var("b")) }), rhs: Box::new(var("c")) })]
// Bitwise AND associativity (left-associative)
#[case("a & b & c", Expr::Binary { op: BinaryOp::BitAnd, lhs: Box::new(Expr::Binary { op: BinaryOp::BitAnd, lhs: Box::new(var("a")), rhs: Box::new(var("b")) }), rhs: Box::new(var("c")) })]
// Bitwise XOR operator tests
#[case("a ^ b", Expr::Binary { op: BinaryOp::BitXor, lhs: Box::new(var("a")), rhs: Box::new(var("b")) })]
#[case("a & b ^ c", Expr::Binary { op: BinaryOp::BitXor, lhs: Box::new(Expr::Binary { op: BinaryOp::BitAnd, lhs: Box::new(var("a")), rhs: Box::new(var("b")) }), rhs: Box::new(var("c")) })]
// Bitwise XOR associativity (left-associative)
#[case("a ^ b ^ c", Expr::Binary { op: BinaryOp::BitXor, lhs: Box::new(Expr::Binary { op: BinaryOp::BitXor, lhs: Box::new(var("a")), rhs: Box::new(var("b")) }), rhs: Box::new(var("c")) })]
// Bitwise OR operator tests
#[case("a | b", Expr::Binary { op: BinaryOp::BitOr, lhs: Box::new(var("a")), rhs: Box::new(var("b")) })]
#[case("a ^ b | c", Expr::Binary { op: BinaryOp::BitOr, lhs: Box::new(Expr::Binary { op: BinaryOp::BitXor, lhs: Box::new(var("a")), rhs: Box::new(var("b")) }), rhs: Box::new(var("c")) })]
// Bitwise OR associativity (left-associative)
#[case("a | b | c", Expr::Binary { op: BinaryOp::BitOr, lhs: Box::new(Expr::Binary { op: BinaryOp::BitOr, lhs: Box::new(var("a")), rhs: Box::new(var("b")) }), rhs: Box::new(var("c")) })]
// Bitwise hierarchy: & binds tighter than ^ which binds tighter than |
#[case("a & b ^ c | d", Expr::Binary { op: BinaryOp::BitOr, lhs: Box::new(Expr::Binary { op: BinaryOp::BitXor, lhs: Box::new(Expr::Binary { op: BinaryOp::BitAnd, lhs: Box::new(var("a")), rhs: Box::new(var("b")) }), rhs: Box::new(var("c")) }), rhs: Box::new(var("d")) })]
// Comparison operators
#[case("a < b", Expr::Binary { op: BinaryOp::Lt, lhs: Box::new(var("a")), rhs: Box::new(var("b")) })]
#[case("a <= b", Expr::Binary { op: BinaryOp::Lte, lhs: Box::new(var("a")), rhs: Box::new(var("b")) })]
#[case("a > b", Expr::Binary { op: BinaryOp::Gt, lhs: Box::new(var("a")), rhs: Box::new(var("b")) })]
#[case("a >= b", Expr::Binary { op: BinaryOp::Gte, lhs: Box::new(var("a")), rhs: Box::new(var("b")) })]
// Equality operators share precedence with comparisons (left-associative)
#[case("a == b < c", Expr::Binary { op: BinaryOp::Lt, lhs: Box::new(Expr::Binary { op: BinaryOp::Eq, lhs: Box::new(var("a")), rhs: Box::new(var("b")) }), rhs: Box::new(var("c")) })]
#[case("a < b == c", Expr::Binary { op: BinaryOp::Eq, lhs: Box::new(Expr::Binary { op: BinaryOp::Lt, lhs: Box::new(var("a")), rhs: Box::new(var("b")) }), rhs: Box::new(var("c")) })]
// Comparisons bind looser than bitwise OR
#[case("a | b < c", Expr::Binary { op: BinaryOp::Lt, lhs: Box::new(Expr::Binary { op: BinaryOp::BitOr, lhs: Box::new(var("a")), rhs: Box::new(var("b")) }), rhs: Box::new(var("c")) })]
// Comparisons bind tighter than logical AND
#[case("a < b and c", Expr::Binary { op: BinaryOp::And, lhs: Box::new(Expr::Binary { op: BinaryOp::Lt, lhs: Box::new(var("a")), rhs: Box::new(var("b")) }), rhs: Box::new(var("c")) })]
// Comparisons with shifts (shifts bind tighter)
#[case("a << b < c", Expr::Binary { op: BinaryOp::Lt, lhs: Box::new(Expr::Binary { op: BinaryOp::Shl, lhs: Box::new(var("a")), rhs: Box::new(var("b")) }), rhs: Box::new(var("c")) })]
#[case("a < b >> c", Expr::Binary { op: BinaryOp::Lt, lhs: Box::new(var("a")), rhs: Box::new(Expr::Binary { op: BinaryOp::Shr, lhs: Box::new(var("b")), rhs: Box::new(var("c")) }) })]
// Comparisons with concatenation (++ binds tighter)
#[case("a ++ b < c", Expr::Binary { op: BinaryOp::Lt, lhs: Box::new(Expr::Binary { op: BinaryOp::Concat, lhs: Box::new(var("a")), rhs: Box::new(var("b")) }), rhs: Box::new(var("c")) })]
// Comparisons with bitwise AND, XOR, OR (all bind tighter)
#[case("a & b < c", Expr::Binary { op: BinaryOp::Lt, lhs: Box::new(Expr::Binary { op: BinaryOp::BitAnd, lhs: Box::new(var("a")), rhs: Box::new(var("b")) }), rhs: Box::new(var("c")) })]
#[case("a ^ b < c", Expr::Binary { op: BinaryOp::Lt, lhs: Box::new(Expr::Binary { op: BinaryOp::BitXor, lhs: Box::new(var("a")), rhs: Box::new(var("b")) }), rhs: Box::new(var("c")) })]
// Comparisons with logical OR (comparisons bind tighter)
#[case("a < b or c", Expr::Binary { op: BinaryOp::Or, lhs: Box::new(Expr::Binary { op: BinaryOp::Lt, lhs: Box::new(var("a")), rhs: Box::new(var("b")) }), rhs: Box::new(var("c")) })]
// Multiple comparisons with logical ops
#[case("a < b and c > d", Expr::Binary { op: BinaryOp::And, lhs: Box::new(Expr::Binary { op: BinaryOp::Lt, lhs: Box::new(var("a")), rhs: Box::new(var("b")) }), rhs: Box::new(Expr::Binary { op: BinaryOp::Gt, lhs: Box::new(var("c")), rhs: Box::new(var("d")) }) })]
// Prefix bitwise NOT operator
#[case("~a", Expr::Unary { op: UnaryOp::BitNot, expr: Box::new(var("a")) })]
#[case("~a & b", Expr::Binary { op: BinaryOp::BitAnd, lhs: Box::new(Expr::Unary { op: UnaryOp::BitNot, expr: Box::new(var("a")) }), rhs: Box::new(var("b")) })]
// Prefix reference operator
#[case("&a", Expr::Unary { op: UnaryOp::Ref, expr: Box::new(var("a")) })]
#[case("&a + b", Expr::Binary { op: BinaryOp::Add, lhs: Box::new(Expr::Unary { op: UnaryOp::Ref, expr: Box::new(var("a")) }), rhs: Box::new(var("b")) })]
// Dual-use `&` cases (prefix vs infix)
#[case("&a & b", Expr::Binary { op: BinaryOp::BitAnd, lhs: Box::new(Expr::Unary { op: UnaryOp::Ref, expr: Box::new(var("a")) }), rhs: Box::new(var("b")) })]
#[case("a & &b", Expr::Binary { op: BinaryOp::BitAnd, lhs: Box::new(var("a")), rhs: Box::new(Expr::Unary { op: UnaryOp::Ref, expr: Box::new(var("b")) }) })]
#[case("&a&b", Expr::Binary { op: BinaryOp::BitAnd, lhs: Box::new(Expr::Unary { op: UnaryOp::Ref, expr: Box::new(var("a")) }), rhs: Box::new(var("b")) })]
// => with new operators
#[case("a | b => c", Expr::Binary { op: BinaryOp::Imply, lhs: Box::new(Expr::Binary { op: BinaryOp::BitOr, lhs: Box::new(var("a")), rhs: Box::new(var("b")) }), rhs: Box::new(var("c")) })]
#[case("a ^ b => c ^ d", Expr::Binary { op: BinaryOp::Imply, lhs: Box::new(Expr::Binary { op: BinaryOp::BitXor, lhs: Box::new(var("a")), rhs: Box::new(var("b")) }), rhs: Box::new(Expr::Binary { op: BinaryOp::BitXor, lhs: Box::new(var("c")), rhs: Box::new(var("d")) }) })]
fn parses_operator_precedence(#[case] src: &str, #[case] expected: Expr) {
    let expr =
        parse_expression(src).unwrap_or_else(|errs| panic!("source {src:?} errors: {errs:?}"));
    assert_eq!(expr, expected);
}
