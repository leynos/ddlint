//! Generators and renderers shared by the block-expression properties.
//!
//! Item sources are limited to forms that already parse as one block item, so
//! a generated block tests ordering and separator handling rather than
//! unrelated expression coverage. The expected AST for each source lives in
//! [`expected_item`], keeping the generator and its oracle together.

use proptest::prelude::*;

use crate::{
    parser::ast::{BinaryOp, Expr},
    test_util::{
        block,
        call,
        closure,
        for_loop,
        if_expr,
        lit_num,
        map_entry,
        map_lit,
        return_expr,
        tuple,
        var,
        vec_lit,
    },
};

/// Item sources that each parse as exactly one block item.
pub(super) const ITEM_SOURCES: &[&str] = &[
    "a",
    "b",
    "1",
    "f()",
    "x + 1",
    "[a, b]",
    "[k -> v]",
    "(a, b)",
    "(a; b)",
    "{ a }",
    "if c { a } else { b }",
    "for (x in xs) f(x)",
    "return x",
    "|x| x",
];

/// Tokens used by the panic-freedom property, including every block delimiter
/// and separator.
const SOUP_TOKENS: &[&str] = &[
    "{", "}", ";", "a", "b", "1", ",", ":", "->", "(", ")", "[", "]", "if", "else", "c",
];

/// Strategy producing one item source from [`ITEM_SOURCES`].
pub(super) fn item_source() -> impl Strategy<Value = &'static str> {
    prop::sample::select(ITEM_SOURCES)
}

/// Strategy producing an arbitrary, possibly malformed block source.
///
/// Every draw starts with `{` so it reaches block parsing; a `}` appears only
/// when the token soup supplies one, exercising both complete and unterminated
/// blocks.
pub(super) fn brace_token_soup() -> impl Strategy<Value = String> {
    prop::collection::vec(prop::sample::select(SOUP_TOKENS), 0..12).prop_map(|tokens| {
        let mut source = String::from("{ ");
        source.push_str(&tokens.join(" "));
        source
    })
}

/// Render a block source from item sources and a trailing-separator flag.
pub(super) fn render_block(items: &[&str], ends_with_semicolon: bool) -> String {
    if items.is_empty() {
        return "{}".to_owned();
    }
    let mut body = items.join("; ");
    if ends_with_semicolon {
        body.push(';');
    }
    format!("{{ {body} }}")
}

/// Return the AST that the parser must build for one item source.
///
/// Returns `None` for a source outside [`ITEM_SOURCES`], letting a property
/// report the mismatch instead of panicking inside the fixture.
pub(super) fn expected_item(src: &str) -> Option<Expr> {
    let expr = match src {
        "a" => var("a"),
        "b" => var("b"),
        "1" => lit_num("1"),
        "f()" => call("f", vec![]),
        "x + 1" => Expr::Binary {
            op: BinaryOp::Add,
            lhs: Box::new(var("x")),
            rhs: Box::new(lit_num("1")),
        },
        "[a, b]" => vec_lit(vec![var("a"), var("b")]),
        "[k -> v]" => map_lit(vec![map_entry(var("k"), var("v"))]),
        "(a, b)" => tuple(vec![var("a"), var("b")]),
        "(a; b)" => Expr::Group(Box::new(super::seq(var("a"), var("b")))),
        "{ a }" => block(vec![], Some(var("a"))),
        "if c { a } else { b }" => if_expr(
            var("c"),
            block(vec![], Some(var("a"))),
            Some(block(vec![], Some(var("b")))),
        ),
        "for (x in xs) f(x)" => for_loop("x", var("xs"), None, call("f", vec![var("x")])),
        "return x" => return_expr(Some(var("x"))),
        "|x| x" => closure(["x"], var("x")),
        _ => return None,
    };
    Some(expr)
}
