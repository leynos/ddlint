//! Generators and renderers shared by the block-expression properties.
//!
//! Item sources are limited to forms that already parse as one block item, so
//! a generated block tests ordering and separator handling rather than
//! unrelated expression coverage. The expected AST for each source lives in
//! [`expected_item`], keeping the generator and its oracle together.
//!
//! The panic-freedom and differential properties use two further generators:
//! [`structural_brace_sources`] mixes arbitrary token soup with well-formed
//! blocks over the full block alphabet, and [`differential_block_sources`]
//! renders a small block grammar with an injected fault. The second is paired
//! with [`recognize_block`], an independent model of the documented block
//! contract.

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

/// Tokens used by the structural fuzz: every delimiter, separator, operator,
/// keyword, and atom shape the block parser dispatches on.
const STRUCTURAL_TOKENS: &[&str] = &[
    "{", "}", "(", ")", "[", "]", ";", ",", ":", "->", "a", "b", "1", "+", "if", "else", "for",
    "in", "return", "|", "Foo",
];

/// Strategy producing one item source from [`ITEM_SOURCES`].
pub(super) fn item_source() -> impl Strategy<Value = &'static str> {
    prop::sample::select(ITEM_SOURCES)
}

/// Strategy producing structurally arbitrary, possibly malformed block sources.
///
/// Half the draws are arbitrary token soup over [`STRUCTURAL_TOKENS`], which
/// exercises delimiter and keyword combinations no handwritten case would
/// suggest. The other half splice item forms built from the same tokens into a
/// well-formed block, so the property still sees accepted parses often enough
/// to bound both outcomes. Every draw begins with `{` so it reaches block
/// parsing.
pub(super) fn structural_brace_sources() -> impl Strategy<Value = String> {
    prop_oneof![
        1 => prop::collection::vec(prop::sample::select(STRUCTURAL_TOKENS), 0..10)
            .prop_map(|tokens| render_soup(&tokens)),
        1 => (
            prop::collection::vec(prop::sample::select(ITEM_SOURCES), 0..4),
            any::<bool>(),
        )
            .prop_map(|(items, ends_with_semicolon)| render_block(&items, ends_with_semicolon)),
    ]
}

/// Render arbitrary tokens after an opening `{` with no closing brace.
fn render_soup(tokens: &[&str]) -> String {
    let mut source = String::from("{ ");
    source.push_str(&tokens.join(" "));
    source
}

/// Item forms the differential property draws, each exactly one item over the
/// `{ } ; a 1 +` alphabet.
const DIFF_ITEMS: &[&str] = &["a", "1", "a + 1"];

/// A malformation injected into an otherwise well-formed block.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum BlockFault {
    /// No fault: a well-formed block.
    None,
    /// The closing `}` is missing.
    Unclosed,
    /// A separator appears where the first item should start.
    EmptyFirstItem,
    /// A second separator follows the separator after the last item.
    EmptyLastItem,
    /// An operator ends the block's last item.
    TrailingOperator,
}

/// Every fault the differential property injects, in draw order.
static BLOCK_FAULTS: &[BlockFault] = &[
    BlockFault::None,
    BlockFault::Unclosed,
    BlockFault::EmptyFirstItem,
    BlockFault::EmptyLastItem,
    BlockFault::TrailingOperator,
];

/// Strategy producing a block source over `{ } ; a 1 +` with an injected fault.
pub(super) fn differential_block_sources() -> impl Strategy<Value = String> {
    (
        prop::collection::vec(prop::sample::select(DIFF_ITEMS), 0..4),
        any::<bool>(),
        prop::sample::select(BLOCK_FAULTS),
    )
        .prop_map(|(items, ends_with_semicolon, fault)| {
            render_faulty_block(&items, ends_with_semicolon, fault)
        })
}

/// Render a well-formed block, or the same block with one injected fault.
///
/// Unlike [`render_block`], the braces are always separated from the body, so
/// the rendered source is unambiguous even for the empty block.
fn render_faulty_block(items: &[&str], ends_with_semicolon: bool, fault: BlockFault) -> String {
    let plain = items.join("; ");
    match fault {
        BlockFault::None => {
            let separator = if ends_with_semicolon && !items.is_empty() {
                ";"
            } else {
                ""
            };
            format!("{{ {plain}{separator} }}")
        }
        BlockFault::Unclosed => format!("{{ {plain}"),
        BlockFault::EmptyFirstItem => format!("{{ ; {plain} }}"),
        BlockFault::EmptyLastItem => format!("{{ {plain}; ; }}"),
        BlockFault::TrailingOperator => format!("{{ {plain} + }}"),
    }
}

/// Block shape the reference recognizer predicts for an accepted source.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) struct BlockShape {
    /// Number of items followed by `;`.
    pub(super) statements: usize,
    /// Whether a final item supplies the block's value.
    pub(super) has_value: bool,
}

/// Reference model of block structure over `{ } ; a 1 +`.
///
/// This mirrors the documented item rules rather than the parser's
/// implementation: `;` separates items, an item is an atom with optional
/// `+ atom` continuations, an empty item is rejected, and the last item is the
/// value unless a `;` follows it. Returns the predicted shape, or `None` when
/// the source is not a well-formed block.
///
/// The grammar is whitespace-insensitive, so `;` is split off its neighbours
/// before tokenizing: `{a;}` and `{ a ; }` must reach the same state.
pub(super) fn recognize_block(source: &str) -> Option<BlockShape> {
    let spaced = source.replace(';', " ; ");
    let tokens: Vec<&str> = spaced.split_whitespace().collect();
    if tokens.first().copied() != Some("{") {
        return None;
    }
    let mut index = 1;
    let mut items = 0usize;
    let mut expects_item = true;
    loop {
        match tokens.get(index).copied()? {
            "}" => return Some(close_block(items, expects_item)),
            ";" if expects_item => return None,
            ";" => {
                expects_item = true;
                index += 1;
            }
            "a" | "1" if expects_item => {
                index = parse_reference_item(&tokens, index)?;
                items += 1;
                expects_item = false;
            }
            _ => return None,
        }
    }
}

/// Shape of the block whose closing `}` has just been reached.
///
/// A block that is expecting an item after a separator has no value, and its
/// statements are every item parsed. Otherwise the last item is the value, so
/// it is not a statement.
const fn close_block(items: usize, expects_item: bool) -> BlockShape {
    if expects_item {
        BlockShape {
            statements: items,
            has_value: false,
        }
    } else {
        // `expects_item` is false only after an item, so one is available.
        BlockShape {
            statements: items - 1,
            has_value: true,
        }
    }
}

/// Consume one reference item: an atom with zero or more `+ atom` suffixes.
fn parse_reference_item(tokens: &[&str], mut index: usize) -> Option<usize> {
    if !matches!(tokens.get(index).copied(), Some("a" | "1")) {
        return None;
    }
    index += 1;
    while tokens.get(index).copied() == Some("+") {
        index += 1;
        if !matches!(tokens.get(index).copied(), Some("a" | "1")) {
            return None;
        }
        index += 1;
    }
    Some(index)
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
