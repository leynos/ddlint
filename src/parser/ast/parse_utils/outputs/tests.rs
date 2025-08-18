//! Tests for output list parsing utilities.

use super::*;
use crate::parser::ast::AstNode;
use crate::parser::parse;

#[test]
fn collects_output_names() {
    let src = "extern transformer t(a: X): Out1, Out2";
    let parsed = parse(src);
    #[expect(clippy::expect_used, reason = "Using expect for clearer test failures")]
    let tr = parsed
        .root()
        .transformers()
        .first()
        .cloned()
        .expect("transformer missing");
    let names = parse_output_list(tr.syntax().children_with_tokens());
    assert_eq!(names, vec!["Out1".to_string(), "Out2".to_string()]);
}
