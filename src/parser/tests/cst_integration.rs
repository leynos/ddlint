//! CST integration checks for mixed statement programs.

use crate::test_util::assert_no_parse_errors;
use crate::{SyntaxKind, parse};

use super::helpers::{count_nodes_by_kind, pretty_print};

#[test]
fn parse_builds_cst_for_all_top_level_categories() {
    let src = concat!(
        "import foo::bar\n",
        "typedef UserId = u64\n",
        "input relation User(id: UserId, name: string) primary key (id)\n",
        "index Idx_User_name on User(name)\n",
        "function greet(name: string): string {\n",
        "}\n",
        "extern transformer normalise(input: User): Normalized\n",
        "User(id, name) :- name == \"a\", id > 0.\n"
    );
    let parsed = parse(src);
    assert_no_parse_errors(parsed.errors());
    let root = parsed.root();
    let syntax = root.syntax();

    assert_eq!(count_nodes_by_kind(syntax, SyntaxKind::N_IMPORT_STMT), 1);
    assert_eq!(count_nodes_by_kind(syntax, SyntaxKind::N_TYPE_DEF), 1);
    assert_eq!(count_nodes_by_kind(syntax, SyntaxKind::N_RELATION_DECL), 1);
    assert_eq!(count_nodes_by_kind(syntax, SyntaxKind::N_INDEX), 1);
    assert_eq!(count_nodes_by_kind(syntax, SyntaxKind::N_FUNCTION), 1);
    assert_eq!(count_nodes_by_kind(syntax, SyntaxKind::N_TRANSFORMER), 1);
    assert_eq!(count_nodes_by_kind(syntax, SyntaxKind::N_RULE), 1);

    let expr_nodes = count_nodes_by_kind(syntax, SyntaxKind::N_EXPR_NODE);
    assert_eq!(expr_nodes, 2);
    assert_eq!(pretty_print(syntax), src);
}
