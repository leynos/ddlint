//! Tests for CST tree construction.
use super::*;
use crate::parser::ast::Root;
use crate::parser::ast::rule::text_range_to_span;
use crate::parser::span_scanner::parse_tokens;
use crate::test_util::tokenize;
use rowan::{NodeOrToken, SyntaxNode};
use rstest::rstest;

#[test]
fn build_green_tree_round_trip() {
    let src = "import foo::bar;";
    let tokens = tokenize(src);
    let (spans, errors) = parse_tokens(
        &tokens,
        src,
        &crate::parser::observability::NoopParseObserver,
    );
    assert!(errors.is_empty());
    let green = build_green_tree(&tokens, src, &spans);
    let root = crate::parser::ast::Root::from_green(green);
    assert_eq!(root.text(), src);
}

#[cfg(debug_assertions)]
#[test]
#[should_panic(expected = "token span")]
fn build_green_tree_panics_on_oob_token_span() {
    let src = "";
    let spans = match ParsedSpans::builder().build() {
        Ok(spans) => spans,
        Err(err) => panic!("expected valid spans, got: {err}"),
    };
    let tokens = vec![(SyntaxKind::T_IDENT, 0..1)];

    let _ = build_green_tree(&tokens, src, &spans);
}

#[cfg(not(debug_assertions))]
#[test]
fn build_green_tree_skips_oob_token_span_in_release() {
    let src = "import foo::bar;";
    let mut tokens = tokenize(src);
    let (spans, errors) = parse_tokens(
        &tokens,
        src,
        &crate::parser::observability::NoopParseObserver,
    );
    assert!(errors.is_empty());

    tokens.push((SyntaxKind::K_IMPORT, src.len() + 1..src.len() + 2));
    let green = build_green_tree(&tokens, src, &spans);
    let root = crate::parser::ast::Root::from_green(green);
    assert_eq!(root.text(), src);
}

fn collect_kind_spans(root: &SyntaxNode<DdlogLanguage>, kind: SyntaxKind) -> Vec<Span> {
    root.descendants()
        .filter(|node| node.kind() == kind)
        .map(|node| text_range_to_span(node.text_range()))
        .collect()
}

fn assert_span_nodes_match(root: &SyntaxNode<DdlogLanguage>, kind: SyntaxKind, spans: &[Span]) {
    let nodes = collect_kind_spans(root, kind);
    assert_eq!(nodes, spans.to_vec());
}

fn span_for_text(source: &str, text: &str) -> Option<Span> {
    let start = source.find(text)?;
    Some(start..start + text.len())
}

fn spans_for_text(source: &str, fragments: &[&str]) -> Option<Vec<Span>> {
    fragments
        .iter()
        .map(|fragment| span_for_text(source, fragment))
        .collect()
}

fn arrange_tree(
    source: &str,
    rules: &[&str],
    expressions: &[&str],
    attributes: &[&str],
) -> Option<SyntaxNode<DdlogLanguage>> {
    let spans = ParsedSpans::builder()
        .rules(spans_for_text(source, rules)?)
        .expressions(spans_for_text(source, expressions)?)
        .attributes(spans_for_text(source, attributes)?)
        .build()
        .ok()?;
    let tokens = tokenize(source);
    let green = build_green_tree(&tokens, source, &spans);
    Some(Root::from_green(green).syntax().clone())
}

fn node_of_kind(
    root: &SyntaxNode<DdlogLanguage>,
    kind: SyntaxKind,
) -> Option<SyntaxNode<DdlogLanguage>> {
    root.descendants().find(|node| node.kind() == kind)
}

fn has_direct_node_child(
    parent: &SyntaxNode<DdlogLanguage>,
    expected: &SyntaxNode<DdlogLanguage>,
) -> bool {
    parent
        .children_with_tokens()
        .any(|child| matches!(child, NodeOrToken::Node(node) if node == expected.clone()))
}

fn contains_kind(root: &SyntaxNode<DdlogLanguage>, kind: SyntaxKind) -> bool {
    root.descendants().any(|node| node.kind() == kind)
}

#[test]
fn build_green_tree_matches_span_positions() {
    let src = concat!(
        "import foo::bar\n",
        "typedef UserId = u64\n",
        "input relation User(id: UserId, name: string) primary key (id)\n",
        "index Idx_User_name(name: string) on User[name]\n",
        "function greet(name: string): string {\n",
        "}\n",
        "extern transformer normalise(input: User): Normalized\n",
        "apply normalise(User) -> (Normalized)\n",
        "User(id, name) :- name == \"a\", id > 0.\n"
    );
    let tokens = tokenize(src);
    let (spans, errors) = parse_tokens(
        &tokens,
        src,
        &crate::parser::observability::NoopParseObserver,
    );
    assert!(errors.is_empty());
    let green = build_green_tree(&tokens, src, &spans);
    let root = Root::from_green(green);
    let syntax = root.syntax();

    assert_span_nodes_match(syntax, SyntaxKind::N_IMPORT_STMT, spans.imports());
    assert_span_nodes_match(syntax, SyntaxKind::N_TYPE_DEF, spans.typedefs());
    assert_span_nodes_match(syntax, SyntaxKind::N_RELATION_DECL, spans.relations());
    assert_span_nodes_match(syntax, SyntaxKind::N_INDEX, spans.indexes());
    assert_span_nodes_match(syntax, SyntaxKind::N_FUNCTION, spans.functions());
    assert_span_nodes_match(syntax, SyntaxKind::N_TRANSFORMER, spans.transformers());
    assert_span_nodes_match(syntax, SyntaxKind::N_APPLY, spans.applys());
    assert_span_nodes_match(syntax, SyntaxKind::N_RULE, spans.rules());
    assert_span_nodes_match(syntax, SyntaxKind::N_EXPR_NODE, spans.expressions());
}

#[derive(Clone, Copy)]
enum SharedEndShape {
    DifferentStarts,
    IdenticalRuleExpression,
    IdenticalAttributeRuleExpression,
}

#[rstest]
#[case::different_starts(
        "R(x) :- Source(x).",
        &["R(x) :- Source(x)."],
        &["Source(x)."],
        &[],
        SharedEndShape::DifferentStarts
    )]
#[case::identical_rule_expression(
        "R(x) :- Source(x).",
        &["R(x) :- Source(x)."],
        &["R(x) :- Source(x)."],
        &[],
        SharedEndShape::IdenticalRuleExpression
    )]
#[case::identical_attribute_rule_expression(
        "R(x) :- Source(x).",
        &["R(x) :- Source(x)."],
        &["R(x) :- Source(x)."],
        &["R(x) :- Source(x)."],
        SharedEndShape::IdenticalAttributeRuleExpression
    )]
fn shared_end_spans_close_in_cursor_order(
    #[case] source: &str,
    #[case] rules: &[&str],
    #[case] expressions: &[&str],
    #[case] attributes: &[&str],
    #[case] shape: SharedEndShape,
) {
    let root = arrange_tree(source, rules, expressions, attributes)
        .expect("all span fragments should be present and valid");
    assert_eq!(root.kind(), SyntaxKind::N_DATALOG_PROGRAM);
    assert_eq!(root.text().to_string(), source);
    assert!(!contains_kind(&root, SyntaxKind::N_ERROR));

    let rule_spans = spans_for_text(source, rules).expect("rule fragments should be present");
    let expression_spans =
        spans_for_text(source, expressions).expect("expression fragments should be present");
    let attribute_spans =
        spans_for_text(source, attributes).expect("attribute fragments should be present");
    assert_span_nodes_match(&root, SyntaxKind::N_RULE, &rule_spans);
    assert_span_nodes_match(&root, SyntaxKind::N_EXPR_NODE, &expression_spans);
    assert_span_nodes_match(&root, SyntaxKind::N_ATTRIBUTE, &attribute_spans);

    // Rowan's finish_node() carries no kind, so range checks alone cannot
    // establish which parent owns a span.
    match shape {
        SharedEndShape::DifferentStarts => {
            let rule = node_of_kind(&root, SyntaxKind::N_RULE).expect("expected a rule node");
            let expression =
                node_of_kind(&root, SyntaxKind::N_EXPR_NODE).expect("expected an expression node");
            assert!(has_direct_node_child(&root, &rule));
            assert!(has_direct_node_child(&rule, &expression));

            let children = rule.children_with_tokens().collect::<Vec<_>>();
            assert!(
                matches!(children.last(), Some(NodeOrToken::Node(node)) if node.kind() == SyntaxKind::N_EXPR_NODE)
            );
            assert!(
                children
                    .iter()
                    .rev()
                    .skip(1)
                    .all(|child| matches!(child, NodeOrToken::Token(_)))
            );
        }
        SharedEndShape::IdenticalRuleExpression => {
            let rule = node_of_kind(&root, SyntaxKind::N_RULE).expect("expected a rule node");
            let expression =
                node_of_kind(&root, SyntaxKind::N_EXPR_NODE).expect("expected an expression node");
            let children = rule.children_with_tokens().collect::<Vec<_>>();
            assert!(has_direct_node_child(&root, &rule));
            assert_eq!(children.len(), 1);
            assert!(
                matches!(children.first(), Some(NodeOrToken::Node(node)) if node == &expression)
            );
        }
        SharedEndShape::IdenticalAttributeRuleExpression => {
            let attribute =
                node_of_kind(&root, SyntaxKind::N_ATTRIBUTE).expect("expected an attribute node");
            let rule = node_of_kind(&root, SyntaxKind::N_RULE).expect("expected a rule node");
            let expression =
                node_of_kind(&root, SyntaxKind::N_EXPR_NODE).expect("expected an expression node");
            let attribute_children = attribute.children_with_tokens().collect::<Vec<_>>();
            let rule_children = rule.children_with_tokens().collect::<Vec<_>>();
            assert!(has_direct_node_child(&root, &attribute));
            assert!(has_direct_node_child(&attribute, &rule));
            assert!(has_direct_node_child(&rule, &expression));
            assert_eq!(attribute_children.len(), 1);
            assert_eq!(rule_children.len(), 1);
            assert!(
                matches!(attribute_children.first(), Some(NodeOrToken::Node(node)) if node == &rule)
            );
            assert!(
                matches!(rule_children.first(), Some(NodeOrToken::Node(node)) if node == &expression)
            );
        }
    }
}

#[rstest]
#[case::adjacent("R1(x) :- A(x).R2(y) :- B(y).", false)]
#[case::with_intervening_trivia("R1(x) :- A(x). R2(y) :- B(y).", true)]
fn shared_end_span_does_not_capture_the_following_rule(
    #[case] source: &str,
    #[case] has_intervening_trivia: bool,
) {
    let rules = ["R1(x) :- A(x).", "R2(y) :- B(y)."];
    let expressions = ["A(x).", "B(y)."];
    let root = arrange_tree(source, &rules, &expressions, &[])
        .expect("all span fragments should be present and valid");
    assert_eq!(root.kind(), SyntaxKind::N_DATALOG_PROGRAM);
    assert_eq!(root.text().to_string(), source);
    assert!(!contains_kind(&root, SyntaxKind::N_ERROR));

    let rule_spans = spans_for_text(source, &rules).expect("rule fragments should be present");
    let expression_spans =
        spans_for_text(source, &expressions).expect("expression fragments should be present");
    assert_span_nodes_match(&root, SyntaxKind::N_RULE, &rule_spans);
    assert_span_nodes_match(&root, SyntaxKind::N_EXPR_NODE, &expression_spans);

    let children = root.children_with_tokens().collect::<Vec<_>>();
    let direct_rules = children
        .iter()
        .filter_map(|child| match child {
            NodeOrToken::Node(node) if node.kind() == SyntaxKind::N_RULE => Some(node),
            _ => None,
        })
        .collect::<Vec<_>>();
    assert_eq!(direct_rules.len(), 2);
    let first_rule = direct_rules
        .first()
        .expect("expected the first sibling rule");
    let second_rule = direct_rules
        .get(1)
        .expect("expected the second sibling rule");
    assert!(has_direct_node_child(&root, first_rule));
    assert!(has_direct_node_child(&root, second_rule));

    if has_intervening_trivia {
        assert_eq!(children.len(), 3);
        assert!(
            matches!(children.first(), Some(NodeOrToken::Node(node)) if node.kind() == SyntaxKind::N_RULE)
        );
        assert!(matches!(children.get(1), Some(NodeOrToken::Token(token)) if token.text() == " "));
        assert!(
            matches!(children.get(2), Some(NodeOrToken::Node(node)) if node.kind() == SyntaxKind::N_RULE)
        );
    } else {
        assert_eq!(children.len(), 2);
        assert!(children.iter().all(
            |child| matches!(child, NodeOrToken::Node(node) if node.kind() == SyntaxKind::N_RULE)
        ));
    }
}
