//! CST inspection helpers backing the [`Relation`](super::Relation) wrapper.
//!
//! These helpers locate the relation preamble and body delimiter so the wrapper
//! can expose the relation name and bracket element type without duplicating
//! token traversal. They stop before a body delimiter when a malformed node
//! lacks a name, preventing body identifiers from becoming synthetic names.

use crate::{DdlogLanguage, SyntaxKind};

use super::super::parse_utils::is_trivia;
use super::{RelationKind, RelationRole};

/// Parsed metadata from the leading tokens of a relation declaration.
///
/// This captures the role, relation kind, explicit keyword presence, reference
/// marker, and optional relation name before callers inspect the declaration
/// body. The keyword-presence flags distinguish implicit defaults from source
/// text that explicitly names the default role or kind.
#[derive(Debug, Clone)]
pub(super) struct RelationPreamble {
    pub(super) role: RelationRole,
    pub(super) role_keyword_present: bool,
    pub(super) kind: RelationKind,
    pub(super) kind_keyword_present: bool,
    pub(super) is_ref: bool,
    pub(super) name: Option<String>,
}

/// Inspect the leading tokens of a relation declaration.
///
/// Scanning stops at the relation identifier because the preamble ends before
/// the body delimiter. Missing keywords default to an internal relation; the
/// returned keyword-presence flags report whether those defaults came from
/// source keywords or from the parser's implicit fallback.
pub(super) fn inspect_preamble(syntax: &rowan::SyntaxNode<DdlogLanguage>) -> RelationPreamble {
    let mut preamble = RelationPreamble {
        role: RelationRole::Internal,
        role_keyword_present: false,
        kind: RelationKind::Relation,
        kind_keyword_present: false,
        is_ref: false,
        name: None,
    };

    for element in syntax.children_with_tokens() {
        if is_trivia(&element) {
            continue;
        }
        let rowan::NodeOrToken::Token(token) = element else {
            continue;
        };
        match token.kind() {
            SyntaxKind::K_INPUT => {
                preamble.role = RelationRole::Input;
                preamble.role_keyword_present = true;
            }
            SyntaxKind::K_OUTPUT => {
                preamble.role = RelationRole::Output;
                preamble.role_keyword_present = true;
            }
            SyntaxKind::K_RELATION => {
                preamble.kind = RelationKind::Relation;
                preamble.kind_keyword_present = true;
            }
            SyntaxKind::K_STREAM => {
                preamble.kind = RelationKind::Stream;
                preamble.kind_keyword_present = true;
            }
            SyntaxKind::K_MULTISET => {
                preamble.kind = RelationKind::Multiset;
                preamble.kind_keyword_present = true;
            }
            SyntaxKind::T_AMP => preamble.is_ref = true,
            SyntaxKind::T_IDENT => {
                preamble.name = Some(token.text().to_string());
                break;
            }
            SyntaxKind::T_LPAREN | SyntaxKind::T_LBRACKET => break,
            _ => {}
        }
    }

    preamble
}

/// Return the opening delimiter used by the relation body.
///
/// The delimiter is the first parenthesis or bracket token after the relation
/// name: parentheses introduce a field list, while brackets introduce an
/// element type. `None` represents a malformed or incomplete declaration where
/// no body delimiter was found.
pub(super) fn body_open_kind(syntax: &rowan::SyntaxNode<DdlogLanguage>) -> Option<SyntaxKind> {
    elements_after_name(syntax)
        .into_iter()
        .filter(|element| !is_trivia(element))
        .find_map(|element| match element {
            rowan::NodeOrToken::Token(token)
                if matches!(token.kind(), SyntaxKind::T_LPAREN | SyntaxKind::T_LBRACKET) =>
            {
                Some(token.kind())
            }
            _ => None,
        })
}

/// Extract the element type text from a bracket-delimited relation body.
///
/// Bodies that use parentheses return `None`, as do unbalanced bracket bodies.
/// Nested brackets are supported by collecting text from the opening bracket
/// through the matching closing bracket.
pub(super) fn bracket_element_type(syntax: &rowan::SyntaxNode<DdlogLanguage>) -> Option<String> {
    let mut elements = elements_after_name(syntax).into_iter();
    for element in elements.by_ref() {
        if is_trivia(&element) {
            continue;
        }
        if element.kind() == SyntaxKind::T_LBRACKET {
            return collect_bracket_element_text(elements);
        }
        if element.kind() == SyntaxKind::T_LPAREN {
            return None;
        }
    }
    None
}

fn elements_after_name(
    syntax: &rowan::SyntaxNode<DdlogLanguage>,
) -> Vec<rowan::SyntaxElement<DdlogLanguage>> {
    let mut found_name = false;
    let mut elements = Vec::new();
    for element in syntax.children_with_tokens() {
        if found_name {
            elements.push(element);
            continue;
        }
        if is_trivia(&element) {
            continue;
        }
        match element.kind() {
            SyntaxKind::T_IDENT => found_name = true,
            SyntaxKind::T_LPAREN | SyntaxKind::T_LBRACKET => break,
            _ => {}
        }
    }
    elements
}

/// Collect text up to the matching closing bracket for a relation element type.
///
/// The depth counter starts at one because the caller has already consumed the
/// opening bracket. `None` represents either excess closing brackets, detected
/// with checked subtraction, or an exhausted iterator while nested brackets are
/// still open.
fn collect_bracket_element_text<I>(elements: I) -> Option<String>
where
    I: Iterator<Item = rowan::SyntaxElement<DdlogLanguage>>,
{
    let mut depth = 1usize;
    let mut text = String::new();
    for element in elements {
        match element.kind() {
            SyntaxKind::T_LBRACKET => {
                depth += 1;
                text.push_str(&element.to_string());
            }
            SyntaxKind::T_RBRACKET => {
                depth = depth.checked_sub(1)?;
                if depth == 0 {
                    return Some(text);
                }
                text.push_str(&element.to_string());
            }
            _ => text.push_str(&element.to_string()),
        }
    }
    None
}

#[cfg(test)]
mod tests {
    //! Regression tests for the delimiter guard that stops preamble scanning
    //! before a body delimiter when a malformed node lacks a relation name.

    use super::super::Relation;
    use super::{elements_after_name, inspect_preamble};
    use crate::{DdlogLanguage, SyntaxKind};
    use rowan::{GreenNodeBuilder, Language as _, SyntaxNode};

    /// Build a synthetic `N_RELATION_DECL` node from raw `(kind, text)` tokens.
    ///
    /// The span scanner never emits a relation candidate whose body delimiter
    /// precedes the name, so the guard is exercised by constructing the node
    /// directly rather than routing malformed source through `parse`.
    fn relation_node(tokens: &[(SyntaxKind, &str)]) -> SyntaxNode<DdlogLanguage> {
        let mut builder = GreenNodeBuilder::new();
        builder.start_node(DdlogLanguage::kind_to_raw(SyntaxKind::N_RELATION_DECL));
        for (kind, text) in tokens {
            builder.token(DdlogLanguage::kind_to_raw(*kind), text);
        }
        builder.finish_node();
        SyntaxNode::new_root(builder.finish())
    }

    /// Assert the guard keeps a body identifier from leaking out as the
    /// relation name when a body delimiter precedes any name token.
    fn assert_delimiter_guard(node: SyntaxNode<DdlogLanguage>) {
        assert_eq!(inspect_preamble(&node).name, None);
        assert!(
            elements_after_name(&node).is_empty(),
            "no body elements should be returned without a name",
        );
        let rel = Relation { syntax: node };
        assert_eq!(rel.name(), None);
    }

    #[test]
    fn paren_before_name_does_not_expose_body_identifier() {
        assert_delimiter_guard(relation_node(&[
            (SyntaxKind::T_LPAREN, "("),
            (SyntaxKind::T_IDENT, "x"),
            (SyntaxKind::T_COLON, ":"),
            (SyntaxKind::T_WHITESPACE, " "),
            (SyntaxKind::T_IDENT, "u32"),
            (SyntaxKind::T_RPAREN, ")"),
        ]));
    }

    #[test]
    fn bracket_before_name_does_not_expose_body_identifier() {
        assert_delimiter_guard(relation_node(&[
            (SyntaxKind::T_LBRACKET, "["),
            (SyntaxKind::T_IDENT, "u32"),
            (SyntaxKind::T_RBRACKET, "]"),
        ]));
    }
}
