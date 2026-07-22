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

/// Structured interpretation of a relation declaration's body.
///
/// Distinguishes a valid record body, a valid bracket body (with the extracted
/// element type), a bracket body that is unclosed or unbalanced, and a
/// declaration with no `(`/`[` body delimiter at all. Callers use this to
/// surface explicit errors instead of silently treating malformed syntax as an
/// empty record body.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) enum BodyInspection {
    /// Valid record body introduced by `(`.
    Record,
    /// Valid bracket body with the extracted element type text.
    Bracket(String),
    /// A `[` body delimiter whose contents are unclosed or unbalanced.
    MalformedBracket,
    /// No `(` or `[` body delimiter follows the relation name.
    MissingDelimiter,
}

/// Inspect the body that immediately follows the relation name.
///
/// The relation grammar places the body directly after the name, so the first
/// non-trivia element after the name must be the body delimiter. Parentheses
/// introduce a record field list; brackets introduce an element type. Anything
/// else, or no element at all, is reported as
/// [`BodyInspection::MissingDelimiter`], and a bracket whose contents never
/// close is reported as [`BodyInspection::MalformedBracket`].
pub(super) fn inspect_body(syntax: &rowan::SyntaxNode<DdlogLanguage>) -> BodyInspection {
    let mut elements = elements_after_name(syntax).into_iter();
    let Some(delimiter) = elements.by_ref().find(|element| !is_trivia(element)) else {
        return BodyInspection::MissingDelimiter;
    };
    match delimiter.kind() {
        SyntaxKind::T_LPAREN => BodyInspection::Record,
        SyntaxKind::T_LBRACKET => collect_bracket_element_text(elements)
            .map_or(BodyInspection::MalformedBracket, BodyInspection::Bracket),
        _ => BodyInspection::MissingDelimiter,
    }
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
    use super::{BodyInspection, elements_after_name, inspect_body, inspect_preamble};
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

    #[test]
    fn unclosed_bracket_body_is_error() {
        // `R[u32` — a bracket body that never closes.
        let node = relation_node(&[
            (SyntaxKind::K_RELATION, "relation"),
            (SyntaxKind::T_WHITESPACE, " "),
            (SyntaxKind::T_IDENT, "R"),
            (SyntaxKind::T_LBRACKET, "["),
            (SyntaxKind::T_IDENT, "u32"),
        ]);
        assert_eq!(inspect_body(&node), BodyInspection::MalformedBracket);
        let rel = Relation { syntax: node };
        assert!(rel.element_type().is_err());
        assert!(rel.body().is_err());
        assert!(rel.columns().is_err());
    }

    #[test]
    fn record_body_with_invalid_column_list_is_error() {
        // `R(x:)` — a column name with no type in the record body.
        let node = relation_node(&[
            (SyntaxKind::T_IDENT, "R"),
            (SyntaxKind::T_LPAREN, "("),
            (SyntaxKind::T_IDENT, "x"),
            (SyntaxKind::T_COLON, ":"),
            (SyntaxKind::T_RPAREN, ")"),
        ]);
        assert_eq!(inspect_body(&node), BodyInspection::Record);
        let rel = Relation { syntax: node };
        assert!(rel.columns().is_err());
        assert!(rel.body().is_err());
        // The delimiter is a valid record `(`, so element type is `Ok(None)`.
        assert_eq!(rel.element_type(), Ok(None));
    }

    #[test]
    fn malformed_primary_key_binder_list_is_error() {
        // `R(x: u32) primary key (` — an unclosed primary-key binder list.
        let node = relation_node(&[
            (SyntaxKind::T_IDENT, "R"),
            (SyntaxKind::T_LPAREN, "("),
            (SyntaxKind::T_IDENT, "x"),
            (SyntaxKind::T_COLON, ":"),
            (SyntaxKind::T_WHITESPACE, " "),
            (SyntaxKind::T_IDENT, "u32"),
            (SyntaxKind::T_RPAREN, ")"),
            (SyntaxKind::T_WHITESPACE, " "),
            (SyntaxKind::T_IDENT, "primary"),
            (SyntaxKind::T_WHITESPACE, " "),
            (SyntaxKind::T_IDENT, "key"),
            (SyntaxKind::T_WHITESPACE, " "),
            (SyntaxKind::T_LPAREN, "("),
        ]);
        assert_eq!(inspect_body(&node), BodyInspection::Record);
        let rel = Relation { syntax: node };
        assert!(rel.primary_key().is_err());
    }

    #[test]
    fn missing_body_delimiter_is_error() {
        // `relation R` — a name with no `(`/`[` body delimiter.
        let node = relation_node(&[
            (SyntaxKind::K_RELATION, "relation"),
            (SyntaxKind::T_WHITESPACE, " "),
            (SyntaxKind::T_IDENT, "R"),
        ]);
        assert_eq!(inspect_body(&node), BodyInspection::MissingDelimiter);
        let rel = Relation { syntax: node };
        assert!(rel.body().is_err());
        assert!(rel.columns().is_err());
        assert!(rel.element_type().is_err());
        assert!(rel.primary_key().is_err());
    }
}
