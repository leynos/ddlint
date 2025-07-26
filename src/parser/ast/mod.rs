//!
//! Light-weight AST wrappers built around `rowan` syntax nodes.
//!
//! These wrappers expose typed navigation over the CST produced by the parser.
//! They stay intentionally small so that higher layers can interact with the
//! tree without depending on a fully fledged semantic model.

use rowan::SyntaxElement;

use crate::{DdlogLanguage, SyntaxKind};

#[cfg_attr(
    not(test),
    expect(dead_code, reason = "primarily exercised through test modules")
)]
/// Common interface for AST wrappers.
pub(crate) trait AstNode {
    /// Access the underlying syntax node.
    fn syntax(&self) -> &rowan::SyntaxNode<DdlogLanguage>;
}

macro_rules! impl_ast_node {
    ($ty:ty) => {
        impl AstNode for $ty {
            fn syntax(&self) -> &rowan::SyntaxNode<DdlogLanguage> {
                &self.syntax
            }
        }
    };
}

/// Advance the iterator until `predicate` returns `true` for a token kind.
///
/// `K_EXTERN` tokens are skipped as they are syntactic noise for most
/// traversal contexts.
fn skip_to_match(
    iter: &mut impl Iterator<Item = rowan::SyntaxElement<DdlogLanguage>>,
    predicate: impl Fn(SyntaxKind) -> bool,
) -> bool {
    for e in iter.by_ref() {
        let kind = e.kind();
        if kind == SyntaxKind::K_EXTERN {
            continue;
        }
        if predicate(kind) {
            return true;
        }
    }
    false
}

/// Advance the iterator until `typedef` or `type` is encountered.
fn skip_to_typedef_keyword(
    iter: &mut impl Iterator<Item = rowan::SyntaxElement<DdlogLanguage>>,
) -> bool {
    skip_to_match(iter, |k| {
        matches!(k, SyntaxKind::K_TYPEDEF | SyntaxKind::K_TYPE)
    })
}

/// Advance the iterator until `transformer` is encountered.
fn skip_to_transformer_keyword(
    iter: &mut impl Iterator<Item = rowan::SyntaxElement<DdlogLanguage>>,
) -> bool {
    skip_to_match(iter, |k| k == SyntaxKind::K_TRANSFORMER)
}

/// Extract the first identifier token from the iterator, skipping whitespace and
/// comments.
///
/// Returns `None` if the iterator hits a non-identifier token first.
fn take_first_ident(
    iter: impl Iterator<Item = rowan::SyntaxElement<DdlogLanguage>>,
) -> Option<String> {
    use rowan::NodeOrToken;
    for e in iter {
        match e {
            NodeOrToken::Token(t) if t.kind() == SyntaxKind::T_IDENT => {
                return Some(t.text().to_string());
            }
            NodeOrToken::Token(t)
                if matches!(t.kind(), SyntaxKind::T_WHITESPACE | SyntaxKind::T_COMMENT) => {}
            _ => return None,
        }
    }
    None
}

pub(super) fn skip_whitespace_and_comments<I>(iter: &mut std::iter::Peekable<I>)
where
    I: Iterator<Item = SyntaxElement<DdlogLanguage>>,
{
    while matches!(
        iter.peek().map(SyntaxElement::kind),
        Some(SyntaxKind::T_WHITESPACE | SyntaxKind::T_COMMENT)
    ) {
        iter.next();
    }
}

pub mod parse_utils;

pub(crate) mod expr;
mod function;
mod import;
mod index;
mod relation;
mod root;
mod rule;
mod transformer;
mod type_def;

pub use expr::{BinaryOp, Expr, Literal, UnaryOp};
pub use function::Function;
pub use import::Import;
pub use index::Index;
pub use relation::Relation;
pub use root::Root;
pub use rule::Rule;
pub use transformer::Transformer;
pub use type_def::TypeDef;

#[cfg(test)]
mod tests {

    use crate::parse;

    #[test]
    fn root_collects_imports() {
        let src = "import foo";
        let parsed = parse(src);
        assert_eq!(parsed.root().imports().len(), 1);
    }
}
