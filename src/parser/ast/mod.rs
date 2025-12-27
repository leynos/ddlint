//!
//! Light-weight AST wrappers built around `rowan` syntax nodes.
//!
//! These wrappers expose typed navigation over the CST produced by the parser.
//! They stay intentionally small, so higher layers can interact with the tree
//! without depending on a fully fledged semantic model.
//!
//! The façade re-exports expression helpers, including [`MatchArm`], enabling
//! tests and fixtures to assemble match expressions without dipping into
//! private modules.

use rowan::SyntaxElement;

use self::parse_utils::is_trivia;
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

/// Advance the iterator until `predicate` matches a token kind.
///
/// `K_EXTERN` tokens are ignored because they are generally irrelevant for
/// AST navigation. The iterator is advanced until `predicate` returns `true`
/// for a token, in which case the function returns `true`.
///
/// # Parameters
/// - `iter`: Iterator over syntax elements.
/// - `predicate`: Test function identifying the desired token kind.
///
/// # Returns
/// `true` if a matching token was found before the iterator was exhausted.
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
///
/// # Parameters
/// - `iter`: Iterator yielding tokens from the syntax tree.
///
/// # Returns
/// `true` if one of the keywords was found before exhaustion.
fn skip_to_typedef_keyword(
    iter: &mut impl Iterator<Item = rowan::SyntaxElement<DdlogLanguage>>,
) -> bool {
    skip_to_match(iter, |k| {
        matches!(k, SyntaxKind::K_TYPEDEF | SyntaxKind::K_TYPE)
    })
}

/// Advance the iterator until `transformer` is encountered.
///
/// # Parameters
/// - `iter`: Iterator yielding tokens from the syntax tree.
///
/// # Returns
/// `true` if the keyword was found before exhaustion.
fn skip_to_transformer_keyword(
    iter: &mut impl Iterator<Item = rowan::SyntaxElement<DdlogLanguage>>,
) -> bool {
    skip_to_match(iter, |k| k == SyntaxKind::K_TRANSFORMER)
}

/// Extract the first identifier token from the iterator.
///
/// Whitespace, and comments, are skipped. If a non-identifier token is
/// encountered before an identifier, `None` is returned.
///
/// # Parameters
/// - `iter`: Iterator over syntax elements.
///
/// # Returns
/// The identifier text if one is found.
fn take_first_ident(
    iter: impl Iterator<Item = rowan::SyntaxElement<DdlogLanguage>>,
) -> Option<String> {
    use rowan::NodeOrToken;

    iter.into_iter()
        .find(|e| !is_trivia(e))
        .and_then(|e| match e {
            NodeOrToken::Token(t) if t.kind() == SyntaxKind::T_IDENT => Some(t.text().to_string()),
            _ => None,
        })
}

/// Consume consecutive whitespace and comment tokens from the iterator.
///
/// # Parameters
/// - `iter`: Peekable iterator over syntax elements.
pub(super) fn skip_whitespace_and_comments<I>(iter: &mut std::iter::Peekable<I>)
where
    I: Iterator<Item = SyntaxElement<DdlogLanguage>>,
{
    while matches!(iter.peek(), Some(e) if is_trivia(e)) {
        iter.next();
    }
}

pub mod parse_utils;

pub(crate) mod expr;
mod function;
mod import;
mod index;
mod number;
mod pattern;
mod precedence;
mod relation;
mod root;
pub(crate) mod rule;
pub(crate) mod rule_head;
mod string_literal;
mod transformer;
mod type_def;

/// Expression AST nodes and helpers used across tests and fixtures.
///
/// Re-exports [`MatchArm`], allowing callers to assemble match expressions
/// without reaching into private modules.
pub use expr::{BinaryOp, Expr, Literal, MatchArm, UnaryOp};
pub use function::Function;
pub use import::Import;
pub use index::Index;
pub use number::{FloatLiteral, IntBase, IntLiteral, NumberLiteral};
pub use pattern::{Pattern, PatternLiteral};
pub(crate) use precedence::{infix_binding_power, prefix_binding_power};
pub use relation::Relation;
pub use root::Root;
pub use rule::{
    AggregationSource, Rule, RuleAggregation, RuleAssignment, RuleBodyExpression, RuleBodyTerm,
    RuleForLoop,
};
pub use rule_head::RuleHead;
pub use string_literal::{StringKind, StringLiteral};
pub use transformer::Transformer;
pub use type_def::TypeDef;

#[cfg(test)]
mod tests {

    mod precedence;

    use crate::parse;

    #[test]
    fn root_collects_imports() {
        let src = "import foo";
        let parsed = parse(src);
        assert_eq!(parsed.root().imports().len(), 1);
    }
}
