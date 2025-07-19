//!
//! AST wrapper for type definitions in `DDlog`.
//!
//! This module exposes the `TypeDef` struct for both regular `typedef` and
//! `extern type` declarations. It enables extraction of the type name and
//! whether the declaration is marked as `extern`.

use super::AstNode;
use crate::{DdlogLanguage, SyntaxKind};

/// Typed wrapper for a `typedef` or `extern type` declaration.
#[derive(Debug, Clone)]
pub struct TypeDef {
    pub(crate) syntax: rowan::SyntaxNode<DdlogLanguage>,
}

impl TypeDef {
    /// Name of the defined type.
    #[must_use]
    pub fn name(&self) -> Option<String> {
        let mut iter = self.syntax.children_with_tokens().peekable();
        if !super::skip_to_typedef_keyword(&mut iter) {
            return None;
        }
        super::take_first_ident(iter)
    }

    /// Whether this declaration is `extern`.
    #[must_use]
    pub fn is_extern(&self) -> bool {
        self.syntax
            .children_with_tokens()
            .any(|e| e.kind() == SyntaxKind::K_EXTERN)
    }
}

impl_ast_node!(TypeDef);

#[cfg(test)]
mod tests {

    use crate::parse;

    #[test]
    fn extern_type_parsed() {
        let parsed = parse("extern type Handle");
        #[expect(clippy::expect_used, reason = "Using expect for clearer test failures")]
        let td = parsed
            .root()
            .type_defs()
            .first()
            .cloned()
            .expect("typedef missing");
        assert_eq!(td.name(), Some("Handle".into()));
        assert!(td.is_extern());
    }

    #[test]
    fn regular_typedef_parsed() {
        let parsed = parse("typedef UserId = u64");
        #[expect(clippy::expect_used, reason = "Using expect for clearer test failures")]
        let td = parsed
            .root()
            .type_defs()
            .first()
            .cloned()
            .expect("typedef missing");
        assert_eq!(td.name(), Some("UserId".into()));
        assert!(!td.is_extern());
    }
}
