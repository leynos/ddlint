//!
//! AST wrapper for `typedef` and `extern type` declarations.

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

    #[expect(clippy::expect_used, reason = "Using expect for clearer test failures")]
    #[test]
    fn extern_type_parsed() {
        let parsed = parse("extern type Handle");
        let td = parsed
            .root()
            .type_defs()
            .first()
            .cloned()
            .expect("typedef missing");
        assert_eq!(td.name(), Some("Handle".into()));
        assert!(td.is_extern());
    }
}
