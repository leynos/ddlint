//!
//! AST wrapper for transformer declarations.

use super::AstNode;
use crate::DdlogLanguage;

/// Typed wrapper for a transformer declaration.
#[derive(Debug, Clone)]
pub struct Transformer {
    pub(crate) syntax: rowan::SyntaxNode<DdlogLanguage>,
}

impl Transformer {
    /// Name of the transformer if present.
    #[must_use]
    pub fn name(&self) -> Option<String> {
        let mut iter = self.syntax.children_with_tokens();
        if !super::skip_to_transformer_keyword(&mut iter) {
            return None;
        }
        super::take_first_ident(iter)
    }

    /// Input relations as pairs of name and type.
    #[must_use]
    pub fn inputs(&self) -> Vec<(String, String)> {
        let (pairs, _errors) =
            super::parse_utils::parse_name_type_pairs(self.syntax.children_with_tokens());
        pairs
    }

    /// Output relation names.
    #[must_use]
    pub fn outputs(&self) -> Vec<String> {
        super::parse_utils::parse_output_list(self.syntax.children_with_tokens())
    }
}

impl_ast_node!(Transformer);

#[cfg(test)]
mod tests {

    use crate::parse;

    #[expect(clippy::expect_used, reason = "Using expect for clearer test failures")]
    #[test]
    fn transformer_name() {
        let parsed = parse("extern transformer t(x: A): B");
        let tr = parsed
            .root()
            .transformers()
            .first()
            .cloned()
            .expect("transformer missing");
        assert_eq!(tr.name(), Some("t".into()));
    }
}
