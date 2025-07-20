//!
//! AST wrapper for transformer declarations.
//!
//! This module provides a typed wrapper around `DDlog` transformer syntax nodes,
//! enabling structured access to transformer names, input relations with
//! types and output relations.

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
        let (pairs, errors) =
            super::parse_utils::parse_name_type_pairs(self.syntax.children_with_tokens());
        if !errors.is_empty() {
            log::debug!("Parsing errors in transformer inputs: {errors:?}");
        }
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

    #[expect(clippy::expect_used, reason = "Using expect for clearer test failures")]
    #[test]
    fn multiple_inputs_outputs() {
        let src = "extern transformer t(a: X, b: Y): Out1, Out2";
        let parsed = parse(src);
        let tr = parsed
            .root()
            .transformers()
            .first()
            .cloned()
            .expect("transformer missing");
        assert_eq!(
            tr.inputs(),
            vec![("a".into(), "X".into()), ("b".into(), "Y".into())]
        );
        assert_eq!(tr.outputs(), vec!["Out1".to_string(), "Out2".to_string()]);
    }
}
