//!
//! AST wrapper for type definitions in `DDlog`.
//!
//! This module exposes the `TypeDef` struct for both regular `typedef` and
//! `extern type` declarations. It enables extraction of the type name, and
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

    /// Precise source span for the type name token, if present.
    #[must_use]
    pub fn name_span(&self) -> Option<crate::Span> {
        self.name()
            .and_then(|name| super::find_identifier_span(&self.syntax, &name))
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

    use crate::{parse, test_util::span_text};

    #[test]
    fn extern_type_parsed() {
        let parsed = parse("extern type Handle");
        crate::test_util::assert_no_parse_errors(parsed.errors());
        #[expect(clippy::expect_used, reason = "Using expect for clearer test failures")]
        let td = parsed
            .root()
            .type_defs()
            .first()
            .cloned()
            .expect("typedef missing");
        assert_eq!(td.name().as_deref(), Some("Handle"));
        assert!(td.is_extern());
    }

    #[test]
    fn regular_typedef_parsed() {
        let parsed = parse("typedef UserId = u64");
        crate::test_util::assert_no_parse_errors(parsed.errors());
        #[expect(clippy::expect_used, reason = "Using expect for clearer test failures")]
        let td = parsed
            .root()
            .type_defs()
            .first()
            .cloned()
            .expect("typedef missing");
        assert_eq!(td.name().as_deref(), Some("UserId"));
        assert!(!td.is_extern());
    }

    #[test]
    fn typedef_name_span_points_to_declaration_identifier() {
        let source = "typedef UserId = UserId";
        let parsed = parse(source);
        crate::test_util::assert_no_parse_errors(parsed.errors());
        #[expect(clippy::expect_used, reason = "Using expect for clearer test failures")]
        let td = parsed
            .root()
            .type_defs()
            .first()
            .cloned()
            .expect("typedef missing");

        let span = td
            .name_span()
            .unwrap_or_else(|| panic!("missing typedef name_span in `{source}`"));

        assert_eq!(span_text(source, &span), "UserId");
        assert_eq!(span.start, "typedef ".len());
    }
}
