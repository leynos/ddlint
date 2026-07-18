//!
//! AST wrapper for function declarations and definitions.
//!
//! Provides access to function names, parameters, return types, and whether the
//! function is declared as `extern`.

use super::AstNode;
use crate::{DdlogLanguage, SyntaxKind};

/// Typed wrapper for a function declaration or definition.
#[derive(Debug, Clone)]
pub struct Function {
    pub(crate) syntax: rowan::SyntaxNode<DdlogLanguage>,
}

impl Function {
    /// Name of the function if present.
    #[must_use]
    pub fn name(&self) -> Option<String> {
        self.syntax
            .children_with_tokens()
            .skip_while(|e| !matches!(e.kind(), SyntaxKind::K_FUNCTION))
            .skip(1)
            .find_map(|e| match e {
                rowan::NodeOrToken::Token(t) if t.kind() == SyntaxKind::T_IDENT => {
                    Some(t.text().to_string())
                }
                _ => None,
            })
    }

    /// Precise source span for the function name token, if present.
    #[must_use]
    pub fn name_span(&self) -> Option<crate::Span> {
        self.name()
            .and_then(|name| super::find_identifier_span(&self.syntax, &name))
    }

    /// Returns `true` if declared with the `extern` keyword.
    #[must_use]
    pub fn is_extern(&self) -> bool {
        self.syntax
            .children_with_tokens()
            .any(|e| e.kind() == SyntaxKind::K_EXTERN)
    }

    /// Parameters as pairs of name and type.
    #[must_use]
    pub fn parameters(&self) -> Vec<(String, String)> {
        let (pairs, errors) =
            super::parse_utils::parse_name_type_pairs(self.syntax.children_with_tokens());
        if !errors.is_empty() {
            log::debug!("Parsing errors in function parameters: {errors:?}");
        }
        pairs
    }

    /// Return type text if specified.
    #[must_use]
    pub fn return_type(&self) -> Option<String> {
        let mut iter = self.syntax.children_with_tokens().peekable();
        let mut depth = 0usize;
        for e in &mut iter {
            match e.kind() {
                SyntaxKind::T_LPAREN => depth += 1,
                SyntaxKind::T_RPAREN => {
                    depth -= 1;
                    if depth == 0 {
                        break;
                    }
                }
                _ => {}
            }
        }

        super::parse_utils::parse_type_after_colon(&mut iter)
    }
}

impl_ast_node!(Function);

#[cfg(test)]
mod tests {

    //! Tests for function node parsing.
    use crate::{parse, test_util::span_text};

    #[expect(clippy::expect_used, reason = "Using expect for clearer test failures")]
    #[test]
    fn function_name() {
        let parsed = parse("function foo() {}");
        crate::test_util::assert_no_parse_errors(parsed.errors());
        let func = parsed
            .root()
            .functions()
            .first()
            .cloned()
            .expect("function missing");
        assert_eq!(func.name().as_deref(), Some("foo"));
    }

    #[test]
    fn function_name_span_points_to_declaration_identifier() {
        let source = "function project(project: u32): u32 { project }";
        let parsed = parse(source);
        crate::test_util::assert_no_parse_errors(parsed.errors());
        #[expect(clippy::expect_used, reason = "Using expect for clearer test failures")]
        let func = parsed
            .root()
            .functions()
            .first()
            .cloned()
            .expect("function missing");

        let span = func
            .name_span()
            .unwrap_or_else(|| panic!("missing function name_span in `{source}`"));

        assert_eq!(span_text(source, &span), "project");
        assert_eq!(span.start, "function ".len());
    }
}
