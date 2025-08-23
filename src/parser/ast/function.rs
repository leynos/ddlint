//!
//! AST wrapper for function declarations and definitions.
//!
//! Provides access to function names, parameters, return types and whether the
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

    use crate::parse;
    #[expect(clippy::expect_used, reason = "Using expect for clearer test failures")]
    #[test]
    fn function_name() {
        let parsed = parse("function foo() {}");
        let func = parsed
            .root()
            .functions()
            .first()
            .cloned()
            .expect("function missing");
        assert_eq!(func.name().as_deref(), Some("foo"));
    }
}
