//!
//! AST wrapper for index declarations.

use super::AstNode;
use crate::{DdlogLanguage, SyntaxKind};

/// Typed wrapper for an index declaration.
#[derive(Debug, Clone)]
pub struct Index {
    pub(crate) syntax: rowan::SyntaxNode<DdlogLanguage>,
}

impl Index {
    /// Name of the index if present.
    #[must_use]
    pub fn name(&self) -> Option<String> {
        self.syntax
            .children_with_tokens()
            .skip_while(|e| !matches!(e.kind(), SyntaxKind::K_INDEX))
            .skip(1)
            .find_map(|e| match e {
                rowan::NodeOrToken::Token(t) if t.kind() == SyntaxKind::T_IDENT => {
                    Some(t.text().to_string())
                }
                _ => None,
            })
    }

    /// Target relation name.
    #[must_use]
    pub fn relation(&self) -> Option<String> {
        self.syntax
            .children_with_tokens()
            .skip_while(|e| !matches!(e.kind(), SyntaxKind::K_ON))
            .skip(1)
            .find_map(|e| match e {
                rowan::NodeOrToken::Token(t) if t.kind() == SyntaxKind::T_IDENT => {
                    Some(t.text().to_string())
                }
                _ => None,
            })
    }

    /// Column expressions included in the index.
    #[must_use]
    pub fn columns(&self) -> Vec<String> {
        use rowan::NodeOrToken;

        let mut iter = self.syntax.children_with_tokens().peekable();
        Self::skip_to_lparen(&mut iter);

        let mut cols = Vec::new();
        let mut buf = String::new();
        let mut depth = 0usize;

        for e in iter {
            match e {
                NodeOrToken::Token(t) => {
                    if Self::process_token(&t, &mut buf, &mut cols, &mut depth) {
                        break;
                    }
                }
                NodeOrToken::Node(n) => buf.push_str(&n.text().to_string()),
            }
        }

        cols
    }

    /// Skip tokens until the opening parenthesis of the column list.
    fn skip_to_lparen<I>(iter: &mut std::iter::Peekable<I>)
    where
        I: Iterator<Item = rowan::SyntaxElement<DdlogLanguage>>,
    {
        for e in iter {
            if e.kind() == SyntaxKind::T_LPAREN {
                break;
            }
        }
    }

    /// Process one token while collecting column text.
    ///
    /// Returns `true` when the closing parenthesis for the list is reached.
    fn process_token(
        token: &rowan::SyntaxToken<DdlogLanguage>,
        buf: &mut String,
        cols: &mut Vec<String>,
        depth: &mut usize,
    ) -> bool {
        match token.kind() {
            SyntaxKind::T_LPAREN => {
                *depth += 1;
                buf.push_str(token.text());
                false
            }
            SyntaxKind::T_RPAREN => Self::handle_rparen(token, buf, cols, depth),
            SyntaxKind::T_COMMA if *depth == 0 => {
                Self::handle_comma(buf, cols);
                false
            }
            _ => {
                buf.push_str(token.text());
                false
            }
        }
    }

    /// Handle a right parenthesis token.
    ///
    /// When `depth` reaches zero the list is complete and `true` is returned to
    /// signal termination.
    fn handle_rparen(
        token: &rowan::SyntaxToken<DdlogLanguage>,
        buf: &mut String,
        cols: &mut Vec<String>,
        depth: &mut usize,
    ) -> bool {
        if *depth == 0 {
            Self::add_column_if_non_empty(buf, cols);
            true
        } else {
            *depth -= 1;
            buf.push_str(token.text());
            false
        }
    }

    /// Handle a comma token separating top-level column expressions.
    fn handle_comma(buf: &mut String, cols: &mut Vec<String>) {
        Self::add_column_if_non_empty(buf, cols);
        buf.clear();
    }

    /// Add a trimmed column string to the results if not empty.
    fn add_column_if_non_empty(buf: &str, cols: &mut Vec<String>) {
        let col = buf.trim();
        if !col.is_empty() {
            cols.push(col.to_string());
        }
    }
}

impl_ast_node!(Index);

#[cfg(test)]
mod tests {

    use crate::parse;

    #[expect(clippy::expect_used, reason = "Using expect for clearer test failures")]
    #[test]
    fn index_columns() {
        let parsed = parse("index I on R(lower(name))");
        let idx = parsed
            .root()
            .indexes()
            .first()
            .cloned()
            .expect("index missing");
        assert_eq!(idx.relation(), Some("R".into()));
        assert_eq!(idx.columns(), vec!["lower(name)".to_string()]);
    }
}
