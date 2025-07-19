//!
//! Wrapper for the root `DATALOG_PROGRAM` node.
//!
//! Provides typed accessors for top-level items such as imports and relations.

use rowan::GreenNode;

use super::{Function, Import, Index, Relation, Rule, Transformer, TypeDef};
use crate::{DdlogLanguage, SyntaxKind};

/// The root of a parsed `DDlog` file.
#[derive(Debug, Clone)]
pub struct Root {
    pub(crate) syntax: rowan::SyntaxNode<DdlogLanguage>,
}

impl Root {
    /// Obtain the underlying syntax node.
    #[must_use]
    pub fn syntax(&self) -> &rowan::SyntaxNode<DdlogLanguage> {
        &self.syntax
    }

    /// Create a new `Root` from a green node.
    #[must_use]
    pub fn from_green(green: GreenNode) -> Self {
        Self {
            syntax: rowan::SyntaxNode::<DdlogLanguage>::new_root(green),
        }
    }

    /// The kind of this root node.
    #[must_use]
    pub fn kind(&self) -> SyntaxKind {
        self.syntax.kind()
    }

    /// Text range covered by this root.
    #[must_use]
    pub fn text_range(&self) -> rowan::TextRange {
        self.syntax.text_range()
    }

    /// Text content of this root.
    #[must_use]
    pub fn text(&self) -> String {
        self.syntax.text().to_string()
    }

    /// Collect all `import` statements.
    #[must_use]
    pub fn imports(&self) -> Vec<Import> {
        self.syntax
            .children()
            .filter(|n| n.kind() == SyntaxKind::N_IMPORT_STMT)
            .map(|syntax| Import { syntax })
            .collect()
    }

    /// Collect all `typedef` declarations.
    #[must_use]
    pub fn type_defs(&self) -> Vec<TypeDef> {
        self.syntax
            .children()
            .filter(|n| n.kind() == SyntaxKind::N_TYPE_DEF)
            .map(|syntax| TypeDef { syntax })
            .collect()
    }

    /// Collect all relation declarations.
    #[must_use]
    pub fn relations(&self) -> Vec<Relation> {
        self.syntax
            .children()
            .filter(|n| n.kind() == SyntaxKind::N_RELATION_DECL)
            .map(|syntax| Relation { syntax })
            .collect()
    }

    /// Collect all index declarations.
    #[must_use]
    pub fn indexes(&self) -> Vec<Index> {
        self.syntax
            .children()
            .filter(|n| n.kind() == SyntaxKind::N_INDEX)
            .map(|syntax| Index { syntax })
            .collect()
    }

    /// Collect all function declarations.
    #[must_use]
    pub fn functions(&self) -> Vec<Function> {
        self.syntax
            .children()
            .filter(|n| n.kind() == SyntaxKind::N_FUNCTION)
            .map(|syntax| Function { syntax })
            .collect()
    }

    /// Collect all transformer declarations.
    #[must_use]
    pub fn transformers(&self) -> Vec<Transformer> {
        self.syntax
            .children()
            .filter(|n| n.kind() == SyntaxKind::N_TRANSFORMER)
            .map(|syntax| Transformer { syntax })
            .collect()
    }

    /// Collect all rule declarations.
    #[must_use]
    pub fn rules(&self) -> Vec<Rule> {
        self.syntax
            .children()
            .filter(|n| n.kind() == SyntaxKind::N_RULE)
            .map(|syntax| Rule { syntax })
            .collect()
    }
}

#[cfg(test)]
mod tests {

    use crate::parse;

    #[test]
    fn round_trip_empty() {
        let parsed = parse("");
        assert!(parsed.root().imports().is_empty());
    }
}
