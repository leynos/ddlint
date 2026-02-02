//!
//! Wrapper for the root `DATALOG_PROGRAM` node.
//!
//! Provides typed accessors for top-level items such as imports and relations.
//!
//! # Examples
//!
//! ```rust,ignore
//! # use ddlint::parse;
//! # fn root(src: &str) -> ddlint::parser::ast::Root {
//! #     parse(src).root()
//! # }
//! let root = root("import foo; relation R(x: u32)");
//! assert_eq!(root.imports().len(), 1);
//! assert_eq!(root.relations().len(), 1);
//! ```

use rowan::GreenNode;

use super::{Apply, Function, Import, Index, Relation, Rule, Transformer, TypeDef};
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

    fn collect_children<T>(
        &self,
        kind: SyntaxKind,
        map: impl Fn(rowan::SyntaxNode<DdlogLanguage>) -> T,
    ) -> Vec<T> {
        self.syntax
            .children()
            .filter(|n| n.kind() == kind)
            .map(map)
            .collect()
    }

    /// Collect all `import` statements.
    #[must_use]
    pub fn imports(&self) -> Vec<Import> {
        self.collect_children(SyntaxKind::N_IMPORT_STMT, |syntax| Import { syntax })
    }

    /// Collect all `typedef` declarations.
    #[must_use]
    pub fn type_defs(&self) -> Vec<TypeDef> {
        self.collect_children(SyntaxKind::N_TYPE_DEF, |syntax| TypeDef { syntax })
    }

    /// Collect all relation declarations.
    #[must_use]
    pub fn relations(&self) -> Vec<Relation> {
        self.collect_children(SyntaxKind::N_RELATION_DECL, |syntax| Relation { syntax })
    }

    /// Collect all index declarations.
    #[must_use]
    pub fn indexes(&self) -> Vec<Index> {
        self.collect_children(SyntaxKind::N_INDEX, |syntax| Index { syntax })
    }

    /// Collect all function declarations.
    #[must_use]
    pub fn functions(&self) -> Vec<Function> {
        self.collect_children(SyntaxKind::N_FUNCTION, |syntax| Function { syntax })
    }

    /// Collect all transformer declarations.
    #[must_use]
    pub fn transformers(&self) -> Vec<Transformer> {
        self.collect_children(SyntaxKind::N_TRANSFORMER, |syntax| Transformer { syntax })
    }

    /// Collect all `apply` statements.
    #[must_use]
    pub fn applys(&self) -> Vec<Apply> {
        self.collect_children(SyntaxKind::N_APPLY, |syntax| Apply { syntax })
    }

    /// Collect all rule declarations.
    #[must_use]
    pub fn rules(&self) -> Vec<Rule> {
        self.collect_children(SyntaxKind::N_RULE, |syntax| Rule { syntax })
    }
}

#[cfg(test)]
mod tests {

    use crate::parse;

    #[test]
    fn round_trip_empty() {
        let parsed = parse("");
        let root = parsed.root();
        assert!(root.imports().is_empty());
        assert!(root.type_defs().is_empty());
        assert!(root.relations().is_empty());
        assert!(root.indexes().is_empty());
        assert!(root.functions().is_empty());
        assert!(root.transformers().is_empty());
        assert!(root.applys().is_empty());
        assert!(root.rules().is_empty());
    }
}
