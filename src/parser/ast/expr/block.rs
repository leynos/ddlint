//! Brace-delimited block expressions.
//!
//! A block is a sequence of items separated by `;`. Every item except an
//! optional final one is a statement whose value is discarded; a final item
//! without a trailing `;` supplies the block's value. When there is no such
//! item the block's value is unit `()`.
//!
//! Upstream `DDlog` has no brace-delimited collections, so braces always mean
//! a block. This module keeps the parsed items in source order and exposes
//! [`BlockExpr::items`] and [`BlockExpr::into_items`] so every consumer walks
//! that order consistently.

use super::Expr;

/// Parsed block expression: `{ item; item; value }`.
///
/// Every combination of `statements` and `value` is a valid block, so the
/// fields are public: `statements` holds the items followed by `;`, and
/// `value` holds the final item when it is not followed by `;`. An empty
/// `statements` list with no `value` is `{}`, whose value is unit.
#[derive(Debug, Clone, Default, PartialEq)]
pub struct BlockExpr {
    /// Items followed by `;`, in source order.
    pub statements: Vec<Expr>,
    /// Final item when it is not followed by `;`.
    pub value: Option<Box<Expr>>,
}

impl BlockExpr {
    /// Build a block from parsed items and whether the last item was
    /// followed by `;`.
    ///
    /// Returns `None` for the empty item list with a trailing separator,
    /// because `{ ; }` has no item to separate. The empty list without a
    /// separator is the empty block.
    ///
    /// # Examples
    ///
    /// ```
    /// use ddlint::parser::ast::{BlockExpr, Expr};
    ///
    /// let items = vec![
    ///     Expr::Variable("a".to_owned()),
    ///     Expr::Variable("b".to_owned()),
    /// ];
    /// let block = BlockExpr::from_items(items, false).expect("two items with no trailing ';'");
    ///
    /// assert_eq!(block.statements, vec![Expr::Variable("a".to_owned())]);
    /// assert_eq!(
    ///     block.value.as_deref(),
    ///     Some(&Expr::Variable("b".to_owned()))
    /// );
    /// assert!(BlockExpr::from_items(Vec::new(), true).is_none());
    /// ```
    #[must_use]
    pub fn from_items(mut items: Vec<Expr>, ends_with_semicolon: bool) -> Option<Self> {
        if items.is_empty() {
            return (!ends_with_semicolon).then(Self::default);
        }
        if ends_with_semicolon {
            return Some(Self {
                statements: items,
                value: None,
            });
        }
        let value = items.pop()?;
        Some(Self {
            statements: items,
            value: Some(Box::new(value)),
        })
    }

    /// Iterate over every item, statements first and the value last.
    ///
    /// # Examples
    ///
    /// ```
    /// use ddlint::parser::ast::{BlockExpr, Expr};
    ///
    /// let block = BlockExpr {
    ///     statements: vec![Expr::Variable("a".to_owned())],
    ///     value: Some(Box::new(Expr::Variable("b".to_owned()))),
    /// };
    /// let rendered: Vec<String> = block.items().map(Expr::to_sexpr).collect();
    ///
    /// assert_eq!(rendered, vec!["a".to_owned(), "b".to_owned()]);
    /// ```
    pub fn items(&self) -> impl Iterator<Item = &Expr> {
        self.statements.iter().chain(self.value.as_deref())
    }

    /// Consume the block and iterate over every item in source order.
    ///
    /// # Examples
    ///
    /// ```
    /// use ddlint::parser::ast::{BlockExpr, Expr};
    ///
    /// let block = BlockExpr {
    ///     statements: vec![Expr::Variable("a".to_owned())],
    ///     value: None,
    /// };
    ///
    /// assert_eq!(block.into_items().count(), 1);
    /// ```
    pub fn into_items(self) -> impl Iterator<Item = Expr> {
        self.statements
            .into_iter()
            .chain(self.value.map(|value| *value))
    }

    /// Return `true` when a non-empty block ends with `;`, so its value is
    /// unit.
    ///
    /// # Examples
    ///
    /// ```
    /// use ddlint::parser::ast::{BlockExpr, Expr};
    ///
    /// let unit_block = BlockExpr {
    ///     statements: vec![Expr::Variable("a".to_owned())],
    ///     value: None,
    /// };
    /// let value_block = BlockExpr {
    ///     statements: Vec::new(),
    ///     value: Some(Box::new(Expr::Variable("a".to_owned()))),
    /// };
    ///
    /// assert!(unit_block.ends_with_semicolon());
    /// assert!(!value_block.ends_with_semicolon());
    /// ```
    #[must_use]
    pub const fn ends_with_semicolon(&self) -> bool {
        !self.statements.is_empty() && self.value.is_none()
    }
}
