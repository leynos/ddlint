//! Parsing of brace-delimited block expressions.
//!
//! A prefix `{` always introduces a block; there are no brace collection
//! literals in `DDlog`, and a `{` after an identifier in a struct-literal
//! context opens a struct instead. Items are expressions separated by `;`, a
//! trailing `;` is allowed,
//! and an empty item is rejected. The final item without a trailing `;` is the
//! block's value.
//!
//! Item parsing is a small state machine. [`classify_block_follow`] maps the
//! parser's position within the block and the next token to the action the
//! loop takes, keeping the diagnostics table-testable without a parser.

use super::pratt::Pratt;
use crate::{
    Span,
    SyntaxKind,
    parser::{
        ast::{BLOCK_ITEM_MIN_BP, BlockExpr, Expr},
        error_messages::{
            BLOCK_ITEM_SEPARATOR_ERROR,
            EMPTY_BLOCK_ITEM_ERROR,
            UNCLOSED_BLOCK_ERROR,
        },
    },
};

/// Where the block item loop stands.
///
/// The three positions are the only states the loop needs: nothing has been
/// parsed yet, an item has just been parsed, or a separator has just been
/// consumed.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum ItemPosition {
    /// Just after the opening `{`.
    Opening,
    /// Just after an item, awaiting `;` or `}`.
    AfterItem,
    /// Just after `;`, awaiting another item, `}`, or end of input.
    AfterSeparator,
}

/// What the block item loop does next.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum BlockFollow {
    /// Consume `}` and finish an empty block.
    Empty,
    /// Consume `}` and finish a block that has items.
    Close,
    /// Consume `;` and await the next item.
    Separator,
    /// Parse one item at [`BLOCK_ITEM_MIN_BP`].
    Item,
    /// Report an empty item.
    EmptyItem,
    /// Report a missing separator between two items.
    MissingSeparator,
    /// Report that the block is not closed.
    Unclosed,
    /// Report that an enclosing delimiter closed before the block's `}`.
    UnclosedBeforeDelimiter,
}

/// Map the loop position and next token to the next action.
///
/// The function is total: every `(position, kind)` pair has one outcome, and
/// `kind == None` always means end of input.
pub(super) const fn classify_block_follow(
    position: ItemPosition,
    kind: Option<SyntaxKind>,
) -> BlockFollow {
    use BlockFollow::{
        Close,
        Empty,
        EmptyItem,
        Item,
        MissingSeparator,
        Separator,
        Unclosed,
        UnclosedBeforeDelimiter,
    };
    use ItemPosition::{AfterItem, AfterSeparator, Opening};

    match (position, kind) {
        (_, None) => Unclosed,
        (Opening, Some(SyntaxKind::T_RBRACE)) => Empty,
        (AfterItem | AfterSeparator, Some(SyntaxKind::T_RBRACE)) => Close,
        (Opening | AfterSeparator, Some(SyntaxKind::T_SEMI)) => EmptyItem,
        (AfterItem, Some(SyntaxKind::T_SEMI)) => Separator,
        // An enclosing `)` or `]` ends the item list without a `}`, so the
        // block is unclosed rather than missing a separator.
        (_, Some(SyntaxKind::T_RPAREN | SyntaxKind::T_RBRACKET)) => UnclosedBeforeDelimiter,
        (AfterItem, Some(_)) => MissingSeparator,
        (Opening | AfterSeparator, Some(_)) => Item,
    }
}

impl<I> Pratt<'_, I>
where
    I: Iterator<Item = (SyntaxKind, Span)> + Clone,
{
    /// Parse a block after its opening `{` has been consumed.
    ///
    /// `;` terminates each item and is consumed as the item separator, so it
    /// is not the sequencing operator here. Nested delimiters restore ordinary
    /// sequencing for their own contents.
    pub(super) fn parse_block(&mut self) -> Option<Expr> {
        self.with_block_item_scope(|this| {
            let (items, ends_with_semicolon) = this.parse_block_items()?;
            Self::finish_block(items, ends_with_semicolon)
        })
    }

    /// Parse block items up to and including the closing `}`.
    ///
    /// Returns the items in source order and whether a `;` followed the last
    /// one.
    fn parse_block_items(&mut self) -> Option<(Vec<Expr>, bool)> {
        let mut position = ItemPosition::Opening;
        let mut items = Vec::new();
        loop {
            match classify_block_follow(position, self.ts.peek_kind()) {
                BlockFollow::Empty => {
                    self.ts.next_tok()?;
                    return Some((items, false));
                }
                BlockFollow::Close => {
                    self.ts.next_tok()?;
                    let ends_with_semicolon = position == ItemPosition::AfterSeparator;
                    return Some((items, ends_with_semicolon));
                }
                BlockFollow::Separator => {
                    self.ts.next_tok()?;
                    position = ItemPosition::AfterSeparator;
                }
                BlockFollow::Item => {
                    let item = self.parse_expr(BLOCK_ITEM_MIN_BP)?;
                    items.push(item);
                    position = ItemPosition::AfterItem;
                }
                BlockFollow::EmptyItem => {
                    let span = self.ts.peek_span().unwrap_or_else(|| self.ts.eof_span());
                    self.ts.push_error(span, EMPTY_BLOCK_ITEM_ERROR);
                    return None;
                }
                BlockFollow::MissingSeparator => {
                    let span = self.ts.peek_span().unwrap_or_else(|| self.ts.eof_span());
                    self.ts.push_error(span, BLOCK_ITEM_SEPARATOR_ERROR);
                    return None;
                }
                BlockFollow::Unclosed => {
                    self.ts.push_error(self.ts.eof_span(), UNCLOSED_BLOCK_ERROR);
                    return None;
                }
                BlockFollow::UnclosedBeforeDelimiter => {
                    let span = self.ts.peek_span().unwrap_or_else(|| self.ts.eof_span());
                    self.ts.push_error(span, UNCLOSED_BLOCK_ERROR);
                    return None;
                }
            }
        }
    }

    /// Turn parsed items into a block expression.
    ///
    /// `None` is returned for the one unrepresentable pair, an empty item
    /// list with a trailing separator.
    fn finish_block(items: Vec<Expr>, ends_with_semicolon: bool) -> Option<Expr> {
        BlockExpr::from_items(items, ends_with_semicolon).map(Expr::Block)
    }
}

#[cfg(test)]
mod tests {
    //! Table tests for the block item state machine.
    use rstest::rstest;

    use super::{BlockFollow, ItemPosition, classify_block_follow};
    use crate::SyntaxKind;

    #[rstest]
    #[case::opening_close(ItemPosition::Opening, Some(SyntaxKind::T_RBRACE), BlockFollow::Empty)]
    #[case::opening_separator(
        ItemPosition::Opening,
        Some(SyntaxKind::T_SEMI),
        BlockFollow::EmptyItem
    )]
    #[case::opening_eof(ItemPosition::Opening, None, BlockFollow::Unclosed)]
    #[case::opening_rparen(
        ItemPosition::Opening,
        Some(SyntaxKind::T_RPAREN),
        BlockFollow::UnclosedBeforeDelimiter
    )]
    #[case::opening_item(ItemPosition::Opening, Some(SyntaxKind::T_IDENT), BlockFollow::Item)]
    #[case::after_item_separator(
        ItemPosition::AfterItem,
        Some(SyntaxKind::T_SEMI),
        BlockFollow::Separator
    )]
    #[case::after_item_close(
        ItemPosition::AfterItem,
        Some(SyntaxKind::T_RBRACE),
        BlockFollow::Close
    )]
    #[case::after_item_eof(ItemPosition::AfterItem, None, BlockFollow::Unclosed)]
    #[case::after_item_rparen(
        ItemPosition::AfterItem,
        Some(SyntaxKind::T_RPAREN),
        BlockFollow::UnclosedBeforeDelimiter
    )]
    #[case::after_item_rbracket(
        ItemPosition::AfterItem,
        Some(SyntaxKind::T_RBRACKET),
        BlockFollow::UnclosedBeforeDelimiter
    )]
    #[case::after_item_missing(
        ItemPosition::AfterItem,
        Some(SyntaxKind::T_IDENT),
        BlockFollow::MissingSeparator
    )]
    #[case::after_separator_close(
        ItemPosition::AfterSeparator,
        Some(SyntaxKind::T_RBRACE),
        BlockFollow::Close
    )]
    #[case::after_separator_separator(
        ItemPosition::AfterSeparator,
        Some(SyntaxKind::T_SEMI),
        BlockFollow::EmptyItem
    )]
    #[case::after_separator_eof(ItemPosition::AfterSeparator, None, BlockFollow::Unclosed)]
    #[case::after_separator_rparen(
        ItemPosition::AfterSeparator,
        Some(SyntaxKind::T_RPAREN),
        BlockFollow::UnclosedBeforeDelimiter
    )]
    #[case::after_separator_item(
        ItemPosition::AfterSeparator,
        Some(SyntaxKind::T_IDENT),
        BlockFollow::Item
    )]
    fn block_follow_classification_is_total(
        #[case] position: ItemPosition,
        #[case] kind: Option<SyntaxKind>,
        #[case] expected: BlockFollow,
    ) {
        assert_eq!(
            classify_block_follow(position, kind),
            expected,
            "position {position:?} with {kind:?} should classify as {expected:?}"
        );
    }

    /// The token corpus for the differential sweep: the tokens a block can
    /// meet (`{`, `}`, `;`, `,`, `:`, an identifier, a number, `+`, and an
    /// enclosing delimiter) plus end of input.
    const KINDS: [Option<SyntaxKind>; 11] = [
        None,
        Some(SyntaxKind::T_LBRACE),
        Some(SyntaxKind::T_RBRACE),
        Some(SyntaxKind::T_SEMI),
        Some(SyntaxKind::T_COMMA),
        Some(SyntaxKind::T_COLON),
        Some(SyntaxKind::T_IDENT),
        Some(SyntaxKind::T_NUMBER),
        Some(SyntaxKind::T_PLUS),
        Some(SyntaxKind::T_RPAREN),
        Some(SyntaxKind::T_RBRACKET),
    ];

    /// Independent decision table used to cross-check
    /// [`classify_block_follow`].
    ///
    /// It asks a token-shaped question first and only then dispatches on the
    /// loop position, which is a different shape from the implementation's
    /// pair match.
    const fn reference_follow(position: ItemPosition, kind: Option<SyntaxKind>) -> BlockFollow {
        let Some(token) = kind else {
            return BlockFollow::Unclosed;
        };
        let is_separator = matches!(token, SyntaxKind::T_SEMI);
        let is_close = matches!(token, SyntaxKind::T_RBRACE);
        let is_delimiter = matches!(token, SyntaxKind::T_RPAREN | SyntaxKind::T_RBRACKET);
        match position {
            ItemPosition::Opening => reference_opening(is_close, is_separator, is_delimiter),
            ItemPosition::AfterItem => reference_after_item(is_close, is_separator, is_delimiter),
            ItemPosition::AfterSeparator => {
                reference_after_separator(is_close, is_separator, is_delimiter)
            }
        }
    }

    /// The reference outcome just after the opening `{`.
    const fn reference_opening(
        is_close: bool,
        is_separator: bool,
        is_delimiter: bool,
    ) -> BlockFollow {
        if is_close {
            BlockFollow::Empty
        } else if is_separator {
            BlockFollow::EmptyItem
        } else if is_delimiter {
            BlockFollow::UnclosedBeforeDelimiter
        } else {
            BlockFollow::Item
        }
    }

    /// The reference outcome just after an item, where `;` separates.
    const fn reference_after_item(
        is_close: bool,
        is_separator: bool,
        is_delimiter: bool,
    ) -> BlockFollow {
        if is_separator {
            BlockFollow::Separator
        } else if is_close {
            BlockFollow::Close
        } else if is_delimiter {
            BlockFollow::UnclosedBeforeDelimiter
        } else {
            BlockFollow::MissingSeparator
        }
    }

    /// The reference outcome just after a separator, where `;` is empty.
    const fn reference_after_separator(
        is_close: bool,
        is_separator: bool,
        is_delimiter: bool,
    ) -> BlockFollow {
        if is_close {
            BlockFollow::Close
        } else if is_separator {
            BlockFollow::EmptyItem
        } else if is_delimiter {
            BlockFollow::UnclosedBeforeDelimiter
        } else {
            BlockFollow::Item
        }
    }

    /// Every `SyntaxKind` falls into one of the sweep's semantic classes, so
    /// the eleven-token corpus is exhaustive rather than a sample.
    #[test]
    fn block_follow_classification_matches_the_reference_table() {
        let mut accepted = 0usize;
        let mut rejected = 0usize;
        for position in [
            ItemPosition::Opening,
            ItemPosition::AfterItem,
            ItemPosition::AfterSeparator,
        ] {
            for kind in KINDS {
                let actual = classify_block_follow(position, kind);
                assert_eq!(
                    actual,
                    reference_follow(position, kind),
                    "position {position:?} with {kind:?} disagreed with the reference table"
                );
                match actual {
                    BlockFollow::Empty
                    | BlockFollow::Close
                    | BlockFollow::Separator
                    | BlockFollow::Item => accepted += 1,
                    BlockFollow::EmptyItem
                    | BlockFollow::MissingSeparator
                    | BlockFollow::Unclosed
                    | BlockFollow::UnclosedBeforeDelimiter => rejected += 1,
                }
            }
        }
        assert_eq!(
            (accepted, rejected),
            (16, 17),
            "the sweep should exercise both outcomes across every position"
        );
    }
}
