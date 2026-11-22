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

    /// The loop positions, in the column order used by
    /// [`EXPECTED_OUTCOMES`].
    const POSITIONS: [ItemPosition; 3] = [
        ItemPosition::Opening,
        ItemPosition::AfterItem,
        ItemPosition::AfterSeparator,
    ];

    /// Independent expected outcome for every token the sweep exercises.
    ///
    /// Rows cover the tokens a block can meet (`{`, `}`, `;`, `,`, `:`, an
    /// identifier, a number, `+`, and an enclosing delimiter) plus end of
    /// input; columns follow [`POSITIONS`]. Every `SyntaxKind` falls into one
    /// of these semantic classes, so the corpus is exhaustive rather than a
    /// sample.
    const EXPECTED_OUTCOMES: [(Option<SyntaxKind>, [BlockFollow; 3]); 11] = [
        // End of input leaves the block unclosed wherever it appears.
        (None, [BlockFollow::Unclosed; 3]),
        // A nested `{` begins an item unless one already ended without `;`.
        (
            Some(SyntaxKind::T_LBRACE),
            [
                BlockFollow::Item,
                BlockFollow::MissingSeparator,
                BlockFollow::Item,
            ],
        ),
        // `}` closes an empty block at the opening, and any block afterwards.
        (
            Some(SyntaxKind::T_RBRACE),
            [BlockFollow::Empty, BlockFollow::Close, BlockFollow::Close],
        ),
        // `;` separates only after an item; otherwise it is an empty item.
        (
            Some(SyntaxKind::T_SEMI),
            [
                BlockFollow::EmptyItem,
                BlockFollow::Separator,
                BlockFollow::EmptyItem,
            ],
        ),
        // Any other token begins an item unless one already ended without a
        // separator. `,`, `:`, an identifier, a number and `+` behave alike.
        (
            Some(SyntaxKind::T_COMMA),
            [
                BlockFollow::Item,
                BlockFollow::MissingSeparator,
                BlockFollow::Item,
            ],
        ),
        (
            Some(SyntaxKind::T_COLON),
            [
                BlockFollow::Item,
                BlockFollow::MissingSeparator,
                BlockFollow::Item,
            ],
        ),
        (
            Some(SyntaxKind::T_IDENT),
            [
                BlockFollow::Item,
                BlockFollow::MissingSeparator,
                BlockFollow::Item,
            ],
        ),
        (
            Some(SyntaxKind::T_NUMBER),
            [
                BlockFollow::Item,
                BlockFollow::MissingSeparator,
                BlockFollow::Item,
            ],
        ),
        (
            Some(SyntaxKind::T_PLUS),
            [
                BlockFollow::Item,
                BlockFollow::MissingSeparator,
                BlockFollow::Item,
            ],
        ),
        // An enclosing delimiter ends the item list without a `}`.
        (
            Some(SyntaxKind::T_RPAREN),
            [BlockFollow::UnclosedBeforeDelimiter; 3],
        ),
        (
            Some(SyntaxKind::T_RBRACKET),
            [BlockFollow::UnclosedBeforeDelimiter; 3],
        ),
    ];

    /// Every corpus token produces its tabulated outcome in every position,
    /// and the sweep exercises both accepting and rejecting outcomes.
    #[test]
    fn block_follow_classification_matches_expected_outcomes() {
        let outcomes: Vec<BlockFollow> = EXPECTED_OUTCOMES
            .iter()
            .flat_map(|(kind, row)| {
                row.iter()
                    .copied()
                    .zip(POSITIONS)
                    .map(|(expected, position)| checked_outcome(position, *kind, expected))
            })
            .collect();
        let accepted = outcomes
            .iter()
            .filter(|outcome| is_accepted(**outcome))
            .count();
        assert_eq!(
            (accepted, outcomes.len() - accepted),
            (16, 17),
            "the sweep should exercise both outcomes across every position"
        );
    }

    /// Compare one corpus outcome with the expected table.
    fn checked_outcome(
        position: ItemPosition,
        kind: Option<SyntaxKind>,
        expected: BlockFollow,
    ) -> BlockFollow {
        let actual = classify_block_follow(position, kind);
        assert_eq!(
            actual, expected,
            "position {position:?} with {kind:?} should classify as {expected:?}"
        );
        actual
    }

    /// Whether an outcome lets the block loop continue with the item.
    const fn is_accepted(outcome: BlockFollow) -> bool {
        matches!(
            outcome,
            BlockFollow::Empty | BlockFollow::Close | BlockFollow::Separator | BlockFollow::Item
        )
    }
}
