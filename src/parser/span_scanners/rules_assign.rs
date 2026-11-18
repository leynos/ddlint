//! Assignment scanning for rule declarations.
//!
//! Kept apart from the rule-body grammar so the assignment shape, whose
//! right-hand side may be a control-flow statement, stays readable on its own.

use chumsky::prelude::*;

use crate::{SyntaxKind, parser::lexer_helpers::balanced_block};

/// Scan an assignment whose right-hand side may be a control-flow expression.
///
/// The flat expression loop in [`super::rule_statement`] stops at `if`,
/// `for`, and `skip`, so `var y = if x > 1 { x } else { 0 }` would otherwise be
/// scanned as a left-hand side followed by a separate statement. Keyword
/// statements are tried before this parser, keeping a leading `if` or `for`
/// under its own grammar.
pub(super) fn assignment_statement(
    ws: impl Parser<SyntaxKind, (), Error = Simple<SyntaxKind>> + Clone + 'static,
    statement: BoxedParser<'static, SyntaxKind, (), Simple<SyntaxKind>>,
) -> impl Parser<SyntaxKind, (), Error = Simple<SyntaxKind>> + Clone {
    let lhs_token = choice((
        balanced_block(SyntaxKind::T_LPAREN, SyntaxKind::T_RPAREN),
        balanced_block(SyntaxKind::T_LBRACKET, SyntaxKind::T_RBRACKET),
        filter(|kind: &SyntaxKind| {
            !matches!(
                kind,
                SyntaxKind::T_COMMA
                    | SyntaxKind::T_DOT
                    | SyntaxKind::T_EQ
                    | SyntaxKind::K_IF
                    | SyntaxKind::K_FOR
                    | SyntaxKind::K_SKIP
            )
        })
        .ignored()
        .padded_by(ws.clone()),
    ));
    lhs_token
        .repeated()
        .at_least(1)
        .ignored()
        .then_ignore(just(SyntaxKind::T_EQ).padded_by(ws))
        // Repeat the segment parser so a brace-led right-hand side keeps
        // scanning past the block, as in `var x = { 1 } + 2`. A statement
        // alternative consumes a whole `if`/`for`/`skip`, while the flat
        // expression loop consumes operator tokens, and both stop at the rule
        // body's `,` or `.`.
        .then(statement.repeated().at_least(1))
        .ignored()
}
