//! Parsing helpers for identifiers, struct literals, closures, groupings, and
//! collection literals.

use super::pratt::Pratt;
use crate::{
    Span,
    SyntaxKind,
    parser::{
        ast::Expr,
        error_messages::{
            EMPTY_VECTOR_LITERAL_ERROR,
            MISSING_MAP_ARROW_ERROR,
            MIXED_COLLECTION_ENTRY_ERROR,
        },
    },
};

/// Shape of a comma-separated identifier list.
///
/// The terminator, diagnostic text, and trailing-comma policy always travel
/// together, so grouping them keeps the list parser's argument count small.
#[derive(Clone, Copy)]
struct IdentifierListOptions {
    terminator: SyntaxKind,
    err_msg: &'static str,
    allow_trailing_comma: bool,
}

impl<I> Pratt<'_, I>
where
    I: Iterator<Item = (SyntaxKind, Span)> + Clone,
{
    pub(super) fn parse_identifier_or_struct(&mut self, span: &Span) -> Option<Expr> {
        let (name, full_span) = self.parse_scoped_identifier(span)?;
        self.parse_ident_expression(name, &full_span)
    }

    pub(super) fn parse_ident_expression(&mut self, name: String, span: &Span) -> Option<Expr> {
        if !matches!(self.ts.peek_kind(), Some(SyntaxKind::T_LBRACE)) {
            return Some(Expr::Variable(name));
        }

        if !self.allows_struct_literals() {
            let next_kind = self.ts.peek_nth_kind(1);
            let after_next_kind = self.ts.peek_nth_kind(2);
            // `ident {}` is only suspicious when another `{` follows, as in
            // `if x == Foo{} { a }`. Without that lookahead `if ready {}`
            // must fall through to the branch block.
            let brace_brace = matches!(next_kind, Some(SyntaxKind::T_RBRACE))
                && matches!(after_next_kind, Some(SyntaxKind::T_LBRACE));
            let looks_like_struct =
                brace_brace || matches!(after_next_kind, Some(SyntaxKind::T_COLON));
            if looks_like_struct {
                self.ts.push_error(
                    span.clone(),
                    "struct literal syntax is not allowed in this context",
                );
            }
            return Some(Expr::Variable(name));
        }

        self.ts.next_tok();
        let fields = self.parse_struct_fields()?;
        if !self.ts.expect(SyntaxKind::T_RBRACE) {
            return None;
        }
        Some(Expr::Struct { name, fields })
    }

    pub(super) fn parse_parenthesized_expr(&mut self) -> Option<Expr> {
        if matches!(self.ts.peek_kind(), Some(SyntaxKind::T_RPAREN)) {
            self.ts.next_tok();
            return Some(Expr::Tuple(Vec::new()));
        }
        self.with_nested_delimiter_scope(|this| {
            let (mut items, is_tuple) = this.parse_parenthesized_items()?;
            if !this.ts.expect(SyntaxKind::T_RPAREN) {
                return None;
            }
            if is_tuple {
                Some(Expr::Tuple(items))
            } else {
                // Parser invariant: the group contains exactly one item, so
                // `pop` always succeeds; propagate `None` rather than panic if
                // the invariant is ever broken.
                let item = items.pop()?;
                Some(Expr::Group(Box::new(item)))
            }
        })
    }

    /// Parse the comma-separated items of a parenthesised expression.
    ///
    /// Returns the parsed items and whether a comma was seen, which decides
    /// between a tuple and a single-item group.
    fn parse_parenthesized_items(&mut self) -> Option<(Vec<Expr>, bool)> {
        let first = self.parse_expr(0)?;
        let mut items = vec![first];
        let mut is_tuple = false;
        while matches!(self.ts.peek_kind(), Some(SyntaxKind::T_COMMA)) {
            is_tuple = true;
            self.ts.next_tok();
            if matches!(self.ts.peek_kind(), Some(SyntaxKind::T_RPAREN)) {
                break;
            }
            items.push(self.parse_expr(0)?);
        }
        Some((items, is_tuple))
    }

    /// Parse a bracket literal: a vector `[e1, …]` or a map `[k1 -> v1, …]`.
    ///
    /// Both forms require at least one item and allow a trailing comma,
    /// mirroring upstream `evec_literal` and `emap_literal`. The form is chosen
    /// by whether the first item is followed by `->`; a literal that mixes the
    /// two forms is rejected at the first offending item. The opening bracket
    /// has already been consumed by the prefix dispatcher.
    pub(super) fn parse_vector_literal(&mut self) -> Option<Expr> {
        if matches!(self.ts.peek_kind(), Some(SyntaxKind::T_RBRACKET)) {
            let span = self.ts.peek_span().unwrap_or_else(|| self.ts.eof_span());
            self.ts.push_error(span, EMPTY_VECTOR_LITERAL_ERROR);
            return None;
        }

        self.with_nested_delimiter_scope(|this| {
            let first = this.parse_expr(0)?;
            if matches!(this.ts.peek_kind(), Some(SyntaxKind::T_ARROW)) {
                this.parse_bracket_map_entries(first)
            } else {
                this.parse_bracket_vector_items(first)
            }
        })
    }

    /// Parse the remaining elements of a bracket vector after its first item.
    ///
    /// A later item followed by `->` would mix the two literal forms, which
    /// upstream cannot express; report it at that item's first token.
    fn parse_bracket_vector_items(&mut self, first: Expr) -> Option<Expr> {
        let mut items = vec![first];
        while matches!(self.ts.peek_kind(), Some(SyntaxKind::T_COMMA)) {
            self.ts.next_tok();
            if matches!(self.ts.peek_kind(), Some(SyntaxKind::T_RBRACKET)) {
                break;
            }
            let item_start = self.ts.peek_span().unwrap_or_else(|| self.ts.eof_span());
            items.push(self.parse_expr(0)?);
            if matches!(self.ts.peek_kind(), Some(SyntaxKind::T_ARROW)) {
                self.ts.push_error(item_start, MIXED_COLLECTION_ENTRY_ERROR);
                return None;
            }
        }
        if !self.ts.expect(SyntaxKind::T_RBRACKET) {
            return None;
        }
        Some(Expr::VecLit(items))
    }

    /// Parse the remaining entries of a bracket map after its first key.
    ///
    /// The caller has just seen, but not consumed, the `->` after the first
    /// key.
    fn parse_bracket_map_entries(&mut self, first_key: Expr) -> Option<Expr> {
        self.ts.next_tok(); // consume the '->' the caller saw
        let first_value = self.parse_expr(0)?;
        let mut entries = vec![(first_key, first_value)];

        while matches!(self.ts.peek_kind(), Some(SyntaxKind::T_COMMA)) {
            self.ts.next_tok();
            if matches!(self.ts.peek_kind(), Some(SyntaxKind::T_RBRACKET)) {
                break;
            }
            entries.push(self.parse_bracket_map_entry()?);
        }

        if !self.ts.expect(SyntaxKind::T_RBRACKET) {
            return None;
        }
        Some(Expr::MapLit(entries))
    }

    /// Parse one comma-separated map entry, starting at its key.
    ///
    /// `None` means the key is not followed by `->`; the diagnostic has
    /// already been reported at the entry's first token.
    fn parse_bracket_map_entry(&mut self) -> Option<(Expr, Expr)> {
        let item_start = self.ts.peek_span().unwrap_or_else(|| self.ts.eof_span());
        let key = self.parse_expr(0)?;
        self.parse_map_arrow(item_start)?;
        let value = self.parse_expr(0)?;
        Some((key, value))
    }

    /// Consume the `->` that separates a map key from its value.
    ///
    /// `None` means the arrow is missing, after reporting why at `item_start`.
    /// A comma or `]` completes the item as a vector element, so the literal
    /// mixes the two forms; anything else leaves the map entry itself
    /// unfinished, with no second item to mix.
    fn parse_map_arrow(&mut self, item_start: Span) -> Option<()> {
        let next = self.ts.peek_kind();
        if matches!(next, Some(SyntaxKind::T_ARROW)) {
            self.ts.next_tok();
            return Some(());
        }
        let message = if matches!(next, Some(SyntaxKind::T_COMMA | SyntaxKind::T_RBRACKET)) {
            MIXED_COLLECTION_ENTRY_ERROR
        } else {
            MISSING_MAP_ARROW_ERROR
        };
        self.ts.push_error(item_start, message);
        None
    }

    pub(super) fn parse_closure_literal(&mut self) -> Option<Expr> {
        let params = self.parse_closure_params()?;
        if !self.ts.expect(SyntaxKind::T_PIPE) {
            return None;
        }
        let body = self.with_struct_literals_suspended(|this| {
            this.with_closure_body_scope(|closure| closure.parse_expr(0))
        })?;
        Some(Expr::Closure {
            params,
            body: Box::new(body),
        })
    }

    fn parse_closure_params(&mut self) -> Option<Vec<String>> {
        self.parse_comma_separated_identifiers(
            IdentifierListOptions {
                terminator: SyntaxKind::T_PIPE,
                err_msg: "expected parameter name",
                allow_trailing_comma: true,
            },
            |_parser, name| Some(name),
        )
    }

    fn parse_struct_fields(&mut self) -> Option<Vec<(String, Expr)>> {
        self.parse_comma_separated_identifiers(
            IdentifierListOptions {
                terminator: SyntaxKind::T_RBRACE,
                err_msg: "expected field name",
                allow_trailing_comma: true,
            },
            |parser, name| {
                if !parser.ts.expect(SyntaxKind::T_COLON) {
                    return None;
                }
                let value = parser.with_nested_delimiter_scope(|p| p.parse_expr(0))?;
                Some((name, value))
            },
        )
    }

    /// Parse a comma-separated list of identifiers up to (but not consuming) the
    /// configured terminator.
    ///
    /// Returns an empty vector if the next token is the terminator. Callers must
    /// consume the terminator.
    fn parse_comma_separated_identifiers<T>(
        &mut self,
        options: IdentifierListOptions,
        mut process_item: impl FnMut(&mut Self, String) -> Option<T>,
    ) -> Option<Vec<T>> {
        let mut items = Vec::new();
        if self.ts.peek_kind() == Some(options.terminator) {
            return Some(items);
        }
        loop {
            let name = self.take_list_identifier(options.err_msg)?;
            items.push(process_item(self, name)?);
            if !self.consume_list_separator(options) {
                break;
            }
        }
        Some(items)
    }

    /// Take the next token as a list item name.
    ///
    /// Returns the identifier text. A non-identifier token records `err_msg`
    /// against its span and returns `None`; at end of input it returns `None`
    /// without recording a diagnostic. Scope: the identifier-list parser only;
    /// other lists report their own diagnostics.
    fn take_list_identifier(&mut self, err_msg: &'static str) -> Option<String> {
        let (kind, span) = self.ts.next_tok()?;
        if kind != SyntaxKind::T_IDENT {
            self.ts.push_error(span, err_msg);
            return None;
        }
        Some(self.ts.slice(&span))
    }

    /// Consume the comma after a list item, if there is one.
    ///
    /// Returns `true` when another item should follow, and `false` at the end
    /// of the list: no comma, or a trailing comma before the terminator when
    /// `options.allow_trailing_comma` is set. Scope: the identifier-list parser
    /// only.
    fn consume_list_separator(&mut self, options: IdentifierListOptions) -> bool {
        if self.ts.peek_kind() != Some(SyntaxKind::T_COMMA) {
            return false;
        }
        self.ts.next_tok();
        let trailing =
            options.allow_trailing_comma && self.ts.peek_kind() == Some(options.terminator);
        !trailing
    }

    fn parse_scoped_identifier(&mut self, first_span: &Span) -> Option<(String, Span)> {
        let mut name = self.ts.slice(first_span);
        let mut full_span = first_span.clone();

        while matches!(self.ts.peek_kind(), Some(SyntaxKind::T_COLON_COLON)) {
            self.ts.next_tok();

            let Some((next_kind, next_span)) = self.ts.next_tok() else {
                let eof = self.ts.eof_span();
                self.ts.push_error(eof, "expected identifier after '::'");
                return None;
            };

            if next_kind != SyntaxKind::T_IDENT {
                self.ts
                    .push_error(next_span, "expected identifier after '::'");
                return None;
            }

            name.push_str("::");
            name.push_str(&self.ts.slice(&next_span));
            full_span.end = next_span.end;
        }

        Some((name, full_span))
    }
}
