//! Scanner for index declarations.
//!
//! Parses `index` statements, capturing their spans and any syntax errors.

use chumsky::prelude::*;

use crate::parser::lexer_helpers::token_display;
use crate::{Span, SyntaxKind};

use super::utils::State;

type ScanResult<T> = Result<T, Box<Simple<SyntaxKind>>>;

pub(crate) const MISSING_INDEX_FIELD_LIST: &str =
    "index declarations require a typed field list `(name: Type, ...)` before `on`";
pub(crate) const INVALID_INDEX_FIELD_LIST: &str =
    "index declarations require one or more typed fields in the form `name: Type`";

pub(crate) fn collect_index_spans(
    tokens: &[(SyntaxKind, Span)],
    src: &str,
) -> (Vec<Span>, Vec<Simple<SyntaxKind>>) {
    let mut st: State<'_> = State::new(tokens, src, Vec::new());

    let handle_index = |st: &mut State<'_>, span: Span| {
        let start = span.start;
        match parse_index_decl(st.stream.tokens(), st.stream.src(), st.stream.cursor()) {
            Ok(end) => {
                st.spans.push(start..end);
                st.stream.skip_until(end);
            }
            Err(error) => {
                st.extra.push(*error);
                st.skip_line();
            }
        }
    };

    token_dispatch!(st, {
        SyntaxKind::K_INDEX => handle_index,
    });

    st.into_parts()
}

fn parse_index_decl(
    tokens: &[(SyntaxKind, Span)],
    src: &str,
    mut cursor: usize,
) -> ScanResult<usize> {
    consume_kind(tokens, src, &mut cursor, SyntaxKind::K_INDEX)?;
    skip_trivia(tokens, &mut cursor);
    consume_kind(tokens, src, &mut cursor, SyntaxKind::T_IDENT)?;
    skip_trivia(tokens, &mut cursor);
    parse_index_fields(tokens, src, &mut cursor)?;
    skip_trivia(tokens, &mut cursor);
    consume_kind(tokens, src, &mut cursor, SyntaxKind::K_ON)?;
    skip_trivia(tokens, &mut cursor);
    let mut end = parse_index_target(tokens, src, &mut cursor)?;
    while let Some((kind, span)) = tokens.get(cursor) {
        if !is_trivia(*kind) {
            break;
        }
        end = span.end;
        cursor += 1;
    }
    Ok(end)
}

fn parse_index_fields(
    tokens: &[(SyntaxKind, Span)],
    src: &str,
    cursor: &mut usize,
) -> ScanResult<()> {
    match tokens.get(*cursor) {
        Some((SyntaxKind::K_ON, span)) => {
            return Err(Box::new(Simple::custom(
                span.clone(),
                MISSING_INDEX_FIELD_LIST,
            )));
        }
        Some((SyntaxKind::T_LPAREN, _)) => {}
        Some((found, span)) => {
            return Err(Box::new(Simple::expected_input_found(
                span.clone(),
                [Some(SyntaxKind::T_LPAREN)],
                Some(*found),
            )));
        }
        None => return Err(Box::new(expected_eof(src, SyntaxKind::T_LPAREN))),
    }

    *cursor += 1;
    skip_trivia(tokens, cursor);

    if matches!(tokens.get(*cursor), Some((SyntaxKind::T_RPAREN, _))) {
        return Err(Box::new(Simple::custom(
            current_span(tokens, src, *cursor),
            INVALID_INDEX_FIELD_LIST,
        )));
    }

    loop {
        consume_kind_or_invalid(tokens, src, cursor, SyntaxKind::T_IDENT)?;
        skip_trivia(tokens, cursor);
        if !matches!(tokens.get(*cursor), Some((SyntaxKind::T_COLON, _))) {
            return Err(Box::new(Simple::custom(
                current_span(tokens, src, *cursor),
                INVALID_INDEX_FIELD_LIST,
            )));
        }
        *cursor += 1;
        skip_trivia(tokens, cursor);
        parse_type_expr(tokens, src, cursor)?;
        skip_trivia(tokens, cursor);

        match tokens.get(*cursor) {
            Some((SyntaxKind::T_COMMA, _)) => {
                *cursor += 1;
                skip_trivia(tokens, cursor);
                if matches!(tokens.get(*cursor), Some((SyntaxKind::T_RPAREN, _))) {
                    return Err(Box::new(Simple::custom(
                        current_span(tokens, src, *cursor),
                        INVALID_INDEX_FIELD_LIST,
                    )));
                }
            }
            Some((SyntaxKind::T_RPAREN, _)) => {
                *cursor += 1;
                return Ok(());
            }
            _ => {
                return Err(Box::new(Simple::custom(
                    current_span(tokens, src, *cursor),
                    INVALID_INDEX_FIELD_LIST,
                )));
            }
        }
    }
}

#[derive(Default)]
struct TypeDepths {
    paren: usize,
    bracket: usize,
    brace: usize,
    angle: usize,
}

impl TypeDepths {
    fn at_top_level(&self) -> bool {
        self.paren == 0 && self.bracket == 0 && self.brace == 0 && self.angle == 0
    }
}

fn parse_type_expr(tokens: &[(SyntaxKind, Span)], src: &str, cursor: &mut usize) -> ScanResult<()> {
    let mut consumed = false;
    let mut depths = TypeDepths::default();

    while let Some((kind, _span)) = tokens.get(*cursor) {
        if is_trivia(*kind) {
            *cursor += 1;
            continue;
        }

        if depths.at_top_level() && matches!(kind, SyntaxKind::T_COMMA | SyntaxKind::T_RPAREN) {
            break;
        }

        consumed = true;
        adjust_type_depths(*kind, &mut depths);
        *cursor += 1;
    }

    if consumed {
        Ok(())
    } else {
        Err(Box::new(Simple::custom(
            current_span(tokens, src, *cursor),
            INVALID_INDEX_FIELD_LIST,
        )))
    }
}

fn parse_index_target(
    tokens: &[(SyntaxKind, Span)],
    src: &str,
    cursor: &mut usize,
) -> ScanResult<usize> {
    if matches!(tokens.get(*cursor), Some((SyntaxKind::T_AMP, _))) {
        *cursor += 1;
        skip_trivia(tokens, cursor);
    }

    let mut end = consume_kind_or_invalid(tokens, src, cursor, SyntaxKind::T_IDENT)?;
    skip_trivia(tokens, cursor);

    while let Some((kind, _)) = tokens.get(*cursor) {
        if !matches!(kind, SyntaxKind::T_COLON_COLON | SyntaxKind::T_DOT) {
            break;
        }
        *cursor += 1;
        skip_trivia(tokens, cursor);
        end = consume_kind_or_invalid(tokens, src, cursor, SyntaxKind::T_IDENT)?;
        skip_trivia(tokens, cursor);
    }

    if let Some((SyntaxKind::T_APOSTROPHE, span)) = tokens.get(*cursor) {
        end = span.end;
        *cursor += 1;
        skip_trivia(tokens, cursor);
    }

    if let Some((kind, _)) = tokens.get(*cursor)
        && matches!(
            kind,
            SyntaxKind::T_LPAREN | SyntaxKind::T_LBRACKET | SyntaxKind::T_LBRACE
        )
    {
        end = parse_balanced_arguments(tokens, src, cursor)?;
        skip_trivia(tokens, cursor);
    }

    if matches!(tokens.get(*cursor), Some((SyntaxKind::T_MINUS, _))) {
        end = parse_delay_suffix(tokens, src, cursor)?;
    }

    Ok(end)
}

fn parse_delay_suffix(
    tokens: &[(SyntaxKind, Span)],
    src: &str,
    cursor: &mut usize,
) -> ScanResult<usize> {
    consume_kind(tokens, src, cursor, SyntaxKind::T_MINUS)?;
    skip_trivia(tokens, cursor);
    consume_kind(tokens, src, cursor, SyntaxKind::T_LT)?;
    skip_trivia(tokens, cursor);
    consume_kind_or_invalid(tokens, src, cursor, SyntaxKind::T_NUMBER)?;
    skip_trivia(tokens, cursor);
    consume_kind(tokens, src, cursor, SyntaxKind::T_GT)
}

fn parse_balanced_arguments(
    tokens: &[(SyntaxKind, Span)],
    src: &str,
    cursor: &mut usize,
) -> ScanResult<usize> {
    let Some((open_kind, open_span)) = tokens.get(*cursor) else {
        return Err(Box::new(expected_eof(src, SyntaxKind::T_LPAREN)));
    };
    let expected_close = match open_kind {
        SyntaxKind::T_LPAREN => SyntaxKind::T_RPAREN,
        SyntaxKind::T_LBRACKET => SyntaxKind::T_RBRACKET,
        SyntaxKind::T_LBRACE => SyntaxKind::T_RBRACE,
        _ => {
            return Err(Box::new(Simple::custom(
                open_span.clone(),
                "expected index target arguments",
            )));
        }
    };

    let mut stack = vec![expected_close];
    *cursor += 1;

    while let Some((kind, span)) = tokens.get(*cursor) {
        match kind {
            SyntaxKind::T_LPAREN => stack.push(SyntaxKind::T_RPAREN),
            SyntaxKind::T_LBRACKET => stack.push(SyntaxKind::T_RBRACKET),
            SyntaxKind::T_LBRACE => stack.push(SyntaxKind::T_RBRACE),
            SyntaxKind::T_LT => stack.push(SyntaxKind::T_GT),
            SyntaxKind::T_SHL => {
                stack.push(SyntaxKind::T_GT);
                stack.push(SyntaxKind::T_GT);
            }
            SyntaxKind::T_RPAREN
            | SyntaxKind::T_RBRACKET
            | SyntaxKind::T_RBRACE
            | SyntaxKind::T_GT
            | SyntaxKind::T_SHR => {
                let closes = if *kind == SyntaxKind::T_SHR { 2 } else { 1 };
                for _ in 0..closes {
                    match stack.pop() {
                        Some(expected) if matches_closing_token(*kind, expected) => {}
                        Some(expected) => {
                            return Err(Box::new(Simple::custom(
                                span.clone(),
                                format!(
                                    "expected '{}' before '{}'",
                                    token_display(expected),
                                    token_display(*kind)
                                ),
                            )));
                        }
                        None => break,
                    }
                }
                if stack.is_empty() {
                    *cursor += 1;
                    return Ok(span.end);
                }
            }
            _ => {}
        }
        *cursor += 1;
    }

    Err(Box::new(Simple::custom(
        src.len()..src.len(),
        format!("unclosed '{}'", token_display(expected_close)),
    )))
}

fn consume_kind(
    tokens: &[(SyntaxKind, Span)],
    src: &str,
    cursor: &mut usize,
    expected: SyntaxKind,
) -> ScanResult<usize> {
    match tokens.get(*cursor) {
        Some((found, span)) if *found == expected => {
            *cursor += 1;
            Ok(span.end)
        }
        Some((found, span)) => Err(Box::new(Simple::expected_input_found(
            span.clone(),
            [Some(expected)],
            Some(*found),
        ))),
        None => Err(Box::new(expected_eof(src, expected))),
    }
}

fn consume_kind_or_invalid(
    tokens: &[(SyntaxKind, Span)],
    src: &str,
    cursor: &mut usize,
    expected: SyntaxKind,
) -> ScanResult<usize> {
    match tokens.get(*cursor) {
        Some((found, span)) if *found == expected => {
            *cursor += 1;
            Ok(span.end)
        }
        Some((_, span)) => Err(Box::new(Simple::custom(
            span.clone(),
            INVALID_INDEX_FIELD_LIST,
        ))),
        None => Err(Box::new(Simple::custom(
            src.len()..src.len(),
            INVALID_INDEX_FIELD_LIST,
        ))),
    }
}

fn skip_trivia(tokens: &[(SyntaxKind, Span)], cursor: &mut usize) {
    while let Some((kind, _)) = tokens.get(*cursor) {
        if !is_trivia(*kind) {
            break;
        }
        *cursor += 1;
    }
}

fn is_trivia(kind: SyntaxKind) -> bool {
    matches!(kind, SyntaxKind::T_WHITESPACE | SyntaxKind::T_COMMENT)
}

fn adjust_type_depths(kind: SyntaxKind, depths: &mut TypeDepths) {
    match kind {
        SyntaxKind::T_LPAREN => depths.paren += 1,
        SyntaxKind::T_RPAREN => depths.paren = depths.paren.saturating_sub(1),
        SyntaxKind::T_LBRACKET => depths.bracket += 1,
        SyntaxKind::T_RBRACKET => depths.bracket = depths.bracket.saturating_sub(1),
        SyntaxKind::T_LBRACE => depths.brace += 1,
        SyntaxKind::T_RBRACE => depths.brace = depths.brace.saturating_sub(1),
        SyntaxKind::T_LT => depths.angle += 1,
        SyntaxKind::T_GT => depths.angle = depths.angle.saturating_sub(1),
        SyntaxKind::T_SHL => depths.angle += 2,
        SyntaxKind::T_SHR => depths.angle = depths.angle.saturating_sub(2),
        _ => {}
    }
}

fn matches_closing_token(found: SyntaxKind, expected: SyntaxKind) -> bool {
    match found {
        SyntaxKind::T_SHR => expected == SyntaxKind::T_GT,
        _ => found == expected,
    }
}

fn current_span(tokens: &[(SyntaxKind, Span)], src: &str, cursor: usize) -> Span {
    tokens
        .get(cursor)
        .map_or(src.len()..src.len(), |(_, span)| span.clone())
}

fn expected_eof(src: &str, expected: SyntaxKind) -> Simple<SyntaxKind> {
    Simple::expected_input_found(src.len()..src.len(), [Some(expected)], None)
}
