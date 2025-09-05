//! Lexical analysis for `DDlog` source.
//!
//! This module exposes `tokenize_with_trivia` and `tokenize_without_trivia`
//! functions which convert raw source text into
//! a sequence of `(SyntaxKind, Span)` pairs. It uses the `logos` crate to
//! recognise tokens so that the CST can mirror the input exactly.

use logos::Logos;
use phf::phf_map;

use crate::SyntaxKind;

/// Byte range for a token within the source.
pub type Span = std::ops::Range<usize>;

#[derive(Logos, Debug, Clone, Copy, PartialEq, Eq)]
enum Token {
    #[regex(r"[ \t\r\n]+")]
    Whitespace,
    #[regex(r"/\*([^*]|\*[^/])*\*/", priority = 2)]
    #[regex(r"//[^\n]*")]
    Comment,
    #[regex(r"[A-Za-z_][A-Za-z0-9_]*")]
    Ident,
    #[regex(r"0[xX][0-9a-fA-F]+|0[bB][01]+|0[oO][0-7]+|[0-9]+(?:\.[0-9]+)?(?:[eE][+-]?[0-9]+)?")]
    Number,
    #[regex(r#""([^"\\]|\\.)*""#)]
    String,
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,
    #[token("[")]
    LBracket,
    #[token("]")]
    RBracket,
    #[token(";")]
    Semi,
    #[token(",")]
    Comma,
    #[token(".")]
    Dot,
    #[token("::")]
    ColonColon,
    #[token(":")]
    Colon,
    #[token("|")]
    Pipe,
    #[token("&")]
    Amp,
    #[token("==")]
    EqEq,
    #[token("=")]
    Eq,
    #[token(":-")]
    Implies,
    #[token("%")]
    Percent,
    #[token("*")]
    Star,
    #[token("/")]
    Slash,
    #[token("++")]
    PlusPlus,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("->")]
    Arrow,
    #[token("=>")]
    FatArrow,
    #[token("<=")]
    Lte,
    #[token("<=>")]
    Spaceship,
    #[token(">=")]
    Gte,
    #[token("<")]
    Lt,
    #[token(">")]
    Gt,
    #[token("!=")]
    Neq,
    #[token(">>")]
    Shr,
    #[token("<<")]
    Shl,
    #[token("?")]
    Question,
    #[token("~")]
    Tilde,
    #[token("@")]
    At,
    #[token("#")]
    Hash,
    #[token("'")]
    Apostrophe,
}

/// Maps identifier strings to their keyword `SyntaxKind`.
///
/// Returns `Some(kind)` if `ident` is a recognised `DDlog` keyword, or `None`
/// otherwise. A static map avoids a long match statement and allows O(1)
/// lookups.
static KEYWORDS: phf::Map<&'static str, SyntaxKind> = phf_map! {
    "abstract" => SyntaxKind::K_ABSTRACT,
    "Aggregate" => SyntaxKind::K_AGGREGATE,
    "and" => SyntaxKind::K_AND,
    "apply" => SyntaxKind::K_APPLY,
    "as" => SyntaxKind::K_AS,
    "async" => SyntaxKind::K_ASYNC,
    "await" => SyntaxKind::K_AWAIT,
    "become" => SyntaxKind::K_BECOME,
    "bigint" => SyntaxKind::K_BIGINT,
    "bit" => SyntaxKind::K_BIT,
    "bool" => SyntaxKind::K_BOOL,
    "box" => SyntaxKind::K_BOX,
    "break" => SyntaxKind::K_BREAK,
    "const" => SyntaxKind::K_CONST,
    "continue" => SyntaxKind::K_CONTINUE,
    "crate" => SyntaxKind::K_CRATE,
    "do" => SyntaxKind::K_DO,
    "double" => SyntaxKind::K_DOUBLE,
    "dyn" => SyntaxKind::K_DYN,
    "else" => SyntaxKind::K_ELSE,
    "extern" => SyntaxKind::K_EXTERN,
    "false" => SyntaxKind::K_FALSE,
    "final" => SyntaxKind::K_FINAL,
    "fn" => SyntaxKind::K_FN,
    "FlatMap" => SyntaxKind::K_FLATMAP,
    "float" => SyntaxKind::K_FLOAT,
    "for" => SyntaxKind::K_FOR,
    "function" => SyntaxKind::K_FUNCTION,
    "if" => SyntaxKind::K_IF,
    "impl" => SyntaxKind::K_IMPL,
    "import" => SyntaxKind::K_IMPORT,
    "in" => SyntaxKind::K_IN,
    "index" => SyntaxKind::K_INDEX,
    "input" => SyntaxKind::K_INPUT,
    "Inspect" => SyntaxKind::K_INSPECT,
    "let" => SyntaxKind::K_LET,
    "loop" => SyntaxKind::K_LOOP,
    "macro" => SyntaxKind::K_MACRO,
    "match" => SyntaxKind::K_MATCH,
    "mod" => SyntaxKind::K_MOD,
    "move" => SyntaxKind::K_MOVE,
    "multiset" => SyntaxKind::K_MULTISET,
    "mut" => SyntaxKind::K_MUT,
    "on" => SyntaxKind::K_ON,
    "not" => SyntaxKind::K_NOT,
    "or" => SyntaxKind::K_OR,
    "override" => SyntaxKind::K_OVERRIDE,
    "output" => SyntaxKind::K_OUTPUT,
    "priv" => SyntaxKind::K_PRIV,
    "pub" => SyntaxKind::K_PUB,
    "ref" => SyntaxKind::K_REF,
    "relation" => SyntaxKind::K_RELATION,
    "return" => SyntaxKind::K_RETURN,
    "self" => SyntaxKind::K_SELF,
    "Self" => SyntaxKind::K_SELF_TYPE,
    "signed" => SyntaxKind::K_SIGNED,
    "skip" => SyntaxKind::K_SKIP,
    "static" => SyntaxKind::K_STATIC,
    "stream" => SyntaxKind::K_STREAM,
    "struct" => SyntaxKind::K_STRUCT,
    "super" => SyntaxKind::K_SUPER,
    "trait" => SyntaxKind::K_TRAIT,
    "transformer" => SyntaxKind::K_TRANSFORMER,
    "try" => SyntaxKind::K_TRY,
    "true" => SyntaxKind::K_TRUE,
    "type" => SyntaxKind::K_TYPE,
    "typedef" => SyntaxKind::K_TYPEDEF,
    "typeof" => SyntaxKind::K_TYPEOF,
    "_" => SyntaxKind::K_UNDERSCORE,
    "unsafe" => SyntaxKind::K_UNSAFE,
    "unsized" => SyntaxKind::K_UNSIZED,
    "use" => SyntaxKind::K_USE,
    "var" => SyntaxKind::K_VAR,
    "virtual" => SyntaxKind::K_VIRTUAL,
    "where" => SyntaxKind::K_WHERE,
    "while" => SyntaxKind::K_WHILE,
    "yield" => SyntaxKind::K_YIELD,
};

fn keyword_kind(ident: &str) -> Option<SyntaxKind> {
    KEYWORDS.get(ident).copied()
}

#[must_use]
fn tokenize_impl(src: &str) -> Vec<(SyntaxKind, Span)> {
    let mut lexer = Token::lexer(src);
    #[expect(
        clippy::integer_division,
        clippy::integer_division_remainder_used,
        reason = "rough capacity estimate"
    )]
    let estimated_tokens = src.len() / 4; // roughly four chars per token
    let mut out = Vec::with_capacity(estimated_tokens);
    while let Some(result) = lexer.next() {
        let span = lexer.span();
        #[expect(clippy::expect_used, reason = "invalid span indicates lexer bug")]
        let text = src.get(span.clone()).expect("lexer produced invalid span");
        let Ok(token) = result else {
            out.push((SyntaxKind::N_ERROR, span));
            continue;
        };
        let kind = match token {
            Token::Whitespace => SyntaxKind::T_WHITESPACE,
            Token::Comment => SyntaxKind::T_COMMENT,
            Token::Ident => keyword_kind(text).unwrap_or(SyntaxKind::T_IDENT),
            Token::Number => SyntaxKind::T_NUMBER,
            Token::String => SyntaxKind::T_STRING,
            Token::LParen => SyntaxKind::T_LPAREN,
            Token::RParen => SyntaxKind::T_RPAREN,
            Token::LBrace => SyntaxKind::T_LBRACE,
            Token::RBrace => SyntaxKind::T_RBRACE,
            Token::LBracket => SyntaxKind::T_LBRACKET,
            Token::RBracket => SyntaxKind::T_RBRACKET,
            Token::Semi => SyntaxKind::T_SEMI,
            Token::Comma => SyntaxKind::T_COMMA,
            Token::Dot => SyntaxKind::T_DOT,
            Token::ColonColon => SyntaxKind::T_COLON_COLON,
            Token::Colon => SyntaxKind::T_COLON,
            Token::Pipe => SyntaxKind::T_PIPE,
            Token::Amp => SyntaxKind::T_AMP,
            Token::EqEq => SyntaxKind::T_EQEQ,
            Token::Eq => SyntaxKind::T_EQ,
            Token::Implies => SyntaxKind::T_IMPLIES,
            Token::Percent => SyntaxKind::T_PERCENT,
            Token::Star => SyntaxKind::T_STAR,
            Token::Slash => SyntaxKind::T_SLASH,
            Token::PlusPlus => SyntaxKind::T_PLUSPLUS,
            Token::Plus => SyntaxKind::T_PLUS,
            Token::Minus => SyntaxKind::T_MINUS,
            Token::Arrow => SyntaxKind::T_ARROW,
            Token::FatArrow => SyntaxKind::T_FAT_ARROW,
            Token::Lte => SyntaxKind::T_LTE,
            Token::Spaceship => SyntaxKind::T_SPACESHIP,
            Token::Gte => SyntaxKind::T_GTE,
            Token::Lt => SyntaxKind::T_LT,
            Token::Gt => SyntaxKind::T_GT,
            Token::Neq => SyntaxKind::T_NEQ,
            Token::Shr => SyntaxKind::T_SHR,
            Token::Shl => SyntaxKind::T_SHL,
            Token::Question => SyntaxKind::T_QUESTION,
            Token::Tilde => SyntaxKind::T_TILDE,
            Token::At => SyntaxKind::T_AT,
            Token::Hash => SyntaxKind::T_HASH,
            Token::Apostrophe => SyntaxKind::T_APOSTROPHE,
        };
        out.push((kind, span));
    }
    out
}

/// Tokenise the source, excluding whitespace and comments.
///
/// Returns only significant tokens for use in expression parsing.
///
/// # Examples
///
/// ```rust
/// use ddlint::{tokenize_without_trivia, SyntaxKind};
///
/// let tokens = tokenize_without_trivia("input R(x: u32);");
/// assert!(!tokens.iter().any(|(k, _)| *k == SyntaxKind::T_WHITESPACE));
/// ```
#[must_use]
pub fn tokenize_without_trivia(src: &str) -> Vec<(SyntaxKind, Span)> {
    tokenize_impl(src)
        .into_iter()
        .filter(|(k, _)| !matches!(k, SyntaxKind::T_WHITESPACE | SyntaxKind::T_COMMENT))
        .collect()
}

/// Tokenise the provided `DDlog` source.
///
/// # Examples
///
/// ```rust,no_run
/// use ddlint::{tokenize_with_trivia, SyntaxKind};
///
/// let tokens = tokenize_with_trivia("input relation R(x: u32);");
/// assert_eq!(tokens.len(), 12);
/// assert_eq!(tokens[0].0, SyntaxKind::K_INPUT);
/// ```
///
/// This variant retains whitespace and comment tokens.
#[must_use]
pub fn tokenize_with_trivia(src: &str) -> Vec<(SyntaxKind, Span)> {
    tokenize_impl(src)
}
