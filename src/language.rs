//! `rowan` integration and `DDlog` syntax kinds.
//!
//! This module defines the `SyntaxKind` enum covering all tokens and
//! non-terminal nodes in the `DDlog` grammar.  The enumeration is used by
//! `rowan` to tag syntax tree elements.  The `DdlogLanguage` newtype
//! implements `rowan::Language` using conversions provided by
//! `num_derive`.

use num_derive::{FromPrimitive as FromPrimitiveDerive, ToPrimitive as ToPrimitiveDerive};
use num_traits::FromPrimitive;
use rowan::Language as RowanLanguage;
use rowan::SyntaxKind as RowanSyntaxKind;

/// Every possible token or node in the `DDlog` syntax tree.
#[derive(
    Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, FromPrimitiveDerive, ToPrimitiveDerive,
)]
#[repr(u16)]
#[expect(non_camel_case_types, reason = "token naming follows design spec")]
pub enum SyntaxKind {
    // Tokens
    /// A line or block comment.
    T_COMMENT,
    /// A run of whitespace.
    T_WHITESPACE,
    /// An identifier.
    T_IDENT,
    /// A string literal, quoted or raw.
    T_STRING,
    /// A numeric literal.
    T_NUMBER,
    /// The `(` token.
    T_LPAREN,
    /// The `)` token.
    T_RPAREN,
    /// The `{` token.
    T_LBRACE,
    /// The `}` token.
    T_RBRACE,
    /// The `[` token.
    T_LBRACKET,
    /// The `]` token.
    T_RBRACKET,
    /// The `;` token.
    T_SEMI,
    /// The `,` token.
    T_COMMA,
    /// The `.` token.
    T_DOT,
    /// The `:` token.
    T_COLON,
    /// The `::` token.
    T_COLON_COLON,
    /// The `|` token.
    T_PIPE,
    /// The `&` token.
    T_AMP,
    /// The `^` token.
    T_CARET,
    /// The `==` token.
    T_EQEQ,
    /// The `=` token.
    T_EQ,
    /// The `:-` token, which separates a rule head from its body.
    T_IMPLIES,
    /// The `%` token.
    T_PERCENT,
    /// The `*` token.
    T_STAR,
    /// The `/` token.
    T_SLASH,
    /// The `++` token.
    T_PLUSPLUS,
    /// The `+` token.
    T_PLUS,
    /// The `-` token.
    T_MINUS,
    /// The `->` token.
    T_ARROW,
    /// The `=>` token.
    T_FAT_ARROW,
    /// The `<=` token.
    T_LTE,
    /// The `<=>` token.
    T_SPACESHIP,
    /// The `>=` token.
    T_GTE,
    /// The `<` token.
    T_LT,
    /// The `>` token.
    T_GT,
    /// The `!=` token.
    T_NEQ,
    /// The `>>` token.
    T_SHR,
    /// The `<<` token.
    T_SHL,
    /// The `?` token.
    T_QUESTION,
    /// The `~` token.
    T_TILDE,
    /// The `@` token.
    T_AT,
    /// The `#` token.
    T_HASH,
    /// The `'` token.
    T_APOSTROPHE,
    // Keywords
    /// The `abstract` keyword.
    K_ABSTRACT,
    /// The `Aggregate` keyword.
    K_AGGREGATE,
    /// The `and` keyword.
    K_AND,
    /// The `apply` keyword.
    K_APPLY,
    /// The `as` keyword.
    K_AS,
    /// The `async` keyword.
    K_ASYNC,
    /// The `await` keyword.
    K_AWAIT,
    /// The `become` keyword.
    K_BECOME,
    /// The `bigint` keyword.
    K_BIGINT,
    /// The `bit` keyword.
    K_BIT,
    /// The `bool` keyword.
    K_BOOL,
    /// The `box` keyword.
    K_BOX,
    /// The `break` keyword.
    K_BREAK,
    /// The `const` keyword.
    K_CONST,
    /// The `continue` keyword.
    K_CONTINUE,
    /// The `crate` keyword.
    K_CRATE,
    /// The `do` keyword.
    K_DO,
    /// The `double` keyword.
    K_DOUBLE,
    /// The `dyn` keyword.
    K_DYN,
    /// The `else` keyword.
    K_ELSE,
    /// The `extern` keyword.
    K_EXTERN,
    /// The `false` keyword.
    K_FALSE,
    /// The `final` keyword.
    K_FINAL,
    /// The `fn` keyword.
    K_FN,
    /// The `FlatMap` keyword.
    K_FLATMAP,
    /// The `float` keyword.
    K_FLOAT,
    /// The `for` keyword.
    K_FOR,
    /// The `function` keyword.
    K_FUNCTION,
    /// The `if` keyword.
    K_IF,
    /// The `impl` keyword.
    K_IMPL,
    /// The `import` keyword.
    K_IMPORT,
    /// The `in` keyword.
    K_IN,
    /// The `index` keyword.
    K_INDEX,
    /// The `input` keyword.
    K_INPUT,
    /// The `Inspect` keyword.
    K_INSPECT,
    /// The `let` keyword.
    K_LET,
    /// The `loop` keyword.
    K_LOOP,
    /// The `macro` keyword.
    K_MACRO,
    /// The `match` keyword.
    K_MATCH,
    /// The `mod` keyword.
    K_MOD,
    /// The `move` keyword.
    K_MOVE,
    /// The `multiset` keyword.
    K_MULTISET,
    /// The `mut` keyword.
    K_MUT,
    /// The `on` keyword.
    K_ON,
    /// The `not` keyword.
    K_NOT,
    /// The `or` keyword.
    K_OR,
    /// The `override` keyword.
    K_OVERRIDE,
    /// The `output` keyword.
    K_OUTPUT,
    /// The `priv` keyword.
    K_PRIV,
    /// The `pub` keyword.
    K_PUB,
    /// The `ref` keyword.
    K_REF,
    /// The `relation` keyword.
    K_RELATION,
    /// The `return` keyword.
    K_RETURN,
    /// The `self` keyword.
    K_SELF,
    /// The `Self` keyword.
    K_SELF_TYPE,
    /// The `signed` keyword.
    K_SIGNED,
    /// The `skip` keyword.
    K_SKIP,
    /// The `static` keyword.
    K_STATIC,
    /// The `stream` keyword.
    K_STREAM,
    /// The `struct` keyword.
    K_STRUCT,
    /// The `super` keyword.
    K_SUPER,
    /// The `trait` keyword.
    K_TRAIT,
    /// The `transformer` keyword.
    K_TRANSFORMER,
    /// The `try` keyword.
    K_TRY,
    /// The `true` keyword.
    K_TRUE,
    /// The `type` keyword.
    K_TYPE,
    /// The `typedef` keyword.
    K_TYPEDEF,
    /// The `typeof` keyword.
    K_TYPEOF,
    /// The `_` keyword.
    K_UNDERSCORE,
    /// The `unsafe` keyword.
    K_UNSAFE,
    /// The `unsized` keyword.
    K_UNSIZED,
    /// The `use` keyword.
    K_USE,
    /// The `var` keyword.
    K_VAR,
    /// The `virtual` keyword.
    K_VIRTUAL,
    /// The `where` keyword.
    K_WHERE,
    /// The `while` keyword.
    K_WHILE,
    /// The `yield` keyword.
    K_YIELD,
    // Nodes
    /// An attribute attached to a declaration.
    N_ATTRIBUTE,
    /// Reserved for a field of a struct or constructor; the parser does not emit it yet.
    N_FIELD,
    /// Reserved for a typed argument in a function or transformer signature; the parser does not emit it yet.
    N_ARG_TYPE,
    /// Reserved for an identifier with its position in a relation declaration; the parser does not emit it yet.
    N_IDENTIFIER_WITH_POS,
    /// Reserved for a type expression; the parser does not emit it yet.
    N_TYPE,
    /// A `typedef` declaration.
    N_TYPE_DEF,
    /// A relation declaration.
    N_RELATION_DECL,
    /// Reserved for a constructor of an algebraic data type; the parser does not emit it yet.
    N_CONSTRUCTOR,
    /// Reserved for the key expression of a relation; the parser does not emit it yet.
    N_KEY_EXPR,
    /// Reserved for the role of a relation (`input`, `output` or internal); the parser does not emit it yet.
    N_RELATION_ROLE,
    /// Reserved for the semantics of a relation (set, stream or multiset); the parser does not emit it yet.
    N_RELATION_SEMANTICS,
    /// Reserved for a relation declaration; the parser does not emit it yet.
    N_RELATION,
    /// An index declaration.
    N_INDEX,
    /// Reserved for a delay annotation on an atom; the parser does not emit it yet.
    N_DELAY,
    /// Reserved for an atom: a relation applied to arguments; the parser does not emit it yet.
    N_ATOM,
    /// Reserved for the head of a rule; the parser does not emit it yet.
    N_RULE_LHS,
    /// Reserved for the body of a rule; the parser does not emit it yet.
    N_RULE_RHS,
    /// A rule.
    N_RULE,
    /// An expression node.
    N_EXPR_NODE,
    /// Reserved for a parameter of a closure expression; the parser does not emit it yet.
    N_CLOSURE_EXPR_ARG,
    /// Reserved for a parameter of a function; the parser does not emit it yet.
    N_FUNC_ARG,
    /// A function declaration.
    N_FUNCTION,
    /// Reserved for a module name path; the parser does not emit it yet.
    N_MODULE_NAME,
    /// Reserved for a higher-order type; the parser does not emit it yet.
    N_HO_TYPE,
    /// Reserved for a field of a higher-order type; the parser does not emit it yet.
    N_HO_FIELD,
    /// A transformer declaration.
    N_TRANSFORMER,
    /// An `apply` statement.
    N_APPLY,
    /// An `import` statement.
    N_IMPORT_STMT,
    /// The root node: a whole `DDlog` program.
    N_DATALOG_PROGRAM,
    // Special
    /// A node or token the parser could not classify; also the fallback for unknown raw kinds.
    N_ERROR,
}

/// Newtype wrapper allowing `rowan` to store `SyntaxKind` values.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct DdlogLanguage;

impl RowanLanguage for DdlogLanguage {
    type Kind = SyntaxKind;

    fn kind_from_raw(raw: RowanSyntaxKind) -> Self::Kind {
        SyntaxKind::from_u16(raw.0).unwrap_or(SyntaxKind::N_ERROR)
    }

    fn kind_to_raw(kind: Self::Kind) -> RowanSyntaxKind {
        // `SyntaxKind` uses `#[repr(u16)]`, so every variant is a valid `u16`.
        RowanSyntaxKind(kind as u16)
    }
}
