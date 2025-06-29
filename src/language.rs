//! `rowan` integration and `DDlog` syntax kinds.
//!
//! This module defines the `SyntaxKind` enum covering all tokens and
//! non-terminal nodes in the `DDlog` grammar.  The enumeration is used by
//! `rowan` to tag syntax tree elements.  The `DdlogLanguage` newtype
//! implements `rowan::Language` using conversions provided by
//! `num_derive`.

use num_derive::{FromPrimitive as FromPrimitiveDerive, ToPrimitive as ToPrimitiveDerive};
use num_traits::{FromPrimitive, ToPrimitive};
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
    T_COMMENT,
    T_WHITESPACE,
    T_IDENT,
    T_STRING,
    T_NUMBER,
    T_LPAREN,
    T_RPAREN,
    T_LBRACE,
    T_RBRACE,
    T_LBRACKET,
    T_RBRACKET,
    T_SEMI,
    T_COMMA,
    T_DOT,
    T_COLON,
    T_COLON_COLON,
    T_PIPE,
    T_AMP,
    T_EQEQ,
    T_EQ,
    T_IMPLIES,
    T_PERCENT,
    T_STAR,
    T_SLASH,
    T_PLUS,
    T_MINUS,
    T_ARROW,
    T_FAT_ARROW,
    T_LTE,
    T_SPACESHIP,
    T_GTE,
    T_LT,
    T_GT,
    T_NEQ,
    T_SHR,
    T_SHL,
    T_TILDE,
    T_AT,
    T_HASH,
    T_APOSTROPHE,
    // Keywords
    K_ABSTRACT,
    K_AGGREGATE,
    K_AND,
    K_APPLY,
    K_AS,
    K_ASYNC,
    K_AWAIT,
    K_BECOME,
    K_BIGINT,
    K_BIT,
    K_BOOL,
    K_BOX,
    K_BREAK,
    K_CONST,
    K_CONTINUE,
    K_CRATE,
    K_DO,
    K_DOUBLE,
    K_DYN,
    K_ELSE,
    K_EXTERN,
    K_FALSE,
    K_FINAL,
    K_FN,
    K_FLATMAP,
    K_FLOAT,
    K_FOR,
    K_FUNCTION,
    K_IF,
    K_IMPL,
    K_IMPORT,
    K_IN,
    K_INPUT,
    K_INSPECT,
    K_LET,
    K_LOOP,
    K_MACRO,
    K_MATCH,
    K_MOD,
    K_MOVE,
    K_MULTISET,
    K_MUT,
    K_NOT,
    K_OR,
    K_OVERRIDE,
    K_OUTPUT,
    K_PRIV,
    K_PUB,
    K_REF,
    K_RELATION,
    K_RETURN,
    K_SELF,
    K_SELF_TYPE,
    K_SIGNED,
    K_SKIP,
    K_STATIC,
    K_STREAM,
    K_STRUCT,
    K_SUPER,
    K_TRAIT,
    K_TRANSFORMER,
    K_TRY,
    K_TRUE,
    K_TYPE,
    K_TYPEDEF,
    K_TYPEOF,
    K_UNDERSCORE,
    K_UNSAFE,
    K_UNSIZED,
    K_USE,
    K_VAR,
    K_VIRTUAL,
    K_WHERE,
    K_WHILE,
    K_YIELD,
    // Nodes
    N_ATTRIBUTE,
    N_FIELD,
    N_ARG_TYPE,
    N_IDENTIFIER_WITH_POS,
    N_TYPE,
    N_TYPE_DEF,
    N_RELATION_DECL,
    N_CONSTRUCTOR,
    N_KEY_EXPR,
    N_RELATION_ROLE,
    N_RELATION_SEMANTICS,
    N_RELATION,
    N_INDEX,
    N_DELAY,
    N_ATOM,
    N_RULE_LHS,
    N_RULE_RHS,
    N_RULE,
    N_EXPR_NODE,
    N_CLOSURE_EXPR_ARG,
    N_FUNC_ARG,
    N_FUNCTION,
    N_MODULE_NAME,
    N_HO_TYPE,
    N_HO_FIELD,
    N_TRANSFORMER,
    N_APPLY,
    N_IMPORT_STMT,
    N_DATALOG_PROGRAM,
    // Special
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
        RowanSyntaxKind(
            kind.to_u16()
                .unwrap_or_else(|| unreachable!("all SyntaxKind variants map to u16")),
        )
    }
}
