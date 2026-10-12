//! Typed expectations and source scenarios for reserved-token policy tests.
//!
//! Pairing each lexeme with its diagnostic prevents contradictory test inputs.
//! These fixtures are private to the reserved-token suite, not parser APIs.

use super::super::helpers::SourceText;
use crate::parser::reserved_tokens::{
    RESERVED_BARE_HASH_ERROR, RESERVED_BIGINT_ERROR, RESERVED_BIT_ERROR, RESERVED_DOUBLE_ERROR,
    RESERVED_FLOAT_ERROR, RESERVED_SIGNED_ERROR, RESERVED_SPACESHIP_ERROR, RESERVED_TYPEDEF_ERROR,
};

#[derive(Clone, Copy, Debug)]
pub(super) enum ExpectedReservedToken {
    Typedef,
    Bigint,
    Bit,
    Double,
    Float,
    Signed,
    BareHash,
    Spaceship,
}

impl ExpectedReservedToken {
    pub(super) const fn lexeme(self) -> &'static str {
        match self {
            Self::Typedef => "typedef",
            Self::Bigint => "bigint",
            Self::Bit => "bit",
            Self::Double => "double",
            Self::Float => "float",
            Self::Signed => "signed",
            Self::BareHash => "#",
            Self::Spaceship => "<=>",
        }
    }

    pub(super) const fn message(self) -> &'static str {
        match self {
            Self::Typedef => RESERVED_TYPEDEF_ERROR,
            Self::Bigint => RESERVED_BIGINT_ERROR,
            Self::Bit => RESERVED_BIT_ERROR,
            Self::Double => RESERVED_DOUBLE_ERROR,
            Self::Float => RESERVED_FLOAT_ERROR,
            Self::Signed => RESERVED_SIGNED_ERROR,
            Self::BareHash => RESERVED_BARE_HASH_ERROR,
            Self::Spaceship => RESERVED_SPACESHIP_ERROR,
        }
    }
}

#[derive(Debug)]
pub(super) struct ReservedTokenScenario {
    pub(super) source: SourceText,
    pub(super) expected: ExpectedReservedToken,
}

impl ReservedTokenScenario {
    pub(super) fn new(source: impl Into<SourceText>, expected: ExpectedReservedToken) -> Self {
        Self {
            source: source.into(),
            expected,
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub(super) enum DeclarationPosition {
    FunctionParameter,
    RelationField,
}

impl DeclarationPosition {
    pub(super) fn source(self, expected: ExpectedReservedToken) -> SourceText {
        let token = expected.lexeme();
        match self {
            Self::FunctionParameter => format!("function bad(value: {token}): u32 = 0;"),
            Self::RelationField => format!("input relation Bad(value: {token});"),
        }
        .into()
    }
}
