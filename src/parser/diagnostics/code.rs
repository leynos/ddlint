//! Diagnostic codes and their owning parser categories.
//!
//! The `D-REL-001` through `D-REL-008` identifiers are the frozen compatibility
//! surface established by ADR-001 Phase 2.

use std::fmt;

use crate::parser::error_messages::{
    RELATION_BRACKET_PRIMARY_KEY_ERROR, RELATION_BRACKET_WRAPPED_PRIMARY_KEY_ERROR,
    RELATION_DUPLICATE_KIND_ERROR, RELATION_DUPLICATE_ROLE_ERROR,
    RELATION_INVALID_BRACKET_ELEMENT_TYPE_ERROR, RELATION_KIND_BEFORE_ROLE_ERROR,
    RELATION_MALFORMED_PRIMARY_KEY_ERROR, RELATION_PRIMARY_KEY_ON_NON_INPUT_ERROR,
};

/// Stable parser diagnostic families.
///
/// Categories identify the parser phase that produced a diagnostic. They are
/// suitable for bounded metrics labels and remain available while individual
/// scanner families acquire more specific diagnostic codes.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[non_exhaustive]
pub enum DiagnosticCategory {
    /// The complete parser entry point.
    Parser,
    /// Attribute span scanning.
    Attribute,
    /// Import span scanning.
    Import,
    /// Type definition span scanning.
    Typedef,
    /// Relation span scanning.
    Relation,
    /// Index span scanning.
    Index,
    /// Function span scanning.
    Function,
    /// Transformer span scanning.
    Transformer,
    /// Apply-item span scanning.
    Apply,
    /// Rule and expression span scanning.
    Rule,
    /// Lexical error collection.
    Lexer,
    /// Parsed-span validation and construction.
    SpanBuilder,
    /// Top-level `for` desugaring.
    TopLevelFor,
    /// Parser-level name-uniqueness validation.
    NameUniqueness,
}

impl DiagnosticCategory {
    /// Return the stable, low-cardinality category label.
    #[must_use]
    pub const fn as_str(self) -> &'static str {
        match self {
            Self::Parser => "parser",
            Self::Attribute => "attribute",
            Self::Import => "import",
            Self::Typedef => "typedef",
            Self::Relation => "relation",
            Self::Index => "index",
            Self::Function => "function",
            Self::Transformer => "transformer",
            Self::Apply => "apply",
            Self::Rule => "rule",
            Self::Lexer => "lexer",
            Self::SpanBuilder => "span_builder",
            Self::TopLevelFor => "top_level_for",
            Self::NameUniqueness => "name_uniqueness",
        }
    }
}

impl fmt::Display for DiagnosticCategory {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter.write_str(self.as_str())
    }
}

macro_rules! define_diagnostic_codes {
    ($( $(#[$variant_doc:meta])* $variant:ident {
        code: $code:literal,
        category: $category:ident,
        message: $message:path,
    }),+ $(,)?) => {
        /// Stable parser diagnostic codes.
        ///
        /// Code strings are compatibility identifiers. Consumers should use
        /// [`DiagnosticCode::as_str`] rather than deriving labels from variant
        /// names or diagnostic message text.
        #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
        #[non_exhaustive]
        pub enum DiagnosticCode {
            $(
                $(#[$variant_doc])*
                $variant,
            )+
        }

        const DIAGNOSTIC_CODES: &[DiagnosticCode] = &[
            $(DiagnosticCode::$variant,)+
        ];

        impl DiagnosticCode {
            /// Return the stable external diagnostic identifier.
            #[must_use]
            pub const fn as_str(self) -> &'static str {
                match self {
                    $(Self::$variant => $code,)+
                }
            }

            /// Return the parser category that owns this diagnostic.
            #[must_use]
            pub const fn category(self) -> DiagnosticCategory {
                match self {
                    $(Self::$variant => DiagnosticCategory::$category,)+
                }
            }

            /// Return the canonical human-facing message for this diagnostic.
            pub(crate) const fn message_text(self) -> &'static str {
                match self {
                    $(Self::$variant => $message,)+
                }
            }

            /// Identify a diagnostic from the stable code prefix in its message.
            pub(crate) fn from_message(message: &str) -> Option<Self> {
                let identifier = message.split_once(':')?.0;
                DIAGNOSTIC_CODES
                    .iter()
                    .copied()
                    .find(|code| code.as_str() == identifier)
            }
        }
    };
}

define_diagnostic_codes! {
    /// A relation kind appears before its role keyword.
    RelationKindBeforeRole {
        code: "D-REL-001",
        category: Relation,
        message: RELATION_KIND_BEFORE_ROLE_ERROR,
    },
    /// A relation declaration contains more than one role keyword.
    RelationDuplicateRole {
        code: "D-REL-002",
        category: Relation,
        message: RELATION_DUPLICATE_ROLE_ERROR,
    },
    /// A relation declaration contains more than one kind keyword.
    RelationDuplicateKind {
        code: "D-REL-003",
        category: Relation,
        message: RELATION_DUPLICATE_KIND_ERROR,
    },
    /// A bracket-form relation declares a primary-key clause.
    RelationBracketPrimaryKey {
        code: "D-REL-004",
        category: Relation,
        message: RELATION_BRACKET_PRIMARY_KEY_ERROR,
    },
    /// A bracket-form relation does not contain exactly one element type.
    RelationInvalidBracketElementType {
        code: "D-REL-005",
        category: Relation,
        message: RELATION_INVALID_BRACKET_ELEMENT_TYPE_ERROR,
    },
    /// A non-input relation declares a primary-key clause.
    RelationPrimaryKeyOnNonInput {
        code: "D-REL-006",
        category: Relation,
        message: RELATION_PRIMARY_KEY_ON_NON_INPUT_ERROR,
    },
    /// A relation contains an unexpected or malformed primary-key clause.
    RelationMalformedPrimaryKey {
        code: "D-REL-007",
        category: Relation,
        message: RELATION_MALFORMED_PRIMARY_KEY_ERROR,
    },
    /// A relation uses an unsupported bracket-wrapped primary-key clause.
    RelationBracketWrappedPrimaryKey {
        code: "D-REL-008",
        category: Relation,
        message: RELATION_BRACKET_WRAPPED_PRIMARY_KEY_ERROR,
    },
}

impl fmt::Display for DiagnosticCode {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter.write_str(self.as_str())
    }
}

#[cfg(test)]
mod tests {
    //! Contract tests for stable diagnostic identifiers.

    use rstest::rstest;

    use super::{DiagnosticCategory, DiagnosticCode};

    #[rstest]
    #[case(DiagnosticCode::RelationKindBeforeRole, "D-REL-001")]
    #[case(DiagnosticCode::RelationDuplicateRole, "D-REL-002")]
    #[case(DiagnosticCode::RelationDuplicateKind, "D-REL-003")]
    #[case(DiagnosticCode::RelationBracketPrimaryKey, "D-REL-004")]
    #[case(DiagnosticCode::RelationInvalidBracketElementType, "D-REL-005")]
    #[case(DiagnosticCode::RelationPrimaryKeyOnNonInput, "D-REL-006")]
    #[case(DiagnosticCode::RelationMalformedPrimaryKey, "D-REL-007")]
    #[case(DiagnosticCode::RelationBracketWrappedPrimaryKey, "D-REL-008")]
    fn relation_codes_have_stable_labels(#[case] code: DiagnosticCode, #[case] expected: &str) {
        assert_eq!(code.as_str(), expected);
        assert_eq!(code.to_string(), expected);
        assert_eq!(code.category(), DiagnosticCategory::Relation);
        assert!(code.message_text().starts_with(expected));
        assert_eq!(
            DiagnosticCode::from_message(code.message_text()),
            Some(code)
        );
    }

    #[rstest]
    #[case("D-REL-999: message")]
    #[case("D-REL-0010: message")]
    #[case("D-REL-001message")]
    fn message_code_match_requires_a_known_message(#[case] message: &str) {
        assert_eq!(DiagnosticCode::from_message(message), None);
    }

    #[test]
    fn message_code_match_ignores_wording_after_the_stable_prefix() {
        assert_eq!(
            DiagnosticCode::from_message("D-REL-001: wording may evolve"),
            Some(DiagnosticCode::RelationKindBeforeRole)
        );
    }

    #[rstest]
    #[case(DiagnosticCategory::Parser, "parser")]
    #[case(DiagnosticCategory::Attribute, "attribute")]
    #[case(DiagnosticCategory::Import, "import")]
    #[case(DiagnosticCategory::Typedef, "typedef")]
    #[case(DiagnosticCategory::Relation, "relation")]
    #[case(DiagnosticCategory::Index, "index")]
    #[case(DiagnosticCategory::Function, "function")]
    #[case(DiagnosticCategory::Transformer, "transformer")]
    #[case(DiagnosticCategory::Apply, "apply")]
    #[case(DiagnosticCategory::Rule, "rule")]
    #[case(DiagnosticCategory::Lexer, "lexer")]
    #[case(DiagnosticCategory::SpanBuilder, "span_builder")]
    #[case(DiagnosticCategory::TopLevelFor, "top_level_for")]
    #[case(DiagnosticCategory::NameUniqueness, "name_uniqueness")]
    fn categories_have_stable_labels(#[case] category: DiagnosticCategory, #[case] expected: &str) {
        assert_eq!(category.as_str(), expected);
        assert_eq!(category.to_string(), expected);
    }
}
