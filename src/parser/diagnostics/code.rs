//! Diagnostic codes and their owning parser categories.

use std::fmt;

/// Stable parser diagnostic families.
///
/// Categories identify the parser phase that produced a diagnostic. They are
/// suitable for bounded metrics labels and remain available while individual
/// scanner families acquire more specific diagnostic codes.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
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

/// Stable parser diagnostic codes.
///
/// Code strings are compatibility identifiers. Consumers should use
/// [`DiagnosticCode::as_str`] rather than deriving labels from variant names or
/// diagnostic message text.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum DiagnosticCode {
    /// A relation kind appears before its role keyword.
    RelationKindBeforeRole,
    /// A relation declaration contains more than one role keyword.
    RelationDuplicateRole,
    /// A relation declaration contains more than one kind keyword.
    RelationDuplicateKind,
    /// A bracket-form relation declares a primary-key clause.
    RelationBracketPrimaryKey,
    /// A bracket-form relation does not contain exactly one element type.
    RelationInvalidBracketElementType,
    /// A non-input relation declares a primary-key clause.
    RelationPrimaryKeyOnNonInput,
    /// A relation contains an unexpected or malformed primary-key clause.
    RelationMalformedPrimaryKey,
    /// A relation uses an unsupported bracket-wrapped primary-key clause.
    RelationBracketWrappedPrimaryKey,
}

impl DiagnosticCode {
    /// Return the stable external diagnostic identifier.
    #[must_use]
    pub const fn as_str(self) -> &'static str {
        match self {
            Self::RelationKindBeforeRole => "D-REL-001",
            Self::RelationDuplicateRole => "D-REL-002",
            Self::RelationDuplicateKind => "D-REL-003",
            Self::RelationBracketPrimaryKey => "D-REL-004",
            Self::RelationInvalidBracketElementType => "D-REL-005",
            Self::RelationPrimaryKeyOnNonInput => "D-REL-006",
            Self::RelationMalformedPrimaryKey => "D-REL-007",
            Self::RelationBracketWrappedPrimaryKey => "D-REL-008",
        }
    }

    /// Return the parser category that owns this diagnostic.
    #[must_use]
    pub const fn category(self) -> DiagnosticCategory {
        match self {
            Self::RelationKindBeforeRole
            | Self::RelationDuplicateRole
            | Self::RelationDuplicateKind
            | Self::RelationBracketPrimaryKey
            | Self::RelationInvalidBracketElementType
            | Self::RelationPrimaryKeyOnNonInput
            | Self::RelationMalformedPrimaryKey
            | Self::RelationBracketWrappedPrimaryKey => DiagnosticCategory::Relation,
        }
    }

    pub(crate) fn from_message(message: &str) -> Option<Self> {
        [
            Self::RelationKindBeforeRole,
            Self::RelationDuplicateRole,
            Self::RelationDuplicateKind,
            Self::RelationBracketPrimaryKey,
            Self::RelationInvalidBracketElementType,
            Self::RelationPrimaryKeyOnNonInput,
            Self::RelationMalformedPrimaryKey,
            Self::RelationBracketWrappedPrimaryKey,
        ]
        .into_iter()
        .find(|code| message.starts_with(code.as_str()))
    }
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
        assert_eq!(
            DiagnosticCode::from_message(&format!("{expected}: message")),
            Some(code)
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
