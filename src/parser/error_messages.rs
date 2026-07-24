//! Shared parser diagnostic messages.
//!
//! Centralizing parser-facing message text keeps scanner code and test helpers
//! aligned when diagnostics are intentionally part of the parser contract.

/// Diagnostic emitted when a transformer declaration omits `:` or its first
/// output identifier.
pub const MISSING_OUTPUT_SIGNATURE_ERROR: &str =
    "transformer declarations require ':' followed by at least one output identifier";

/// Diagnostic emitted when a transformer name starts with an uppercase letter
/// instead of a lowercase letter or underscore.
pub const CAPITALIZED_TRANSFORMER_NAME_ERROR: &str =
    "transformer names must start with a lowercase letter or underscore";

/// Message for [`DiagnosticCode::RelationKindBeforeRole`].
///
/// [`DiagnosticCode::RelationKindBeforeRole`]: super::diagnostics::DiagnosticCode::RelationKindBeforeRole
pub const RELATION_KIND_BEFORE_ROLE_ERROR: &str =
    "D-REL-001: relation role keyword (input/output) must precede the kind keyword";

/// Message for [`DiagnosticCode::RelationDuplicateRole`].
///
/// [`DiagnosticCode::RelationDuplicateRole`]: super::diagnostics::DiagnosticCode::RelationDuplicateRole
pub const RELATION_DUPLICATE_ROLE_ERROR: &str =
    "D-REL-002: at most one role keyword (input, output) is permitted";

/// Message for [`DiagnosticCode::RelationDuplicateKind`].
///
/// [`DiagnosticCode::RelationDuplicateKind`]: super::diagnostics::DiagnosticCode::RelationDuplicateKind
pub const RELATION_DUPLICATE_KIND_ERROR: &str =
    "D-REL-003: at most one kind keyword (relation, stream, multiset) is permitted";

/// Message for [`DiagnosticCode::RelationBracketPrimaryKey`].
///
/// [`DiagnosticCode::RelationBracketPrimaryKey`]: super::diagnostics::DiagnosticCode::RelationBracketPrimaryKey
pub const RELATION_BRACKET_PRIMARY_KEY_ERROR: &str =
    "D-REL-004: bracket-form relations cannot declare a primary key clause";

/// Message for [`DiagnosticCode::RelationInvalidBracketElementType`].
///
/// [`DiagnosticCode::RelationInvalidBracketElementType`]: super::diagnostics::DiagnosticCode::RelationInvalidBracketElementType
pub const RELATION_INVALID_BRACKET_ELEMENT_TYPE_ERROR: &str =
    "D-REL-005: bracket-form relations require a single element type between '[' and ']'";

/// Message for [`DiagnosticCode::RelationPrimaryKeyOnNonInput`].
///
/// [`DiagnosticCode::RelationPrimaryKeyOnNonInput`]: super::diagnostics::DiagnosticCode::RelationPrimaryKeyOnNonInput
pub const RELATION_PRIMARY_KEY_ON_NON_INPUT_ERROR: &str =
    "D-REL-006: primary key clauses are only valid on input relations";

/// Message for [`DiagnosticCode::RelationMalformedPrimaryKey`].
///
/// [`DiagnosticCode::RelationMalformedPrimaryKey`]: super::diagnostics::DiagnosticCode::RelationMalformedPrimaryKey
pub const RELATION_MALFORMED_PRIMARY_KEY_ERROR: &str =
    "D-REL-007: unexpected or malformed primary key clause";

/// Message for [`DiagnosticCode::RelationBracketWrappedPrimaryKey`].
///
/// [`DiagnosticCode::RelationBracketWrappedPrimaryKey`]: super::diagnostics::DiagnosticCode::RelationBracketWrappedPrimaryKey
pub const RELATION_BRACKET_WRAPPED_PRIMARY_KEY_ERROR: &str = concat!(
    "D-REL-008: bracket-wrapped primary key clauses are not supported; ",
    "remove the surrounding '['/']'"
);
