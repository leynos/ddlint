//! Shared parser diagnostic messages.
//!
//! Centralizing parser-facing message text keeps scanner code and test helpers
//! aligned when diagnostics are intentionally part of the parser contract.

/// Diagnostic emitted for a literal that mixes vector elements and map
/// entries.
///
/// Upstream `DDlog` decides between `evec_literal` and `emap_literal` from the
/// first item, so a literal cannot be both.
pub const MIXED_COLLECTION_ENTRY_ERROR: &str =
    "cannot mix vector elements and map entries in one literal";

/// Diagnostic emitted when a bracket map entry ends without its `->`.
///
/// The literal began as a map, so a key followed by end of input is an
/// unfinished entry rather than a vector element: the entry is missing its
/// value.
pub const MISSING_MAP_ARROW_ERROR: &str = "expected '->' in map entry";

/// Diagnostic emitted for the empty vector literal `[]`.
///
/// Upstream `evec_literal` uses `commaSepEnd1`, so the empty form is not
/// syntax; the standard library provides the constructors instead.
pub const EMPTY_VECTOR_LITERAL_ERROR: &str =
    "empty vector literals are not DDlog syntax; use vec_empty() instead";

/// Diagnostic emitted when a block item is not followed by `;` or `}`.
///
/// Inside braces `;` separates block items; it is not the sequencing operator,
/// so an adjacent expression is a missing separator rather than a
/// continuation.
pub const BLOCK_ITEM_SEPARATOR_ERROR: &str = "expected ';' or '}' after a block item";

/// Diagnostic emitted for an empty block item, as in `{ ; }` or `{ a;; b }`.
///
/// A separator with no item on one side would make the block's item order
/// ambiguous, so it is rejected rather than skipped.
pub const EMPTY_BLOCK_ITEM_ERROR: &str = "empty statement in block: remove the extra ';'";

/// Diagnostic emitted when a block reaches end of input before its `}`.
pub const UNCLOSED_BLOCK_ERROR: &str = "expected '}' to close block";

/// Diagnostic emitted for `return` in a rule-body literal.
///
/// `return` supplies a value to a function or closure body. A rule body has no
/// return value, so the keyword is rejected wherever it appears in one,
/// including inside a block, `if`, or loop nested in the body.
pub const RETURN_IN_RULE_BODY_ERROR: &str =
    "'return' is not permitted in a rule body; use it only in a function or closure body";

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
