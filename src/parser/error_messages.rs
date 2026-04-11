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
