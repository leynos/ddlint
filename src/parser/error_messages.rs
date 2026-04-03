//! Shared parser diagnostic messages.
//!
//! Centralizing parser-facing message text keeps scanner code and test helpers
//! aligned when diagnostics are intentionally part of the parser contract.

/// Diagnostic emitted when a transformer declaration omits `:` or its first
/// output identifier.
pub const MISSING_OUTPUT_SIGNATURE_ERROR: &str =
    "transformer declarations require ':' followed by at least one output identifier";
