//! Stable parser diagnostic identifiers.
//!
//! Diagnostic codes are a compatibility surface under ADR-001 Phase 2. Once
//! published, a code keeps its meaning even when its human-facing message is
//! clarified. Categories provide a stable fallback for parser failures that do
//! not yet have individual codes.

mod code;

pub use code::{DiagnosticCategory, DiagnosticCode};
