//! Stable parser diagnostic identifiers and message rendering.
//!
//! Diagnostic codes are a compatibility surface under ADR-001 Phase 2. Once
//! published, a code keeps its meaning even when its human-facing message is
//! clarified. Categories provide a stable fallback for parser failures that do
//! not yet have individual codes. [`parse_error_message`] renders a final
//! `Simple` diagnostic for humans, so observers and front ends produce the
//! same text.

mod code;
mod message;

pub use code::{DiagnosticCategory, DiagnosticCode};
pub use message::{parse_error_message, parse_error_span};
