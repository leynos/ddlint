//! Post-parse validation passes.
//!
//! Validators run after CST construction and emit diagnostics for
//! semantic constraints that require whole-program visibility.

pub(crate) mod name_uniqueness;
pub(crate) use name_uniqueness::validate_name_uniqueness;
