//! Compile-time proof that public APIs promoted to `const` by the lint
//! baseline are usable from outside the crate.
//!
//! Removing `const` from any of these methods makes this fixture fail to
//! compile, because each result is bound in a `const` item.

use ddlint::{
    linter::RuleConfigValue,
    sema::{SymbolOrigin, UseOrigin},
};

/// A boolean rule setting can be inspected at compile time.
const ENABLED: Option<bool> = RuleConfigValue::Bool(true).as_bool();

/// A read-like relation position is recognised at compile time.
const IS_RELATION_READ: bool = UseOrigin::RelationBody.is_relation_read();

/// A user-authored binding is warning-eligible at compile time.
const IS_WARNING_ELIGIBLE: bool =
    SymbolOrigin::RuleHead.is_warning_eligible_rule_binding();

const _: () = assert!(matches!(ENABLED, Some(true)));
const _: () = assert!(IS_RELATION_READ);
const _: () = assert!(IS_WARNING_ELIGIBLE);

fn main() {}