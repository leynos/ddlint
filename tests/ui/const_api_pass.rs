//! Compile-time proof that public APIs promoted to `const` by the lint
//! baseline are usable from outside the crate.
//!
//! Removing `const` from any of these methods makes this fixture fail to
//! compile, because each result is bound in a `const` item. Covered APIs:
//!
//! - `RuleConfigValue::as_bool`;
//! - `RuleConfigValue::as_integer`;
//! - `UseOrigin::is_relation_read`;
//! - `SymbolOrigin::is_warning_eligible_rule_binding`;
//! - `StringLiteral::is_interpolated`;
//! - `ParseAttemptContext::category`;
//! - `ParseAttemptContext::diagnostic_count`.

use ddlint::{
    DiagnosticCategory,
    ParseAttemptContext,
    linter::RuleConfigValue,
    parser::ast::{StringKind, StringLiteral},
    sema::{SymbolOrigin, UseOrigin},
};

/// A boolean rule setting can be inspected at compile time.
const ENABLED: Option<bool> = RuleConfigValue::Bool(true).as_bool();

/// An integer rule setting can be inspected at compile time.
const INTEGER: Option<i64> = RuleConfigValue::Integer(3).as_integer();

/// A parser attempt summary is constructible and inspectable at compile time.
const ATTEMPT: ParseAttemptContext =
    ParseAttemptContext::new(DiagnosticCategory::Parser, 2);
const ATTEMPT_CATEGORY: DiagnosticCategory = ATTEMPT.category();
const ATTEMPT_COUNT: usize = ATTEMPT.diagnostic_count();

/// A read-like relation position is recognised at compile time.
const IS_RELATION_READ: bool = UseOrigin::RelationBody.is_relation_read();

/// A user-authored binding is warning-eligible at compile time.
const IS_WARNING_ELIGIBLE: bool =
    SymbolOrigin::RuleHead.is_warning_eligible_rule_binding();

/// A string literal's interpolation flag is inspectable at compile time.
const IS_INTERPOLATED: bool = {
    let literal = StringLiteral {
        body: String::new(),
        kind: StringKind::Standard { interpolated: true },
        interned: false,
    };
    let interpolated = literal.is_interpolated();
    // `StringLiteral` owns a `String`, whose destructor cannot run during
    // const evaluation (`const_destruct` is unstable), so leak the temporary.
    core::mem::forget(literal);
    interpolated
};

const _: () = assert!(matches!(ENABLED, Some(true)));
const _: () = assert!(matches!(INTEGER, Some(3)));
const _: () = assert!(matches!(ATTEMPT_CATEGORY, DiagnosticCategory::Parser));
const _: () = assert!(ATTEMPT_COUNT == 2);
const _: () = assert!(IS_RELATION_READ);
const _: () = assert!(IS_WARNING_ELIGIBLE);
const _: () = assert!(IS_INTERPOLATED);

fn main() {}