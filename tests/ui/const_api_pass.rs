//! Compile-time proof that public APIs promoted to `const` by the lint
//! baseline are usable from outside the crate.
//!
//! Removing `const` from any of these methods makes this fixture fail to
//! compile, because each is bound in a `const` item or called from a `const
//! fn`. Covered APIs:
//!
//! - `RuleConfigValue::as_bool`;
//! - `RuleConfigValue::as_integer`;
//! - `UseOrigin::is_relation_read`;
//! - `SymbolOrigin::is_warning_eligible_rule_binding`;
//! - `StringLiteral::is_interpolated`;
//! - `ParseAttemptContext::category`;
//! - `ParseAttemptContext::diagnostic_count`;
//! - `LintDiagnostic::rule_name`;
//! - `LintDiagnostic::span`;
//! - `RuleCtx::ast_root`;
//! - `RuleCtx::cst_root`;
//! - `RuleCtx::config`;
//! - `Parsed::green`;
//! - `Parsed::root`;
//! - `PrimaryKey::expression`.

use ddlint::{
    DiagnosticCategory,
    ParseAttemptContext,
    Parsed,
    linter::{LintDiagnostic, RuleConfigValue, RuleCtx},
    parser::ast::{PrimaryKey, StringKind, StringLiteral},
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

/// Diagnostic accessors are callable from a `const fn`.
const fn check_diagnostic(diagnostic: &LintDiagnostic) {
    let _ = diagnostic.rule_name();
    let _ = diagnostic.span();
}

/// Rule-context accessors are callable from a `const fn`.
const fn check_rule_ctx(context: &RuleCtx) {
    let _ = context.ast_root();
    let _ = context.cst_root();
    let _ = context.config();
}

/// Parse-result accessors are callable from a `const fn`.
const fn check_parsed(parsed: &Parsed) {
    let _ = parsed.green();
    let _ = parsed.root();
}

/// Primary-key accessors are callable from a `const fn`.
const fn check_primary_key(key: &PrimaryKey) {
    let _ = key.expression();
}

fn main() {
    // Reference the helpers so they are not dead code. Their `const fn` bodies
    // are what prove the accessors above remain `const`.
    let _ = (
        check_diagnostic,
        check_rule_ctx,
        check_parsed,
        check_primary_key,
    );
}