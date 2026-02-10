//! Helpers for constructing expression nodes in tests.

use super::Name;
use crate::parser::ast::{Expr, MatchArm, Pattern};
use crate::parser::pattern::parse_pattern;

/// Construct a variable [`Expr::Variable`].
///
/// Accepts any type convertible into [`Name`].
#[must_use]
pub fn var(name: impl Into<Name>) -> Expr {
    let name: Name = name.into();
    Expr::Variable(name.0)
}

/// Construct an unresolved application [`Expr::Apply`].
///
/// Accepts any type convertible into [`Name`] for the callee name.
#[must_use]
pub fn call(name: impl Into<Name>, args: Vec<Expr>) -> Expr {
    let name: Name = name.into();
    Expr::Apply {
        callee: Box::new(Expr::Variable(name.0)),
        args,
    }
}

/// Construct an unresolved application with an arbitrary callee [`Expr::Apply`].
#[must_use]
pub fn call_expr(callee: Expr, args: Vec<Expr>) -> Expr {
    Expr::Apply {
        callee: Box::new(callee),
        args,
    }
}

/// Construct a qualified function call [`Expr::Call`].
#[must_use]
pub fn qualified_call(name: impl Into<Name>, args: Vec<Expr>) -> Expr {
    let name: Name = name.into();
    Expr::Call {
        callee: Box::new(Expr::Variable(name.0)),
        args,
    }
}

/// Construct a method call [`Expr::MethodCall`].
#[must_use]
pub fn method_call(recv: Expr, name: impl Into<Name>, args: Vec<Expr>) -> Expr {
    let name: Name = name.into();
    Expr::MethodCall {
        recv: Box::new(recv),
        name: name.0,
        args,
    }
}

/// Construct a `break` [`Expr`].
#[must_use]
pub fn break_expr() -> Expr {
    Expr::Break
}

/// Construct a `continue` [`Expr`].
#[must_use]
pub fn continue_expr() -> Expr {
    Expr::Continue
}

/// Construct a `return` [`Expr`].
#[must_use]
pub fn return_expr(value: Option<Expr>) -> Expr {
    let value = value.unwrap_or_else(|| Expr::Tuple(Vec::new()));
    Expr::Return {
        value: Box::new(value),
    }
}

/// Construct a field access [`Expr::FieldAccess`].
#[must_use]
pub fn field_access(expr: Expr, field: impl Into<Name>) -> Expr {
    let field: Name = field.into();
    Expr::FieldAccess {
        expr: Box::new(expr),
        field: field.0,
    }
}

/// Construct a tuple index [`Expr::TupleIndex`].
#[must_use]
pub fn tuple_index(expr: Expr, index: &str) -> Expr {
    Expr::TupleIndex {
        expr: Box::new(expr),
        index: index.to_string(),
    }
}

/// Construct a bit slice [`Expr::BitSlice`].
#[must_use]
pub fn bit_slice(expr: Expr, hi: Expr, lo: Expr) -> Expr {
    Expr::BitSlice {
        expr: Box::new(expr),
        hi: Box::new(hi),
        lo: Box::new(lo),
    }
}

/// Construct a struct literal [`Expr::Struct`].
#[must_use]
pub fn struct_expr(name: impl Into<Name>, fields: Vec<(String, Expr)>) -> Expr {
    let name: Name = name.into();
    Expr::Struct {
        name: name.0,
        fields,
    }
}

/// Convenience to build a struct field tuple.
#[must_use]
pub fn field(name: impl Into<Name>, expr: Expr) -> (String, Expr) {
    let name: Name = name.into();
    (name.0, expr)
}

/// Construct a for-loop expression node.
#[must_use]
pub fn for_loop(
    pattern: impl Into<String>,
    iterable: Expr,
    guard: Option<Expr>,
    body: Expr,
) -> Expr {
    let pattern_src = pattern.into();
    let pattern = parse_pattern(&pattern_src)
        .unwrap_or_else(|errs| panic!("failed to parse pattern {pattern_src:?}: {errs:?}"));
    Expr::ForLoop {
        pattern,
        iterable: Box::new(iterable),
        guard: guard.map(Box::new),
        body: Box::new(body),
    }
}

/// Construct a tuple literal [`Expr::Tuple`].
#[must_use]
pub fn tuple(items: Vec<Expr>) -> Expr {
    Expr::Tuple(items)
}

/// Construct a vector literal [`Expr::VecLit`].
///
/// # Examples
///
/// ```
/// use ddlint::test_util::{lit_num, vec_lit, var};
///
/// let v = vec_lit(vec![lit_num("1"), var("x")]);
/// assert_eq!(v.to_sexpr(), "(vec 1 x)");
/// ```
#[must_use]
pub fn vec_lit(items: Vec<Expr>) -> Expr {
    Expr::VecLit(items)
}

/// Construct a map literal [`Expr::MapLit`].
///
/// # Examples
///
/// ```
/// use ddlint::test_util::{lit_num, map_entry, map_lit, var};
///
/// let m = map_lit(vec![map_entry(var("a"), lit_num("1"))]);
/// assert_eq!(m.to_sexpr(), "(map (entry a 1))");
/// ```
#[must_use]
pub fn map_lit(entries: Vec<(Expr, Expr)>) -> Expr {
    Expr::MapLit(entries)
}

/// Convenience function to build a map entry tuple.
///
/// # Examples
///
/// ```
/// use ddlint::test_util::{lit_num, lit_str, map_entry};
///
/// let entry = map_entry(lit_str("key"), lit_num("42"));
/// assert_eq!(entry.0.to_sexpr(), "\"key\"");
/// ```
#[must_use]
pub fn map_entry(key: Expr, value: Expr) -> (Expr, Expr) {
    (key, value)
}

/// Construct a `match` arm with the provided pattern and body expression.
///
/// # Examples
///
/// ```
/// use ddlint::test_util::{lit_num, match_arm, pat};
///
/// let arm = match_arm("_", lit_num("1"));
/// assert_eq!(arm.pattern, pat("_"));
/// ```
#[must_use]
pub fn match_arm(pattern: impl Into<String>, body: Expr) -> MatchArm {
    let pattern_src = pattern.into();
    let pattern = parse_pattern(&pattern_src)
        .unwrap_or_else(|errs| panic!("failed to parse pattern {pattern_src:?}: {errs:?}"));
    MatchArm { pattern, body }
}

/// Parse and construct a [`Pattern`] node for tests.
#[must_use]
pub fn pat(src: &str) -> Pattern {
    parse_pattern(src).unwrap_or_else(|errs| panic!("failed to parse pattern {src:?}: {errs:?}"))
}

/// Construct a `match` expression from a scrutinee and a list of arms.
#[must_use]
pub fn match_expr(scrutinee: Expr, arms: Vec<MatchArm>) -> Expr {
    Expr::Match {
        scrutinee: Box::new(scrutinee),
        arms,
    }
}

/// Construct a closure literal [`Expr::Closure`].
///
/// Accepts any iterable of parameter names.
#[must_use]
pub fn closure<P, S>(params: P, body: Expr) -> Expr
where
    P: IntoIterator<Item = S>,
    S: AsRef<str>,
{
    Expr::Closure {
        params: params.into_iter().map(|p| p.as_ref().to_string()).collect(),
        body: Box::new(body),
    }
}

/// Construct an `if` expression with an optional `else` branch.
///
/// Passing `None` for `else_branch` defaults to the unit `()` expression.
#[must_use]
pub fn if_expr(condition: Expr, then_branch: Expr, else_branch: Option<Expr>) -> Expr {
    Expr::IfElse {
        condition: Box::new(condition),
        then_branch: Box::new(then_branch),
        // Default to unit `()` when no else is provided.
        else_branch: Box::new(else_branch.unwrap_or_else(|| tuple(Vec::new()))),
    }
}
