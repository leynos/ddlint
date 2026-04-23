//! S-expression formatting helpers for [`super::Expr`].
//!
//! Tests use these compact renderings to compare parsed structure without
//! coupling to debug formatting.

use super::{Expr, Literal, MatchArm, Pattern};

impl Expr {
    /// Display the expression as a simple S-expression for tests.
    #[must_use]
    pub fn to_sexpr(&self) -> String {
        match self {
            Self::Literal(Literal::Number(n)) => n.to_sexpr(),
            Self::Literal(Literal::String(s)) => s.to_sexpr(),
            Self::Literal(Literal::Bool(b)) => b.to_string(),
            Self::Variable(name) => name.clone(),
            Self::Apply { callee, args } | Self::Call { callee, args } => format_nary(
                "call",
                std::iter::once(callee.to_sexpr()).chain(args.iter().map(Self::to_sexpr)),
            ),
            Self::MethodCall { recv, name, args } => format_nary(
                "method",
                std::iter::once(recv.to_sexpr())
                    .chain(std::iter::once(name.clone()))
                    .chain(args.iter().map(Self::to_sexpr)),
            ),
            Self::FieldAccess { expr, field } => {
                format_nary("field", [expr.to_sexpr(), field.clone()])
            }
            Self::TupleIndex { expr, index } => {
                format_nary("tuple-index", [expr.to_sexpr(), index.clone()])
            }
            Self::BitSlice { expr, hi, lo } => {
                format_nary("bitslice", [expr.to_sexpr(), hi.to_sexpr(), lo.to_sexpr()])
            }
            Self::Struct { name, fields } => format_nary(
                "struct",
                std::iter::once(name.clone()).chain(
                    fields
                        .iter()
                        .map(|(name, expr)| format_field(name, &expr.to_sexpr())),
                ),
            ),
            Self::Tuple(items) => format_nary("tuple", items.iter().map(Self::to_sexpr)),
            Self::Closure { params, body } => format_nary(
                "closure",
                [format!("({})", params.join(" ")), body.to_sexpr()],
            ),
            Self::IfElse {
                condition,
                then_branch,
                else_branch,
            } => format_if_else(condition, then_branch, else_branch),
            Self::Unary { op, expr } => format_nary(op.symbol(), std::iter::once(expr.to_sexpr())),
            Self::AtomDiff { expr } => format_nary("diff", [expr.to_sexpr()]),
            Self::AtomDelay { delay, expr } => {
                format_nary("delay", [delay.to_string(), expr.to_sexpr()])
            }
            Self::Binary { op, lhs, rhs } => {
                format_nary(op.symbol(), [lhs.to_sexpr(), rhs.to_sexpr()])
            }
            Self::Group(expr) => format_nary("group", std::iter::once(expr.to_sexpr())),
            Self::ForLoop {
                pattern,
                iterable,
                guard,
                body,
            } => format_for_loop(pattern, iterable, guard.as_deref(), body),
            Self::Match { scrutinee, arms } => format_match(scrutinee, arms),
            Self::Break => "(break)".to_string(),
            Self::Continue => "(continue)".to_string(),
            Self::Return { value } => format_nary("return", [value.to_sexpr()]),
            Self::VecLit(items) => format_nary("vec", items.iter().map(Self::to_sexpr)),
            Self::MapLit(entries) => format_nary(
                "map",
                entries.iter().map(|(key, value)| format_kv(key, value)),
            ),
        }
    }
}

#[inline]
fn format_field(name: &str, value: &str) -> String {
    let mut rendered = String::with_capacity(name.len() + value.len() + 3);
    rendered.push('(');
    rendered.push_str(name);
    rendered.push(' ');
    rendered.push_str(value);
    rendered.push(')');
    rendered
}

#[inline]
fn format_kv(key: &Expr, value: &Expr) -> String {
    let key = key.to_sexpr();
    let value = value.to_sexpr();
    let mut rendered = String::with_capacity(key.len() + value.len() + 10);
    rendered.push_str("(entry ");
    rendered.push_str(&key);
    rendered.push(' ');
    rendered.push_str(&value);
    rendered.push(')');
    rendered
}

fn format_nary<I>(label: &str, parts: I) -> String
where
    I: IntoIterator<Item = String>,
{
    let parts: Vec<String> = parts.into_iter().collect();
    let capacity = 2 + label.len() + parts.iter().map(|part| 1 + part.len()).sum::<usize>();
    let mut rendered = String::with_capacity(capacity);
    rendered.push('(');
    rendered.push_str(label);
    for part in parts {
        rendered.push(' ');
        rendered.push_str(&part);
    }
    rendered.push(')');
    rendered
}

fn format_if_else(condition: &Expr, then_branch: &Expr, else_branch: &Expr) -> String {
    format_nary(
        "if",
        [
            condition.to_sexpr(),
            then_branch.to_sexpr(),
            else_branch.to_sexpr(),
        ],
    )
}

fn format_for_loop(
    pattern: &Pattern,
    iterable: &Expr,
    guard: Option<&Expr>,
    body: &Expr,
) -> String {
    let mut parts = vec![pattern.to_source(), iterable.to_sexpr()];
    if let Some(condition) = guard {
        parts.push(condition.to_sexpr());
    }
    parts.push(body.to_sexpr());
    format_nary("for", parts)
}

fn format_match(scrutinee: &Expr, arms: &[MatchArm]) -> String {
    let arm_parts = arms
        .iter()
        .map(|arm| format_nary("arm", [arm.pattern.to_source(), arm.body.to_sexpr()]));
    format_nary(
        "match",
        std::iter::once(scrutinee.to_sexpr()).chain(arm_parts),
    )
}
