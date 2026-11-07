//! Variable-use traversal for semantic-model collection.

use super::builder::SemanticModelBuilder;
use crate::{
    Span,
    parser::ast::Expr,
    sema::model::{ScopeId, UseKind, UseOrigin, UseSite},
};

#[derive(Clone, Copy)]
pub(super) struct ForLoopExprs<'a> {
    pub(super) iterable: &'a Expr,
    pub(super) guard: Option<&'a Expr>,
    pub(super) body: &'a Expr,
}

#[derive(Clone, Copy)]
pub(super) struct VariableUseContext<'a> {
    current_scope: ScopeId,
    literal_index: usize,
    span: &'a Span,
    rule_order_limit: usize,
}

impl<'a> VariableUseContext<'a> {
    pub(super) const fn new(
        current_scope: ScopeId,
        literal_index: usize,
        span: &'a Span,
        rule_order_limit: usize,
    ) -> Self {
        Self {
            current_scope,
            literal_index,
            span,
            rule_order_limit,
        }
    }

    pub(super) const fn current_scope(self) -> ScopeId { self.current_scope }

    pub(super) const fn literal_index(self) -> usize { self.literal_index }

    pub(super) const fn span(self) -> &'a Span { self.span }

    pub(super) const fn rule_order_limit(self) -> usize { self.rule_order_limit }
}

impl SemanticModelBuilder {
    /// Records every variable use reachable from `expr`.
    ///
    /// The walk covers the whole expression tree, dispatching each variant to
    /// a helper that knows how to reach its children, so uses nested inside
    /// calls, wrappers, composite literals, conditionals, loops, and match
    /// arms are all captured. `context` supplies the scope, source position,
    /// and rule-order limit inherited by each recorded [`UseSite`].
    pub(crate) fn walk_variable_uses(&mut self, expr: &Expr, context: VariableUseContext<'_>) {
        match expr {
            Expr::Variable(name) => self.record_variable_use(name, context),
            Expr::Literal(_) | Expr::Break | Expr::Continue => {}
            Expr::Apply { .. } | Expr::Call { .. } | Expr::MethodCall { .. } => {
                self.walk_variable_uses_call_like(expr, context);
            }
            Expr::FieldAccess { .. }
            | Expr::TupleIndex { .. }
            | Expr::Group(_)
            | Expr::AtomDiff { .. }
            | Expr::AtomDelay { .. }
            | Expr::Unary { .. }
            | Expr::Closure { .. }
            | Expr::Return { .. } => {
                // The helper dispatches on the wrapper before unwrapping it.
                // Passing the child here would fall through its match and
                // silently drop the variable uses inside.
                self.walk_variable_uses_unary_simple(expr, context);
            }
            Expr::BitSlice { .. }
            | Expr::Struct { .. }
            | Expr::Tuple(_)
            | Expr::VecLit(_)
            | Expr::MapLit(_) => self.walk_variable_uses_composite(expr, context),
            Expr::IfElse { .. } | Expr::Binary { .. } => {
                self.walk_variable_uses_conditional(expr, context);
            }
            Expr::ForLoop { .. } | Expr::Match { .. } => {
                self.walk_variable_uses_loop_and_match(expr, context);
            }
        }
    }

    fn walk_variable_uses_call_like(&mut self, expr: &Expr, context: VariableUseContext<'_>) {
        match expr {
            Expr::Apply { args, .. } | Expr::Call { args, .. } => {
                for arg in args {
                    self.walk_variable_uses(arg, context);
                }
            }
            Expr::MethodCall { recv, args, .. } => {
                self.walk_variable_uses(recv, context);
                for arg in args {
                    self.walk_variable_uses(arg, context);
                }
            }
            // The dispatcher routes only call-like expressions here; anything
            // else has no bindings to walk.
            _ => {}
        }
    }

    fn walk_variable_uses_unary_simple(&mut self, expr: &Expr, context: VariableUseContext<'_>) {
        match expr {
            Expr::FieldAccess { expr: inner, .. }
            | Expr::TupleIndex { expr: inner, .. }
            | Expr::Group(inner)
            | Expr::AtomDiff { expr: inner }
            | Expr::AtomDelay { expr: inner, .. }
            | Expr::Unary { expr: inner, .. }
            | Expr::Closure { body: inner, .. }
            | Expr::Return { value: inner } => self.walk_variable_uses(inner, context),
            // The dispatcher routes only unary-simple expressions here.
            _ => {}
        }
    }

    fn walk_variable_uses_composite(&mut self, expr: &Expr, context: VariableUseContext<'_>) {
        match expr {
            Expr::BitSlice {
                expr: inner,
                hi,
                lo,
            } => {
                self.walk_variable_uses(inner, context);
                self.walk_variable_uses(hi, context);
                self.walk_variable_uses(lo, context);
            }
            Expr::Struct { fields, .. } => {
                for (_, value) in fields {
                    self.walk_variable_uses(value, context);
                }
            }
            Expr::Tuple(items) | Expr::VecLit(items) => {
                for item in items {
                    self.walk_variable_uses(item, context);
                }
            }
            Expr::MapLit(entries) => self.walk_map_entries_uses(entries, context),
            // The dispatcher routes only composite expressions here.
            _ => {}
        }
    }

    fn walk_variable_uses_conditional(&mut self, expr: &Expr, context: VariableUseContext<'_>) {
        match expr {
            Expr::IfElse {
                condition,
                then_branch,
                else_branch,
            } => {
                self.walk_variable_uses(condition, context);
                self.walk_variable_uses(then_branch, context);
                self.walk_variable_uses(else_branch, context);
            }
            Expr::Binary { lhs, rhs, .. } => {
                self.walk_variable_uses(lhs, context);
                self.walk_variable_uses(rhs, context);
            }
            // The dispatcher routes only conditional expressions here.
            _ => {}
        }
    }

    fn walk_variable_uses_loop_and_match(&mut self, expr: &Expr, context: VariableUseContext<'_>) {
        match expr {
            Expr::ForLoop {
                iterable,
                guard,
                body,
                ..
            } => self.walk_for_loop_uses(
                ForLoopExprs {
                    iterable,
                    guard: guard.as_deref(),
                    body,
                },
                context,
            ),
            Expr::Match { scrutinee, arms } => {
                self.walk_variable_uses(scrutinee, context);
                for arm in arms {
                    self.walk_variable_uses(&arm.body, context);
                }
            }
            // The dispatcher routes only loop and match expressions here.
            _ => {}
        }
    }

    fn walk_for_loop_uses(&mut self, exprs: ForLoopExprs<'_>, context: VariableUseContext<'_>) {
        self.walk_variable_uses(exprs.iterable, context);
        if let Some(guard) = exprs.guard {
            self.walk_variable_uses(guard, context);
        }
        self.walk_variable_uses(exprs.body, context);
    }

    fn walk_map_entries_uses(&mut self, entries: &[(Expr, Expr)], context: VariableUseContext<'_>) {
        for (key, value) in entries {
            self.walk_variable_uses(key, context);
            self.walk_variable_uses(value, context);
        }
    }

    fn record_variable_use(&mut self, name: &str, context: VariableUseContext<'_>) {
        let resolution = self.resolve_name(context, UseKind::Variable, name);
        self.uses.push(UseSite {
            name: name.to_owned(),
            kind: UseKind::Variable,
            origin: UseOrigin::Variable,
            scope: context.current_scope(),
            span: context.span().clone(),
            source_order: context.literal_index(),
            resolution,
        });
    }
}

#[cfg(test)]
mod tests {
    //! Dispatcher tests for the unary-simple variable-use helper.

    use super::{Expr, ScopeId, SemanticModelBuilder, Span, VariableUseContext};
    use crate::parse;

    #[test]
    fn unary_simple_helper_ignores_a_bare_variable() {
        // `walk_variable_uses` records a bare variable itself and reserves
        // this helper for wrapper expressions. The fallback must stay inert
        // if a variable reaches it anyway, so a use is never recorded twice.
        let parsed = parse("Output(x) :- Source(x).");
        let mut builder = SemanticModelBuilder::new(parsed.root().syntax());
        let span: Span = 0..8;
        let context = VariableUseContext::new(ScopeId(0), 0, &span, 0);

        builder.walk_variable_uses_unary_simple(&Expr::Variable("x".to_owned()), context);

        assert!(
            builder.uses.is_empty(),
            "the unary helper must not record a bare variable use",
        );
    }
}
