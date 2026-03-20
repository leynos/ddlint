//! Variable-use traversal for semantic-model collection.

use crate::Span;
use crate::parser::ast::Expr;
use crate::sema::model::{ScopeId, UseKind, UseSite};

use super::builder::SemanticModelBuilder;

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
    pub(super) fn new(
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

    pub(super) fn current_scope(self) -> ScopeId {
        self.current_scope
    }

    pub(super) fn literal_index(self) -> usize {
        self.literal_index
    }

    pub(super) fn span(self) -> &'a Span {
        self.span
    }

    pub(super) fn rule_order_limit(self) -> usize {
        self.rule_order_limit
    }
}

impl SemanticModelBuilder {
    pub(crate) fn walk_variable_uses(&mut self, expr: &Expr, context: VariableUseContext<'_>) {
        match expr {
            Expr::Variable(name) => self.record_variable_use(name, context),
            Expr::Literal(_) | Expr::Break | Expr::Continue => {}
            Expr::Apply { .. } | Expr::Call { .. } | Expr::MethodCall { .. } => {
                self.walk_variable_uses_call_like(expr, context);
            }
            Expr::FieldAccess { expr, .. }
            | Expr::TupleIndex { expr, .. }
            | Expr::Group(expr)
            | Expr::AtomDiff { expr }
            | Expr::AtomDelay { expr, .. }
            | Expr::Unary { expr, .. }
            | Expr::Closure { body: expr, .. }
            | Expr::Return { value: expr } => {
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
            _ => unreachable!("call-like helper must receive a call-like expression"),
        }
    }

    fn walk_variable_uses_unary_simple(&mut self, expr: &Expr, context: VariableUseContext<'_>) {
        match expr {
            Expr::FieldAccess { expr, .. }
            | Expr::TupleIndex { expr, .. }
            | Expr::Group(expr)
            | Expr::AtomDiff { expr }
            | Expr::AtomDelay { expr, .. }
            | Expr::Unary { expr, .. }
            | Expr::Closure { body: expr, .. }
            | Expr::Return { value: expr } => self.walk_variable_uses(expr, context),
            _ => unreachable!("unary-simple helper must receive a unary-simple expression"),
        }
    }

    fn walk_variable_uses_composite(&mut self, expr: &Expr, context: VariableUseContext<'_>) {
        match expr {
            Expr::BitSlice { expr, hi, lo } => {
                self.walk_variable_uses(expr, context);
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
            _ => unreachable!("composite helper must receive a composite expression"),
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
            _ => unreachable!("conditional helper must receive a conditional expression"),
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
            _ => unreachable!("loop-and-match helper must receive a loop or match expression"),
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
            name: name.to_string(),
            kind: UseKind::Variable,
            scope: context.current_scope(),
            span: context.span().clone(),
            source_order: context.literal_index(),
            resolution,
        });
    }
}
