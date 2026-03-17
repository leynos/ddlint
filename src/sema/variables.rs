//! Variable-use traversal for semantic-model collection.

use crate::Span;
use crate::parser::ast::Expr;
use crate::sema::model::{Resolution, ScopeId, UseKind, UseSite};

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
            Expr::Apply { args, .. } | Expr::Call { args, .. } => {
                self.walk_variable_use_list(args.iter().collect(), context);
            }
            Expr::MethodCall { recv, args, .. } => {
                self.walk_leading_and_rest(recv, args.iter(), context);
            }
            Expr::FieldAccess { expr, .. }
            | Expr::TupleIndex { expr, .. }
            | Expr::Group(expr)
            | Expr::AtomDiff { expr }
            | Expr::AtomDelay { expr, .. }
            | Expr::Unary { expr, .. }
            | Expr::Closure { body: expr, .. }
            | Expr::Return { value: expr } => {
                self.walk_one_variable_use(expr, context);
            }
            Expr::BitSlice { expr, hi, lo } => {
                self.walk_variable_use_list(vec![expr.as_ref(), hi.as_ref(), lo.as_ref()], context);
            }
            Expr::Struct { fields, .. } => {
                self.walk_variable_use_list(
                    fields.iter().map(|(_, value)| value).collect(),
                    context,
                );
            }
            Expr::Tuple(items) | Expr::VecLit(items) => {
                self.walk_variable_use_list(items.iter().collect(), context);
            }
            Expr::IfElse {
                condition,
                then_branch,
                else_branch,
            } => self.walk_variable_use_list(
                vec![
                    condition.as_ref(),
                    then_branch.as_ref(),
                    else_branch.as_ref(),
                ],
                context,
            ),
            Expr::Binary { lhs, rhs, .. } => {
                self.walk_variable_use_list(vec![lhs.as_ref(), rhs.as_ref()], context);
            }
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
                self.walk_leading_and_rest(scrutinee, arms.iter().map(|arm| &arm.body), context);
            }
            Expr::MapLit(entries) => self.walk_map_entries_uses(entries, context),
        }
    }

    fn walk_one_variable_use(&mut self, expr: &Expr, context: VariableUseContext<'_>) {
        self.walk_variable_uses(expr, context);
    }

    fn walk_variable_use_list(&mut self, expressions: Vec<&Expr>, context: VariableUseContext<'_>) {
        for expr in expressions {
            self.walk_variable_uses(expr, context);
        }
    }

    fn walk_leading_and_rest<'e>(
        &mut self,
        leading: &'e Expr,
        rest: impl IntoIterator<Item = &'e Expr>,
        context: VariableUseContext<'_>,
    ) {
        let mut expressions = vec![leading];
        expressions.extend(rest);
        self.walk_variable_use_list(expressions, context);
    }

    fn walk_for_loop_uses(&mut self, exprs: ForLoopExprs<'_>, context: VariableUseContext<'_>) {
        self.walk_one_variable_use(exprs.iterable, context);
        if let Some(guard) = exprs.guard {
            self.walk_one_variable_use(guard, context);
        }
        self.walk_one_variable_use(exprs.body, context);
    }

    fn walk_map_entries_uses(&mut self, entries: &[(Expr, Expr)], context: VariableUseContext<'_>) {
        let expressions = entries
            .iter()
            .flat_map(|(key, value)| [key, value])
            .collect();
        self.walk_variable_use_list(expressions, context);
    }

    fn record_variable_use(&mut self, name: &str, context: VariableUseContext<'_>) {
        let resolution = if name == "_" {
            Resolution::Ignored
        } else {
            self.resolve_name(context, UseKind::Variable, name)
        };
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
