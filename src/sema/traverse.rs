//! Expression and rule-body traversal for semantic-model collection.

use crate::Span;
use crate::parser::ast;
use crate::parser::ast::{Expr, RuleBodyTerm};
use crate::sema::model::{
    DeclarationKind, ScopeId, ScopeKind, ScopeOrigin, SymbolOrigin, UseKind, UseSite,
};

use super::builder::{ScopeSpec, SemanticModelBuilder, SymbolSpec};
use super::resolve::{collect_head_binding_names, collect_pattern_binding_names, relation_name};
use super::variables::VariableUseContext;

#[derive(Clone, Copy)]
pub(super) struct RuleHeadContext<'a> {
    pub(super) scope: ScopeId,
    pub(super) span: &'a Span,
    pub(super) origin: SymbolOrigin,
}

impl SemanticModelBuilder {
    pub(crate) fn collect_rule_heads(&mut self, ctx: RuleHeadContext<'_>, rule: &ast::Rule) {
        if let Ok(heads) = rule.heads() {
            for head in heads {
                self.collect_head_expr(&head.atom, ctx);
                if let Some(location) = head.location.as_ref() {
                    self.walk_variable_uses(
                        location,
                        VariableUseContext::new(ctx.scope, 0, ctx.span, 0),
                    );
                }
            }
        }
    }

    pub(crate) fn collect_head_expr(&mut self, expr: &Expr, ctx: RuleHeadContext<'_>) {
        self.record_top_level_relation_use(
            VariableUseContext::new(ctx.scope, 0, ctx.span, 0),
            UseKind::Relation,
            expr,
        );
        for binding_name in collect_head_binding_names(expr) {
            self.declare_symbol(SymbolSpec {
                name: binding_name,
                kind: DeclarationKind::RuleBinding,
                origin: ctx.origin,
                scope: ctx.scope,
                span: ctx.span.clone(),
                source_order: self.symbols.len(),
                visible_from_rule_order: 0,
            });
        }
    }

    pub(crate) fn collect_rule_term(
        &mut self,
        current_scope: ScopeId,
        literal_index: usize,
        term: &RuleBodyTerm,
        span: &Span,
        rule_order_limit: usize,
    ) {
        let context = VariableUseContext::new(current_scope, literal_index, span, rule_order_limit);
        match term {
            RuleBodyTerm::Expression(expr) => self.collect_expression_term(expr, context),
            RuleBodyTerm::Assignment(assign) => self.collect_assignment_term(assign, context),
            RuleBodyTerm::Aggregation(aggregation) => {
                self.collect_aggregation_term(aggregation, context);
            }
            RuleBodyTerm::ForLoop(for_loop) => self.collect_for_loop_term(for_loop, context),
        }
    }

    pub(crate) fn collect_expression_term(&mut self, expr: &Expr, context: VariableUseContext<'_>) {
        self.record_top_level_relation_use(context, UseKind::Relation, expr);
        self.walk_variable_uses(expr, context);
    }

    fn collect_assignment_term(
        &mut self,
        assign: &ast::RuleAssignment,
        context: VariableUseContext<'_>,
    ) {
        self.walk_variable_uses(&assign.value, context);
        for binding_name in collect_pattern_binding_names(&assign.pattern) {
            self.declare_symbol(SymbolSpec {
                name: binding_name,
                kind: DeclarationKind::RuleBinding,
                origin: SymbolOrigin::AssignmentPattern,
                scope: context.current_scope(),
                span: context.span().clone(),
                source_order: self.symbols.len(),
                visible_from_rule_order: context.literal_index() + 1,
            });
        }
    }

    fn collect_aggregation_term(
        &mut self,
        aggregation: &ast::RuleAggregation,
        context: VariableUseContext<'_>,
    ) {
        self.walk_variable_uses(&aggregation.project, context);
        self.walk_variable_uses(&aggregation.key, context);
    }

    fn collect_for_loop_term(
        &mut self,
        for_loop: &ast::RuleForLoop,
        context: VariableUseContext<'_>,
    ) {
        self.record_top_level_relation_use(context, UseKind::Relation, &for_loop.iterable);
        self.walk_variable_uses(&for_loop.iterable, context);
        if let Some(guard) = for_loop.guard.as_ref() {
            self.record_top_level_relation_use(context, UseKind::Relation, guard);
            self.walk_variable_uses(guard, context);
        }

        let child_scope = self.new_scope(ScopeSpec {
            kind: ScopeKind::ForLoop,
            parent: Some(context.current_scope()),
            origin: ScopeOrigin::ForLoop {
                literal_index: context.literal_index(),
            },
            span: context.span().clone(),
        });
        for binding_name in collect_pattern_binding_names(&for_loop.pattern) {
            self.declare_symbol(SymbolSpec {
                name: binding_name,
                kind: DeclarationKind::RuleBinding,
                origin: SymbolOrigin::ForPattern,
                scope: child_scope,
                span: context.span().clone(),
                source_order: self.symbols.len(),
                visible_from_rule_order: 0,
            });
        }

        for nested_term in &for_loop.body_terms {
            self.collect_rule_term(
                child_scope,
                context.literal_index(),
                nested_term,
                context.span(),
                context.rule_order_limit(),
            );
        }
    }

    fn record_top_level_relation_use(
        &mut self,
        context: VariableUseContext<'_>,
        use_kind: UseKind,
        expr: &Expr,
    ) {
        let Some(name) = relation_name(expr) else {
            return;
        };
        let resolution = self.resolve_name(context, use_kind, name);
        self.uses.push(UseSite {
            name: name.to_string(),
            kind: use_kind,
            scope: context.current_scope(),
            span: context.span().clone(),
            source_order: context.literal_index(),
            resolution,
        });
    }
}
