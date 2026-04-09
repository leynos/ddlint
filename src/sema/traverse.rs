//! Expression and rule-body traversal for semantic-model collection.

use rowan::SyntaxNode;

use crate::Span;
use crate::parser::ast;
use crate::parser::ast::{Expr, RuleBodyTerm, find_identifier_span};
use crate::sema::model::{
    DeclarationKind, ScopeId, ScopeKind, ScopeOrigin, SymbolOrigin, UseKind, UseOrigin, UseSite,
};

use super::builder::{ScopeSpec, SemanticModelBuilder, SymbolSpec};
use super::resolve::{collect_pattern_binding_names, relation_name};
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
                for (binding_name, name_span) in &head.binding_spans {
                    self.declare_symbol(SymbolSpec {
                        name: binding_name.clone(),
                        kind: DeclarationKind::RuleBinding,
                        origin: ctx.origin,
                        scope: ctx.scope,
                        span: ctx.span.clone(),
                        name_span: Some(name_span.clone()),
                        source_order: self.symbols.len(),
                        visible_from_rule_order: 0,
                    });
                }
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
            UseOrigin::RelationHead,
            expr,
        );
    }

    pub(crate) fn collect_rule_term(
        &mut self,
        context: VariableUseContext<'_>,
        syntax: Option<&SyntaxNode<crate::DdlogLanguage>>,
        term: &RuleBodyTerm,
    ) {
        match term {
            RuleBodyTerm::Expression(expr) => self.collect_expression_term(expr, context),
            RuleBodyTerm::Assignment(assign) => {
                self.collect_assignment_term(assign, syntax, context);
            }
            RuleBodyTerm::Aggregation(aggregation) => {
                self.collect_aggregation_term(aggregation, context);
            }
            RuleBodyTerm::ForLoop(for_loop) => {
                self.collect_for_loop_term(for_loop, syntax, context);
            }
        }
    }

    pub(crate) fn collect_expression_term(&mut self, expr: &Expr, context: VariableUseContext<'_>) {
        self.record_top_level_relation_use(context, UseOrigin::RelationBody, expr);
        self.walk_variable_uses(expr, context);
    }

    fn collect_assignment_term(
        &mut self,
        assign: &ast::RuleAssignment,
        syntax: Option<&SyntaxNode<crate::DdlogLanguage>>,
        context: VariableUseContext<'_>,
    ) {
        self.walk_variable_uses(&assign.value, context);
        for binding_name in collect_pattern_binding_names(&assign.pattern) {
            self.declare_pattern_binding(
                &binding_name,
                SymbolOrigin::AssignmentPattern,
                context.current_scope(),
                context.span(),
                syntax,
                context.literal_index() + 1,
            );
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
        syntax: Option<&SyntaxNode<crate::DdlogLanguage>>,
        context: VariableUseContext<'_>,
    ) {
        self.record_top_level_relation_use(context, UseOrigin::ForIterable, &for_loop.iterable);
        self.walk_variable_uses(&for_loop.iterable, context);
        if let Some(guard) = for_loop.guard.as_ref() {
            self.record_top_level_relation_use(context, UseOrigin::ForGuard, guard);
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
            self.declare_pattern_binding(
                &binding_name,
                SymbolOrigin::ForPattern,
                child_scope,
                context.span(),
                syntax,
                0,
            );
        }

        for (term_offset, nested_term) in for_loop.body_terms.iter().enumerate() {
            // Nested bodies recurse through `collect_rule_term`, but the
            // `VariableUseContext::new(...)` path no longer retains CST handles
            // for those inner terms. We therefore pass `None` here
            // deliberately so nested bindings do not claim precise
            // `name_span`s we cannot source faithfully.
            self.collect_rule_term(
                VariableUseContext::new(
                    child_scope,
                    context.literal_index() + term_offset,
                    context.span(),
                    context.rule_order_limit(),
                ),
                None,
                nested_term,
            );
        }
    }

    fn record_top_level_relation_use(
        &mut self,
        context: VariableUseContext<'_>,
        origin: UseOrigin,
        expr: &Expr,
    ) {
        let use_kind = UseKind::Relation;
        let Some(name) = relation_name(expr) else {
            return;
        };
        let resolution = self.resolve_name(context, use_kind, name);
        self.uses.push(UseSite {
            name: name.to_string(),
            kind: use_kind,
            origin,
            scope: context.current_scope(),
            span: context.span().clone(),
            source_order: context.literal_index(),
            resolution,
        });
    }

    fn declare_pattern_binding(
        &mut self,
        binding_name: &str,
        origin: SymbolOrigin,
        scope: ScopeId,
        span: &Span,
        syntax: Option<&SyntaxNode<crate::DdlogLanguage>>,
        visible_from_rule_order: usize,
    ) {
        self.declare_symbol(SymbolSpec {
            name: binding_name.to_string(),
            kind: DeclarationKind::RuleBinding,
            origin,
            scope,
            span: span.clone(),
            name_span: syntax.and_then(|syntax| find_identifier_span(syntax, binding_name)),
            source_order: self.symbols.len(),
            visible_from_rule_order,
        });
    }
}
