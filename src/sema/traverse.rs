//! Expression and rule-body traversal for semantic-model collection.

use crate::Span;
use crate::parser::ast;
use crate::parser::ast::{Expr, RuleBodyTerm};
use crate::sema::model::{
    DeclarationKind, ScopeId, ScopeKind, ScopeOrigin, SymbolOrigin, UseKind, UseSite,
};

use super::builder::{SemanticModelBuilder, SymbolSpec};
use super::resolve::{collect_head_binding_names, collect_pattern_binding_names, relation_name};
use super::variables::VariableUseContext;

impl SemanticModelBuilder {
    pub(crate) fn collect_rule_heads(
        &mut self,
        rule_scope: ScopeId,
        rule: &ast::Rule,
        rule_span: &Span,
        origin: SymbolOrigin,
    ) {
        if let Ok(heads) = rule.heads() {
            for head in heads {
                self.collect_head_expr(rule_scope, &head.atom, rule_span, origin);
                if let Some(location) = head.location.as_ref() {
                    self.walk_variable_uses(
                        location,
                        VariableUseContext::new(rule_scope, 0, rule_span, 0),
                    );
                }
            }
        }
    }

    pub(crate) fn collect_head_expr(
        &mut self,
        rule_scope: ScopeId,
        expr: &Expr,
        span: &Span,
        origin: SymbolOrigin,
    ) {
        self.record_top_level_relation_use(rule_scope, 0, expr, span, 0);
        for binding_name in collect_head_binding_names(expr) {
            self.declare_symbol(SymbolSpec {
                name: binding_name,
                kind: DeclarationKind::RuleBinding,
                origin,
                scope: rule_scope,
                span: span.clone(),
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
        match term {
            RuleBodyTerm::Expression(expr) => self.collect_expression_term(
                current_scope,
                literal_index,
                expr,
                span,
                rule_order_limit,
            ),
            RuleBodyTerm::Assignment(assign) => self.collect_assignment_term(
                current_scope,
                literal_index,
                assign,
                span,
                rule_order_limit,
            ),
            RuleBodyTerm::Aggregation(aggregation) => self.collect_aggregation_term(
                current_scope,
                literal_index,
                aggregation,
                span,
                rule_order_limit,
            ),
            RuleBodyTerm::ForLoop(for_loop) => self.collect_for_loop_term(
                current_scope,
                literal_index,
                for_loop,
                span,
                rule_order_limit,
            ),
        }
    }

    pub(crate) fn collect_expression_term(
        &mut self,
        current_scope: ScopeId,
        literal_index: usize,
        expr: &Expr,
        span: &Span,
        rule_order_limit: usize,
    ) {
        self.record_top_level_relation_use(
            current_scope,
            literal_index,
            expr,
            span,
            rule_order_limit,
        );
        self.walk_variable_uses(
            expr,
            VariableUseContext::new(current_scope, literal_index, span, rule_order_limit),
        );
    }

    fn collect_assignment_term(
        &mut self,
        current_scope: ScopeId,
        literal_index: usize,
        assign: &ast::RuleAssignment,
        span: &Span,
        rule_order_limit: usize,
    ) {
        self.walk_variable_uses(
            &assign.value,
            VariableUseContext::new(current_scope, literal_index, span, rule_order_limit),
        );
        for binding_name in collect_pattern_binding_names(&assign.pattern) {
            self.declare_symbol(SymbolSpec {
                name: binding_name,
                kind: DeclarationKind::RuleBinding,
                origin: SymbolOrigin::AssignmentPattern,
                scope: current_scope,
                span: span.clone(),
                source_order: self.symbols.len(),
                visible_from_rule_order: literal_index + 1,
            });
        }
    }

    fn collect_aggregation_term(
        &mut self,
        current_scope: ScopeId,
        literal_index: usize,
        aggregation: &ast::RuleAggregation,
        span: &Span,
        rule_order_limit: usize,
    ) {
        self.walk_variable_uses(
            &aggregation.project,
            VariableUseContext::new(current_scope, literal_index, span, rule_order_limit),
        );
        self.walk_variable_uses(
            &aggregation.key,
            VariableUseContext::new(current_scope, literal_index, span, rule_order_limit),
        );
    }

    fn collect_for_loop_term(
        &mut self,
        current_scope: ScopeId,
        literal_index: usize,
        for_loop: &ast::RuleForLoop,
        span: &Span,
        rule_order_limit: usize,
    ) {
        self.record_top_level_relation_use(
            current_scope,
            literal_index,
            &for_loop.iterable,
            span,
            rule_order_limit,
        );
        self.walk_variable_uses(
            &for_loop.iterable,
            VariableUseContext::new(current_scope, literal_index, span, rule_order_limit),
        );
        if let Some(guard) = for_loop.guard.as_ref() {
            self.record_top_level_relation_use(
                current_scope,
                literal_index,
                guard,
                span,
                rule_order_limit,
            );
            self.walk_variable_uses(
                guard,
                VariableUseContext::new(current_scope, literal_index, span, rule_order_limit),
            );
        }

        let child_scope = self.new_scope(
            ScopeKind::ForLoop,
            Some(current_scope),
            ScopeOrigin::ForLoop { literal_index },
            span.clone(),
        );
        for binding_name in collect_pattern_binding_names(&for_loop.pattern) {
            self.declare_symbol(SymbolSpec {
                name: binding_name,
                kind: DeclarationKind::RuleBinding,
                origin: SymbolOrigin::ForPattern,
                scope: child_scope,
                span: span.clone(),
                source_order: self.symbols.len(),
                visible_from_rule_order: 0,
            });
        }

        for nested_term in &for_loop.body_terms {
            self.collect_rule_term(
                child_scope,
                literal_index,
                nested_term,
                span,
                rule_order_limit,
            );
        }
    }

    fn record_top_level_relation_use(
        &mut self,
        current_scope: ScopeId,
        literal_index: usize,
        expr: &Expr,
        span: &Span,
        rule_order_limit: usize,
    ) {
        let Some(name) = relation_name(expr) else {
            return;
        };
        let resolution =
            self.resolve_name(current_scope, UseKind::Relation, name, rule_order_limit);
        self.uses.push(UseSite {
            name: name.to_string(),
            kind: UseKind::Relation,
            scope: current_scope,
            span: span.clone(),
            source_order: literal_index,
            resolution,
        });
    }
}
