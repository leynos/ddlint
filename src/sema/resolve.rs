//! Resolution helpers for semantic symbol and scope analysis.

use crate::parser::ast::{Expr, Pattern};
use crate::sema::model::{DeclarationKind, Resolution, ScopeId, ScopeKind, Symbol, UseKind};

use super::builder::SemanticModelBuilder;

impl SemanticModelBuilder {
    pub(crate) fn resolve_name(
        &self,
        context: super::variables::VariableUseContext<'_>,
        use_kind: UseKind,
        name: &str,
    ) -> Resolution {
        if name == "_" {
            return Resolution::Ignored;
        }

        let mut current_scope = Some(context.current_scope());

        while let Some(scope_id) = current_scope {
            if let Some(symbol_indices) = self
                .symbols_by_scope_and_name
                .get(&scope_id)
                .and_then(|symbols_by_name| symbols_by_name.get(name))
            {
                for symbol_index in symbol_indices.iter().rev().copied() {
                    let Some(symbol) = self.symbols.get(symbol_index) else {
                        continue;
                    };
                    if !is_compatible(use_kind, symbol.kind()) {
                        continue;
                    }
                    if !self.symbol_visible(scope_id, symbol, context.rule_order_limit()) {
                        continue;
                    }
                    return Resolution::Resolved(crate::sema::SymbolId(symbol_index));
                }
            }
            current_scope = self.parent_scope(scope_id);
        }

        Resolution::Unresolved
    }

    fn symbol_visible(&self, scope_id: ScopeId, symbol: &Symbol, rule_order_limit: usize) -> bool {
        match self.scope_kind(scope_id) {
            ScopeKind::Program | ScopeKind::ForLoop => true,
            ScopeKind::Rule => symbol.visible_from_rule_order() <= rule_order_limit,
        }
    }

    fn parent_scope(&self, scope_id: ScopeId) -> Option<ScopeId> {
        self.scopes
            .get(scope_id.0)
            .and_then(crate::sema::Scope::parent)
    }

    fn scope_kind(&self, scope_id: ScopeId) -> ScopeKind {
        self.scopes
            .get(scope_id.0)
            .map_or(ScopeKind::Program, crate::sema::Scope::kind)
    }
}

pub(super) fn relation_name(expr: &Expr) -> Option<&str> {
    match expr {
        Expr::Apply { callee, .. } | Expr::Call { callee, .. } => match callee.as_ref() {
            Expr::Variable(name) => Some(name.as_str()),
            _ => None,
        },
        Expr::AtomDiff { expr } | Expr::AtomDelay { expr, .. } | Expr::Group(expr) => {
            relation_name(expr)
        }
        _ => None,
    }
}

pub(super) fn collect_head_binding_names(expr: &Expr) -> Vec<String> {
    let mut names = Vec::new();
    collect_head_bindings(expr, &mut names);
    names
}

pub(super) fn collect_pattern_binding_names(pattern: &Pattern) -> Vec<String> {
    let mut names = Vec::new();
    collect_pattern_bindings(pattern, &mut names);
    names
}

fn is_compatible(use_kind: UseKind, declaration_kind: DeclarationKind) -> bool {
    matches!(
        (use_kind, declaration_kind),
        (UseKind::Relation, DeclarationKind::Relation)
            | (UseKind::Variable, DeclarationKind::RuleBinding)
    )
}

fn collect_head_bindings(expr: &Expr, names: &mut Vec<String>) {
    match expr {
        Expr::Variable(name) => push_binding_name(names, name),
        Expr::Apply { .. } | Expr::Call { .. } | Expr::MethodCall { .. } => {
            collect_call_like_bindings(expr, names);
        }
        Expr::FieldAccess { .. }
        | Expr::TupleIndex { .. }
        | Expr::Group(_)
        | Expr::AtomDiff { .. }
        | Expr::AtomDelay { .. }
        | Expr::Unary { .. }
        | Expr::Return { .. }
        | Expr::BitSlice { .. }
        | Expr::Tuple(_)
        | Expr::VecLit(_)
        | Expr::Struct { .. }
        | Expr::MapLit(_) => collect_container_like_bindings(expr, names),
        Expr::IfElse { .. } | Expr::Binary { .. } => {
            collect_control_flow_like_bindings(expr, names);
        }
        Expr::Literal(_)
        | Expr::Closure { .. }
        | Expr::ForLoop { .. }
        | Expr::Match { .. }
        | Expr::Break
        | Expr::Continue => {}
    }
}

fn collect_call_like_bindings(expr: &Expr, names: &mut Vec<String>) {
    match expr {
        Expr::Apply { args, .. } | Expr::Call { args, .. } => {
            collect_head_bindings_many(args, names);
        }
        Expr::MethodCall { recv, args, .. } => {
            collect_head_bindings(recv, names);
            collect_head_bindings_many(args, names);
        }
        _ => unreachable!("call-like helper must receive a call-like expression"),
    }
}

fn collect_container_like_bindings(expr: &Expr, names: &mut Vec<String>) {
    match expr {
        Expr::FieldAccess { expr, .. }
        | Expr::TupleIndex { expr, .. }
        | Expr::Group(expr)
        | Expr::AtomDiff { expr }
        | Expr::AtomDelay { expr, .. }
        | Expr::Unary { expr, .. }
        | Expr::Return { value: expr } => collect_head_bindings(expr, names),
        Expr::BitSlice { expr, hi, lo } => {
            collect_head_bindings_many([expr.as_ref(), hi.as_ref(), lo.as_ref()], names);
        }
        Expr::Struct { fields, .. } => {
            collect_head_bindings_many(fields.iter().map(|(_, value)| value), names);
        }
        Expr::Tuple(items) | Expr::VecLit(items) => collect_head_bindings_many(items, names),
        Expr::MapLit(entries) => {
            for (key, value) in entries {
                collect_head_bindings_many([key, value], names);
            }
        }
        _ => unreachable!("container-like helper must receive a container-like expression"),
    }
}

fn collect_control_flow_like_bindings(expr: &Expr, names: &mut Vec<String>) {
    match expr {
        Expr::IfElse {
            condition,
            then_branch,
            else_branch,
        } => collect_head_bindings_many(
            [
                condition.as_ref(),
                then_branch.as_ref(),
                else_branch.as_ref(),
            ],
            names,
        ),
        Expr::Binary { lhs, rhs, .. } => {
            collect_head_bindings_many([lhs.as_ref(), rhs.as_ref()], names);
        }
        _ => unreachable!("control-flow helper must receive a control-flow expression"),
    }
}

fn collect_head_bindings_many<'a>(
    exprs: impl IntoIterator<Item = &'a Expr>,
    names: &mut Vec<String>,
) {
    for expr in exprs {
        collect_head_bindings(expr, names);
    }
}

fn collect_pattern_bindings(pattern: &Pattern, names: &mut Vec<String>) {
    match pattern {
        Pattern::Wildcard | Pattern::Literal(_) => {}
        Pattern::Var { name, .. } => push_binding_name(names, name),
        Pattern::Tuple(items) => {
            for item in items {
                collect_pattern_bindings(item, names);
            }
        }
        Pattern::Struct { fields, .. } => {
            for (_, field_pattern) in fields {
                collect_pattern_bindings(field_pattern, names);
            }
        }
        Pattern::Typed { pattern, .. } => collect_pattern_bindings(pattern, names),
    }
}

fn push_binding_name(names: &mut Vec<String>, name: &str) {
    if name == "_" || names.iter().any(|existing| existing == name) {
        return;
    }
    names.push(name.to_string());
}
