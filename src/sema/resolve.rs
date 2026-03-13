//! Resolution helpers for semantic symbol and scope analysis.

use crate::Span;
use crate::parser::ast::{Expr, Pattern};
use crate::sema::model::{DeclarationKind, Resolution, ScopeId, ScopeKind, Symbol, UseKind};

use super::builder::SemanticModelBuilder;

impl SemanticModelBuilder {
    pub(crate) fn resolve_name(
        &self,
        start_scope: ScopeId,
        use_kind: UseKind,
        name: &str,
        rule_order_limit: usize,
    ) -> Resolution {
        let mut current_scope = Some(start_scope);

        while let Some(scope_id) = current_scope {
            for (symbol_index, symbol) in self.symbols.iter().enumerate().rev() {
                if symbol.scope() != scope_id || symbol.name() != name {
                    continue;
                }
                if !is_compatible(use_kind, symbol.kind()) {
                    continue;
                }
                if !self.symbol_visible(scope_id, symbol, rule_order_limit) {
                    continue;
                }
                return Resolution::Resolved(crate::sema::SymbolId(symbol_index));
            }
            current_scope = self.parent_scope(scope_id);
        }

        if name == "_" {
            Resolution::Ignored
        } else {
            Resolution::Unresolved
        }
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

pub(super) fn text_range_to_span(range: rowan::TextRange) -> Span {
    let start: usize = range.start().into();
    let end: usize = range.end().into();
    start..end
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
        Expr::Apply { args, .. } | Expr::Call { args, .. } => {
            for arg in args {
                collect_head_bindings(arg, names);
            }
        }
        Expr::MethodCall { recv, args, .. } => {
            collect_head_bindings(recv, names);
            for arg in args {
                collect_head_bindings(arg, names);
            }
        }
        Expr::FieldAccess { expr, .. }
        | Expr::TupleIndex { expr, .. }
        | Expr::Group(expr)
        | Expr::AtomDiff { expr }
        | Expr::AtomDelay { expr, .. }
        | Expr::Unary { expr, .. } => collect_head_bindings(expr, names),
        Expr::BitSlice { expr, hi, lo } => {
            for nested in [expr.as_ref(), hi.as_ref(), lo.as_ref()] {
                collect_head_bindings(nested, names);
            }
        }
        Expr::Struct { fields, .. } => {
            for (_, value) in fields {
                collect_head_bindings(value, names);
            }
        }
        Expr::Tuple(items) | Expr::VecLit(items) => {
            for item in items {
                collect_head_bindings(item, names);
            }
        }
        Expr::IfElse {
            condition,
            then_branch,
            else_branch,
        } => {
            for nested in [
                condition.as_ref(),
                then_branch.as_ref(),
                else_branch.as_ref(),
            ] {
                collect_head_bindings(nested, names);
            }
        }
        Expr::Binary { lhs, rhs, .. } => {
            for nested in [lhs.as_ref(), rhs.as_ref()] {
                collect_head_bindings(nested, names);
            }
        }
        Expr::Return { value } => collect_head_bindings(value, names),
        Expr::MapLit(entries) => {
            for (key, value) in entries {
                collect_head_bindings(key, names);
                collect_head_bindings(value, names);
            }
        }
        Expr::Literal(_)
        | Expr::Closure { .. }
        | Expr::ForLoop { .. }
        | Expr::Match { .. }
        | Expr::Break
        | Expr::Continue => {}
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
