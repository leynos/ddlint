//! Internal semantic-model builder state and high-level collection passes.

use rowan::SyntaxNode;

use crate::Span;
use crate::SyntaxKind;
use crate::parser::ast;
use crate::parser::ast::SemanticRule;
use crate::sema::model::{
    DeclarationKind, Scope, ScopeId, ScopeKind, ScopeOrigin, SemanticModel, Symbol, SymbolId,
    SymbolOrigin,
};

use super::resolve::text_range_to_span;

pub(crate) struct SymbolSpec {
    pub(crate) name: String,
    pub(crate) kind: DeclarationKind,
    pub(crate) origin: SymbolOrigin,
    pub(crate) scope: ScopeId,
    pub(crate) span: Span,
    pub(crate) source_order: usize,
    pub(crate) visible_from_rule_order: usize,
}

pub(crate) struct SemanticModelBuilder {
    pub(crate) scopes: Vec<Scope>,
    pub(crate) symbols: Vec<Symbol>,
    pub(crate) uses: Vec<crate::sema::UseSite>,
    pub(crate) program_scope: ScopeId,
}

impl SemanticModelBuilder {
    pub(crate) fn new(root: &SyntaxNode<crate::DdlogLanguage>) -> Self {
        let program_scope = ScopeId(0);
        Self {
            scopes: vec![Scope {
                kind: ScopeKind::Program,
                parent: None,
                origin: ScopeOrigin::Program,
                span: text_range_to_span(root.text_range()),
            }],
            symbols: Vec::new(),
            uses: Vec::new(),
            program_scope,
        }
    }

    pub(crate) fn finish(self) -> SemanticModel {
        SemanticModel {
            program_scope: self.program_scope,
            scopes: self.scopes,
            symbols: self.symbols,
            uses: self.uses,
        }
    }

    pub(crate) fn collect_program_declarations(&mut self, root: &ast::Root) {
        let mut order = 0usize;

        for child in root.syntax().children() {
            match child.kind() {
                SyntaxKind::N_RELATION_DECL => {
                    let relation = ast::Relation {
                        syntax: child.clone(),
                    };
                    if let Some(name) = relation.name() {
                        self.declare_symbol(SymbolSpec {
                            name,
                            kind: DeclarationKind::Relation,
                            origin: SymbolOrigin::RelationDeclaration,
                            scope: self.program_scope,
                            span: text_range_to_span(child.text_range()),
                            source_order: order,
                            visible_from_rule_order: 0,
                        });
                        order += 1;
                    }
                }
                SyntaxKind::N_FUNCTION => {
                    let function = ast::Function {
                        syntax: child.clone(),
                    };
                    if let Some(name) = function.name() {
                        self.declare_symbol(SymbolSpec {
                            name,
                            kind: DeclarationKind::Function,
                            origin: SymbolOrigin::FunctionDeclaration,
                            scope: self.program_scope,
                            span: text_range_to_span(child.text_range()),
                            source_order: order,
                            visible_from_rule_order: 0,
                        });
                        order += 1;
                    }
                }
                SyntaxKind::N_TYPE_DEF => {
                    let type_def = ast::TypeDef {
                        syntax: child.clone(),
                    };
                    if let Some(name) = type_def.name() {
                        self.declare_symbol(SymbolSpec {
                            name,
                            kind: DeclarationKind::Type,
                            origin: SymbolOrigin::TypeDeclaration,
                            scope: self.program_scope,
                            span: text_range_to_span(child.text_range()),
                            source_order: order,
                            visible_from_rule_order: 0,
                        });
                        order += 1;
                    }
                }
                _ => {}
            }
        }
    }

    pub(crate) fn collect_ast_rules(&mut self, root: &ast::Root) {
        for (rule_index, rule) in root.rules().into_iter().enumerate() {
            let rule_span = text_range_to_span(rule.syntax.text_range());
            let rule_scope = self.new_scope(
                ScopeKind::Rule,
                Some(self.program_scope),
                ScopeOrigin::AstRule { index: rule_index },
                rule_span.clone(),
            );

            self.collect_rule_heads(rule_scope, &rule, &rule_span, SymbolOrigin::RuleHead);

            if let Ok(terms) = rule.body_terms() {
                let spans: Vec<Span> = rule
                    .body_expression_nodes()
                    .into_iter()
                    .map(|node| node.span())
                    .collect();

                for (literal_index, term) in terms.into_iter().enumerate() {
                    let span = spans
                        .get(literal_index)
                        .cloned()
                        .unwrap_or_else(|| rule_span.clone());
                    self.collect_rule_term(rule_scope, literal_index, &term, &span, literal_index);
                }
            }
        }
    }

    pub(crate) fn collect_semantic_rules(&mut self, semantic_rules: &[SemanticRule]) {
        for (rule_index, rule) in semantic_rules.iter().enumerate() {
            let rule_span = rule.source_span();
            let rule_scope = self.new_scope(
                ScopeKind::Rule,
                Some(self.program_scope),
                ScopeOrigin::SemanticRule {
                    index: rule_index,
                    origin: rule.origin(),
                },
                rule_span.clone(),
            );

            self.collect_head_expr(
                rule_scope,
                rule.head(),
                &rule_span,
                SymbolOrigin::SemanticRuleHead,
            );

            for (literal_index, expr) in rule.body().iter().enumerate() {
                self.collect_expression_term(
                    rule_scope,
                    literal_index,
                    expr,
                    &rule_span,
                    literal_index,
                );
            }
        }
    }

    pub(crate) fn new_scope(
        &mut self,
        kind: ScopeKind,
        parent: Option<ScopeId>,
        origin: ScopeOrigin,
        span: Span,
    ) -> ScopeId {
        let scope_id = ScopeId(self.scopes.len());
        self.scopes.push(Scope {
            kind,
            parent,
            origin,
            span,
        });
        scope_id
    }

    pub(crate) fn declare_symbol(&mut self, spec: SymbolSpec) -> SymbolId {
        let symbol_id = SymbolId(self.symbols.len());
        self.symbols.push(Symbol {
            name: spec.name,
            kind: spec.kind,
            origin: spec.origin,
            scope: spec.scope,
            span: spec.span,
            source_order: spec.source_order,
            visible_from_rule_order: spec.visible_from_rule_order,
        });
        symbol_id
    }
}
